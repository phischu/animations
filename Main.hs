{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Graphics.Gloss

import Control.Monad (guard)

type T = Float

data Stream a = Stream (T -> a) (Event (Stream a))
data Event a = Never
             | Eventually T a

deriving instance Functor Event
deriving instance Functor Stream
deriving instance Functor Player

deriving instance (Show a) => Show (Event a)

data Player a = Player T (Stream a)

advance :: T -> Player a -> Player a
advance dt (Player t (Stream a Never)) = Player (t + dt) (Stream a Never)
advance dt (Player t (Stream a (Eventually et s)))
    | (dt + t) < et = Player (t + dt) (Stream a (Eventually et s))
    | otherwise     = Player (t + dt - et) s

now :: Player a -> a
now (Player t (Stream f _)) = f t

player :: Stream a -> Player a
player s = Player 0 s

run :: Stream Picture -> IO ()
run s = simulate
    (InWindow "Aninmation" (600,600) (100,100))
    white
    40
    (player s)
    now
    (const advance)

data Ball = Ball Radius Position Velocity
type Radius = Float
type Position = Float
type Velocity = Float

deriving instance Show Ball

render :: Ball -> Picture
render (Ball r x _) = translate x 0 (circle r)

earlier :: Event a -> Event a -> Event a
earlier Never e = e
earlier e Never = e
earlier (Eventually t1 a1) (Eventually t2 a2)
    | t1 < t2 = Eventually t1 a1
    | otherwise = Eventually t2 a2

earliest :: [Event a] -> Event a
earliest = foldr earlier Never

data Wall = Wall Float

collideBallWall :: Ball -> Wall -> Event Ball
collideBallWall (Ball r x v) (Wall w)
    | v == 0 = Never
    | otherwise = earliest (do
        t <- [(w + r - x) / v,(w - r - x) / v]
        guard (t >= 0)
        return (Eventually t (Ball r (x + t * v) (negate v))))

roll :: Ball -> T -> Ball
roll (Ball r x v) t = Ball r (x + v * t) v

sim1 :: [Wall] -> Ball -> Stream Ball
sim1 walls ball = Stream (roll ball) (fmap (sim1 walls) (earliest (map (collideBallWall ball) walls)))

collideBallBall :: Ball -> Ball -> Event (Ball,Ball)
collideBallBall (Ball r1 x1 v1) (Ball r2 x2 v2)
    | v2 - v1 == 0 = Never
    | otherwise = earliest (do
        t <- [(x2 - x1 + r1 + r2) / (v1 - v2),(x2 - x1 - r1 - r2) / (v1 - v2)]
        guard (t >= 0)
        return (Eventually t (
            Ball r1 (x1 + v1 * t) v2,Ball r2 (x2 + v2 * t) v1)))

sim2 :: (Ball,Ball) -> Stream (Ball,Ball)
sim2 (ball1,ball2) = Stream (\t -> (roll ball1 t,roll ball2 t)) (fmap sim2 (collideBallBall ball1 ball2))

renderBallPair :: (Ball,Ball) -> Picture
renderBallPair (ball1,ball2) = pictures [render ball1,render ball2]

singleout :: [a] -> [(a,[a])]
singleout [] = []
singleout (a:as) = (a,as): do
	(b,bs) <- singleout as
	return (b,a:bs)

nextCollision :: [Wall] -> [Ball] -> Event [Ball]
nextCollision walls balls = earliest (
    wallBallCollisions walls balls ++ ballBallCollisions balls)

wallBallCollisions :: [Wall] -> [Ball] -> [Event [Ball]]
wallBallCollisions walls balls = do
    (ball,rest) <- singleout balls
    wall <- walls
    let collision = collideBallWall ball wall
    case collision of
        Never -> return Never
        Eventually t ball' ->
            return (Eventually t (ball' : map (flip roll t) rest))

ballBallCollisions :: [Ball] -> [Event [Ball]]
ballBallCollisions balls = do
    (ball1,rest1) <- singleout balls
    (ball2,rest2) <- singleout rest1
    let collision = collideBallBall ball1 ball2
    case collision of
        Never -> return Never
        Eventually t (ball1',ball2') ->
            return (Eventually t (ball1' : ball2' : map (flip roll t) rest2))

sim3 :: [Wall] -> [Ball] -> Stream [Ball]
sim3 walls balls = Stream (sequence (map roll balls)) (fmap (sim3 walls) (nextCollision walls balls))

renderBallList :: [Ball] -> Picture
renderBallList = pictures . map render

testwalls :: [Wall]
testwalls = [Wall (-10),Wall 150]

testballs :: [Ball]
testballs = [Ball 10 0 170,Ball 10 80 (-100),Ball 10 100 100]

main :: IO ()
main = run (fmap renderBallList (sim3 testwalls testballs))

