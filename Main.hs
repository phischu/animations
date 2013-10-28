{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Graphics.Gloss

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

data Ball = Ball Position Velocity
type Position = Float
type Velocity = Float

deriving instance Show Ball

render :: Ball -> Picture
render (Ball x _) = translate x 0 (circle 10)

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
collideBallWall (Ball x v) (Wall w)
    | v == 0 = Never
    | (w - x) / v <= 0 = Never
    | otherwise = Eventually ((w - x) / v) (Ball w (negate v))

roll :: Ball -> T -> Ball
roll (Ball x v) t = Ball (x + v * t) v

sim1 :: [Wall] -> Ball -> Stream Ball
sim1 walls ball = Stream (roll ball) (fmap (sim1 walls) (earliest (map (collideBallWall ball) walls)))

collideBallBall :: Ball -> Ball -> Event (Ball,Ball)
collideBallBall (Ball x1 v1) (Ball x2 v2)
    | v2 - v1 == 0 = Never
    | (x2 - x1) / (v1 - v2) <= 0 = Never
    | otherwise = Eventually t (Ball (x1 + v1 * t) (negate v2),Ball (x2 + v2 * t) (negate v1)) where
    	t = (x2 - x1) / (v1 - v2)

sim2 :: (Ball,Ball) -> Stream (Ball,Ball)
sim2 (ball1,ball2) = Stream (\t -> (roll ball1 t,roll ball2 t)) (fmap sim2 (collideBallBall ball1 ball2))

renderBallPair :: (Ball,Ball) -> Picture
renderBallPair (ball1,ball2) = pictures [render ball1,render ball2]

nextCollisions :: [Wall] -> [Ball] -> Event [Ball]
nextCollisions walls balls = earliest (map (uncurry (nextCollision walls)) (singleout balls))

singleout :: [a] -> [(a,[a])]
singleout [] = []
singleout (a:as) = (a,as): do
	(b,bs) <- singleout as
	return (b,a:bs)

collideBallBalls :: Ball -> [Ball] -> Event [Ball]
collideBallBalls _ [] = Never
collideBallBalls ball (otherball:balls) = earlier
    (fmap (\(ball1,ball2) -> ball1:ball2:balls) (collideBallBall ball otherball))
    (fmap (otherball:) (collideBallBalls ball balls))

collideBallWalls :: Ball -> [Wall] -> Event Ball
collideBallWalls ball = earliest . map (collideBallWall ball)

nextCollision :: [Wall] -> Ball -> [Ball] -> Event [Ball]
nextCollision walls ball balls = earlier
    (collideBallBalls ball balls)
    (fmap (:balls) (collideBallWalls ball walls)) 

sim3 :: [Wall] -> [Ball] -> Stream [Ball]
sim3 walls balls = Stream (sequence (map roll balls)) (fmap (sim3 walls) (nextCollisions walls balls))

renderBallList :: [Ball] -> Picture
renderBallList = pictures . map render

testwalls :: [Wall]
testwalls = [Wall (-10),Wall 150]

testballs :: [Ball]
testballs = [Ball 0 17,Ball 80 (-10),Ball 100 10]

main :: IO ()
main = run (fmap renderBallList (sim3 [] [Ball 20 50,Ball 30 130,Ball 160 (-30)]))

