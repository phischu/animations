module Main where

import Animations (
  Behavior(AndThen))
import Animations.Pure (
  Push(Wait), wait, waitForever, race,
  Pull, at, sample, Time(Time))
import Animations.Prelude (
  unfold)

import Linear (
  (*^), (^+^), V2(V2))
import Data.These (
  These(This,That,These))

import Control.Concurrent (
  threadDelay)


data Ball = Ball (V2 Rational) (V2 Rational)
  deriving (Show, Eq)

data Wall = HorizontalWall Rational | VerticalWall Rational
  deriving (Show, Eq)

data Collision = NoCollision | HorizontalCollision | VerticalCollision
  deriving (Show, Eq)


rolling :: Ball -> Pull Ball
rolling (Ball x v) = at (\t -> Ball (x ^+^ t *^ v) v)

ballRadius :: Rational
ballRadius = 1

collision :: Wall -> Ball -> Push Collision
collision (HorizontalWall p1) (Ball (V2 x1 _) (V2 v1 _)) =
  case compare v1 0 of
    GT -> let t = (p1 - x1 - ballRadius) / v1
      in waitForeverIfPast t HorizontalCollision
    EQ -> waitForever
    LT -> let t = (p1 - x1 + ballRadius) / v1
      in waitForeverIfPast t HorizontalCollision
collision (VerticalWall p2) (Ball (V2 _ x2) (V2 _ v2)) =
  case compare v2 0 of
    GT -> let t = (p2 - x2 - ballRadius) / v2
      in waitForeverIfPast t VerticalCollision
    EQ -> waitForever
    LT -> let t = (p2 - x2 + ballRadius) / v2
      in waitForeverIfPast t VerticalCollision

waitForeverIfPast :: Rational -> a -> Push a
waitForeverIfPast t a
  | t > 0 = wait t a
  | otherwise = waitForever

collide :: Collision -> Ball -> Ball
collide NoCollision ball =
  ball
collide HorizontalCollision (Ball x (V2 v1 v2)) =
  Ball x (V2 (negate v1) v2)
collide VerticalCollision (Ball x (V2 v1 v2)) =
  Ball x (V2 v1 (negate v2))

collideList :: [Collision] -> Ball -> Ball
collideList collisions ball = foldr collide ball collisions

raceList :: [Push a] -> Push [a]
raceList [] = waitForever
raceList (p : ps) = fmap theseLists (race p (raceList ps)) where
  theseLists :: These a [a] -> [a]
  theseLists (This a) = [a]
  theseLists (That as) = as
  theseLists (These a as) = a : as

bouncing :: Ball -> Push Ball
bouncing ball =
  fmap (uncurry collideList) (
    sample nextCollisions (rolling ball)) where
      nextCollisions = raceList [
        collision wall1 ball,
        collision wall2 ball,
        collision wall3 ball,
        collision wall4 ball]

wall1 :: Wall
wall1 = HorizontalWall 10

wall2 :: Wall
wall2 = HorizontalWall (negate 10)

wall3 :: Wall
wall3 = VerticalWall 10

wall4 :: Wall
wall4 = VerticalWall (negate 10)

ball1 :: Ball
ball1 = Ball (V2 0 0) (V2 7 3)

runPushBehavior :: (Show a) => Behavior Push a -> IO x
runPushBehavior (a `AndThen` (Wait (Time t) ab)) = do
  threadDelay (round (t * 1000000))
  putStrLn (show t ++ ": " ++ show a)
  runPushBehavior ab

main :: IO ()
main = runPushBehavior (unfold bouncing ball1)

