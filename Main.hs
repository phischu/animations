{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where

import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (catMaybes)

data T = T Float | Never deriving (Show,Ord,Eq)

data Clip a = Clip T (Float -> a)

data ClipM a where
    Return :: a -> ClipM a
    Bind :: (Pict x) => Clip x -> (x -> ClipM a) -> ClipM a

class Pict a where
    toPict :: a -> String

instance Pict Ball where
    toPict = show

instance (Pict a) => Pict [a] where
    toPict = show . map toPict

instance Monad ClipM where
    return = Return
    (Return x) >>= k = k x
    (Bind c f) >>= k = Bind c (\x -> f x >>= k)

data Ball = Ball Position Velocity deriving Show
type Position = Float
type Velocity = Float

type Wall = Float

bounce :: [Ball] -> ClipM [Ball]
bounce balls = do
    let ballclips = do
            (ball,rest) <- divide balls
            let collision = undefined
            case collision of
                Nothing -> return (neverending (roll ball))
                (Just (t,ball')) -> return (clip t (roll ball) >> return ball')
    foo ballclips >>= bounce

divide :: [a] -> [(a,[a])]
divide = undefined

neverending :: (Pict a) => (Float -> a) -> ClipM a
neverending = clip Never

clip :: (Pict a) => T -> (Float -> a) -> ClipM a
clip t f = Bind (Clip t f) Return

scenetime :: ClipM a -> T
scenetime = undefined

advance :: T -> ClipM a -> ClipM a
advance = undefined

foo :: [ClipM a] -> ClipM [a]
foo = undefined

bar :: ClipM a -> ClipM b -> ClipM (a,b)
bar ca cb = undefined

roll :: Ball -> (Float -> Ball)
roll (Ball x v) t = Ball (x+t*v) v

runClipM :: (Pict a) => ClipM a -> Float -> String
runClipM (Return a) _ = toPict a
runClipM (Bind (Clip Never f) _) t = toPict (f t)
runClipM (Bind (Clip (T e) f) k) t
    | t > e     = runClipM (k (f e)) t
    | otherwise = toPict (f t)

main :: IO ()
main = print "hallo"