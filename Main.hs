{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Graphics.Gloss

import Control.Monad.Free (Free(Pure,Free))

type T = Float

data R x = R T (T -> x) x

type Motion = Free R

deriving instance Functor R

sample :: T -> Motion x -> x
sample _ (Pure x) = x
sample t (Free (R s f c))
    | t < s = sample t (f t)
    | otherwise = sample t c

motion :: T -> (T -> x) -> Motion x
motion t f = Free (R t (Pure . f) (Pure (f 0)))

discrete :: T -> x -> Motion x
discrete t x = motion t (const x)

wait :: T -> Motion ()
wait t = discrete t ()

continuous :: (T -> x) -> Motion x
continuous f = motion 0 f

time :: Motion T
time = motion 0 id

run :: Motion Picture -> IO ()
run r = animate
    (InWindow "Aninmation" (600,600) (100,100))
    white
    (\t -> sample t r)

test :: Motion Picture
test = do
    p <- continuous (\x -> translate (200 * x) (10 * x) (circle 20))
    wait 1
    return p

main :: IO ()
main = run test

