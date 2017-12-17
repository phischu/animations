{-# language GeneralizedNewtypeDeriving #-}
module Animations.Pure where

import Data.These (
  These(This,That,These))
import qualified GHC.Real (
  infinity)

newtype Time = Time { getTime :: Rational }
  deriving (Show, Read, Eq, Ord, Num, Fractional)

instance Monoid Time where
  mempty = Time 0
  mappend (Time t) (Time u) = Time (max t u)

infinity :: Time
infinity = Time GHC.Real.infinity


data Push a = Wait Time a
  deriving (Show, Eq)

instance Functor Push where
  fmap f (Wait t a) = Wait t (f a)

instance Applicative Push where
  pure a =
    Wait mempty a
  Wait t1 f <*> Wait t2 a =
    Wait (mappend t1 t2) (f a)

waitForever :: Push a
waitForever = Wait infinity (error "observing at infinity")

race :: Push a -> Push b -> Push (These a b)
race (Wait t1 a1) (Wait t2 a2) = case compare t1 t2 of
    LT -> Wait t1 (This a1)
    EQ -> Wait t1 (These a1 a2)
    GT -> Wait t2 (That a2)

wait :: Rational -> a -> Push a
wait t a = Wait (Time t) a


newtype Pull a = At { getAt :: Time -> a }

instance Functor Pull where
  fmap f (At p) = At (f . p)

instance Applicative Pull where
  pure a = At (const a)
  At tf <*> At ta = At (\t -> tf t (ta t))

at :: (Rational -> a) -> Pull a
at f = At (\(Time t) -> f t)


sample :: Push a -> Pull b -> Push (a, b)
sample (Wait t a) (At tb) = Wait t (a, tb t)

switch :: Pull a -> Push b -> Pull (Either a b)
switch (At ta) (Wait t b) = At (\tr -> case compare tr t of
  LT -> Left (ta tr)
  EQ -> Left (ta tr)
  GT -> Right b)

