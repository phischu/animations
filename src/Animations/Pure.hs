{-# language GeneralizedNewtypeDeriving #-}
module Animations.Pure where


newtype Time = Time Rational
  deriving (Show, Read, Eq, Ord, Num, Fractional)

instance Monoid Time where
  mempty = Time (negate (recip 0))
  mappend (Time t) (Time u) = Time (max t u)

infinity :: Time
infinity = Time (recip 0)


data Push a = Wait Time a

instance Functor Push where
  fmap f (Wait t a) = Wait t (f a)

instance Applicative Push where
  pure a =
    Wait mempty a
  Wait t1 f <*> Wait t2 a =
    Wait (mappend t1 t2) (f a)


newtype Pull a = At {at :: Time -> a }

instance Functor Pull where
  fmap f (At p) = At (f . p)

instance Applicative Pull where
  pure a = At (const a)
  At tf <*> At ta = At (\t -> tf t (ta t))


sample :: Push a -> Pull b -> Push (a, b)
sample (Wait t a) (At tb) = Wait t (a, tb t)

switch :: Pull a -> Push b -> Pull (Either a b)
switch (At ta) (Wait t b) = At (\tr -> case compare tr t of
  LT -> Left (ta tr)
  EQ -> Left (ta tr)
  GT -> Right b)

