module Animations.Behavior where

import Control.Applicative

data Behavior next a =
  AndThen a (next (Behavior next a))

now :: Behavior next a -> a
now (AndThen a _) = a

future :: Behavior next a -> next (Behavior next a)
future (AndThen _ nextBehavior) = nextBehavior


instance (Functor next) => Functor (Behavior next) where

  fmap f (a `AndThen` nextAs) =
    (f a) `AndThen` (fmap (fmap f) nextAs)


instance (Applicative next) => Applicative (Behavior next) where

  pure a =
    a `AndThen` (pure (pure a))

  (f `AndThen` nextFs) <*> (x `AndThen` nextXs) =
    (f x) `AndThen` (liftA2 (<*>) nextFs nextXs)





