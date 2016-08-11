{-# language DeriveFunctor #-}
module Animations.Event where

import Control.Applicative


data Event next a =
    Occured a |
    Later (next (Event next a))
      deriving (Functor)


instance (Applicative next) => Applicative (Event next) where
    pure = Occured
    Occured f <*> Occured x = pure (f x)
    Occured f <*> Later mx = Later (liftA2 (<*>) (pure (Occured f)) mx)
    Later mf <*> Occured x = Later (liftA2 (<*>) mf (pure (Occured x)))
    Later mf <*> Later mx = Later (liftA2 (<*>) mf mx)


instance (Applicative next) => Monad (Event next) where
    return = pure
    Occured a >>= mf = mf a
    Later ma >>= mf = Later (fmap (>>= mf) ma)



