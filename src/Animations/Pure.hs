{-# language GeneralizedNewtypeDeriving #-}
module Animations.Pure where


import Data.Monoid


newtype Time = Time Float
  deriving (Show, Read, Eq, Ord, Num, Fractional)

instance Monoid Time where
  mempty = Time 0
  mappend (Time t) (Time u) = Time (max t u)

type Pull = (->) Time

type Push = (,) Time



