module Animations.Copy where


import Control.Applicative
import GHC.Dup


data Copy a = Copy a

copy :: a -> Copy a
copy a = Copy a

getCopy :: Copy a -> a
getCopy (Copy a) = case dup a of Box a' -> a'

instance Functor Copy where
  fmap f a = copy (f (getCopy a))

instance Applicative Copy where
  pure a = copy a
  f <*> a = copy ((getCopy f) (getCopy a))


