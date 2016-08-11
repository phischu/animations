module Animations.IO where


import Animations.Copy

import Control.Applicative


newtype Next a = Next (Copy (IO a))

instance Functor Next where
  fmap f (Next action) = Next (fmap (fmap f) action)

instance Applicative Next where
  pure a = Next (pure (pure a))
  Next f <*> Next a = Next (liftA2 (<*>) f a)

runNext :: Next a -> IO a
runNext (Next action) = getCopy action

syncIO :: IO a -> Next a
syncIO action = Next (pure action)



