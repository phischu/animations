module Animations.Pure where


newtype Time = Time Float
  deriving (Show, Read, Eq, Ord, Num, Fractional)

instance Monoid Time where
  mempty = Time 0
  mappend (Time t) (Time u) = Time (max t u)

type Pull = (->) Time

type Push = (,) Time


runI :: (Show a) => Behavior Identity a -> IO ()
runI (a `AndThen` as) = do
  runI (runIdentity as)



