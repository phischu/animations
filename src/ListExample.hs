module Main where


import Animations

import Control.Applicative
import Data.Functor.Identity


main :: IO ()
main = list_example


list_example :: IO ()
list_example = do

  let numbers = countFrom 0

  let alwaysNumbers = fmap now (always numbers)

  let pairs = liftA2 (,) numbers alwaysNumbers

  runList pairs

runList :: (Show a) => Behavior Identity a -> IO ()
runList (a `AndThen` identityAs) = do
  print a
  runList (runIdentity identityAs)



