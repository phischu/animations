module Main where


import Animations
import Animations.Copy

import Control.Applicative


main :: IO ()
main = copy_example

copy_example :: IO ()
copy_example = do

  let numbers = countFrom 0

  let alwaysNumbers = fmap now (always numbers)

  let pairs = liftA2 (,) numbers alwaysNumbers

  runCopying pairs

runCopying :: (Show a) => Behavior Copy a -> IO ()
runCopying (a `AndThen` copyAs) = do
  print a
  runCopying (getCopy copyAs)


