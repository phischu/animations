module Main where


import Animations
import Animations.IO

import Data.Time

import Control.Applicative



main :: IO ()
main = lonelyChat

lonelyChat :: IO ()
lonelyChat = runEvent (Later (
  liftA2 loop nextLine currentTime))

nextLine :: Next (Event Next String)
nextLine = plan (Occured (syncIO getLine))

currentTime :: Next (Behavior Next UTCTime)
currentTime = poll (always (syncIO getCurrentTime))

loop :: Event Next String -> Behavior Next UTCTime -> Event Next ()
loop (Occured "exit") _ = do
  Occured ()
loop (Occured message) (AndThen time futureTime) =
  Later (
    syncIO (putStrLn (show time ++ ": " ++ message)) *>
    liftA2 loop nextLine futureTime)
loop (Later nextEvent) (AndThen _ futureTime) =
  Later (
    liftA2 loop nextEvent futureTime)

runEvent :: Event Next a -> IO a
runEvent (Occured a) =
  return a
runEvent (Later nextEvent) =
  runNext nextEvent >>= runEvent


