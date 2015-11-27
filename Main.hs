{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time


type Behavior = BehaviorT Next

type Event = EventT Next

type Next = IO

now :: Behavior a -> a
now = _now

future :: Behavior a -> Next (Behavior a)
future = _future

always :: a -> Behavior a
always a = a `andThen` return (always a)

andThen :: a -> Next (Behavior a) -> Behavior a
andThen = AndThen


never :: Event a
never = Later (return never)

occured :: a -> Event a
occured = Occured

later :: Next (Event a) -> Event a
later = Later


delay :: a -> Next a
delay = return


data EventT m a =
    Occured a |
    Later (m (EventT m a))
      deriving (Functor)

instance (Applicative m) => Applicative (EventT m) where
    pure = Occured
    Occured f <*> Occured x = pure (f x)
    Occured f <*> Later mx = Later (liftA2 (<*>) (pure (Occured f)) mx)
    Later mf <*> Occured x = Later (liftA2 (<*>) mf (pure (Occured x)))
    Later mf <*> Later mx = Later (liftA2 (<*>) mf mx)


data BehaviorT m a = AndThen {
    _now :: a,
    _future :: m (BehaviorT m a)}
      deriving (Functor)

instance (Applicative m) => Applicative (BehaviorT m) where
    pure a = a `AndThen` pure (pure a)
    (f `AndThen` mf) <*> (x `AndThen` mx) = f x `AndThen` liftA2 (<*>) mf mx




switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = case e of
    Occured b' -> b'
    Later e' -> now b `AndThen` liftA2 switch (future b) e'

whenJust :: Behavior (Maybe a) -> Event a
whenJust b = case now b of
    Just a -> occured a
    Nothing -> later (fmap whenJust (future b))

plan :: Event (Next a) -> Next (Event a)
plan e = case e of
  Occured io -> fmap occured io
  Later nextEvent -> return (later (nextEvent >>= plan))

poll :: Behavior (Next a) -> Next (Behavior a)
poll b = do
  a <- now b
  return (a `andThen` (future b >>= poll))

waitEvent :: Event a -> IO ()
waitEvent (Occured _) = return ()
waitEvent (Later me) = me >>= waitEvent

runBehavior :: Next (Behavior a) -> IO ()
runBehavior nextBehavior = do
  behavior <- runNext nextBehavior
  runBehavior (future behavior)

runEvent :: Next (Event a) -> IO ()
runEvent nextEvent = do
  event <- runNext nextEvent
  case event of
    Occured a -> return ()
    Later nextEvent' -> runEvent nextEvent'

runNext :: Next a -> IO a
runNext next = next

getLines :: Next (Behavior String)
getLines = poll (always (liftIO getLine))

getCurrentTimes :: Next (Behavior UTCTime)
getCurrentTimes = poll (always (liftIO getCurrentTime))

timeSince :: UTCTime -> Next (Behavior NominalDiffTime)
timeSince startTime = fmap (fmap diffStartTime) getCurrentTimes where
  diffStartTime = flip diffUTCTime startTime

main :: IO ()
main = runEvent (do

  startTime <- liftIO getCurrentTime
  times <- timeSince startTime

  userInput <- getLines

  let timesWithUserInput = liftA2 (,) times userInput
  shouldQuit <- poll (fmap userExit timesWithUserInput)

  pure (whenTrue shouldQuit))

userExit :: (NominalDiffTime, String) -> Next Bool
userExit (_, "exit") =
  pure True
userExit (time, input) = do
  liftIO (putStrLn (show time ++ ": " ++ input))
  pure False

{-
test :: Int -> Now (Event ())
test n = do b <- count 
            e <- sample (when ((n ==) <$> b))
            return e

count :: Now (Behavior Int)
count = loop 0 where
  loop i =  do  e <- async (return ())
                e'<- planNow (loop (i+1) <$ e)
                return (pure i `switch` e')
-}

whenTrue :: Behavior Bool -> Event ()
whenTrue behavior = case now behavior of
  True -> occured ()
  False -> later (fmap whenTrue (future behavior))

register :: Event a -> ((a -> IO ()) -> IO ())
register event handler = do
    plan (fmap handler event)
    return ()

callback :: IO (a -> IO (), Event a)
callback = error "not implemented: callback"

whenCalled :: ((a -> IO ()) -> IO ()) -> IO (Event a)
whenCalled k = do
    (handler, event) <- callback
    k handler
    return event


accum :: a -> Behavior (a -> a) -> Behavior a
accum a b =
    let f = now b
        b' = future b
        a' = f a
    in a' `andThen` fmap (accum a') b'


wait :: Event a -> Behavior (Event a)
wait e = case e of
    Occured a -> always (occured a)
    Later e' -> later e' `andThen` fmap wait e'

{-
what :: IO ()
what = do
    inputs <- poll (always getLine)
    runBehavior (accum "" (fmap (++) inputs))
-}
