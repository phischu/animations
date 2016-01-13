{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Main where

import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time
import Control.Monad


type Behavior = BehaviorT Next

type Event = EventT Next

newtype Next a = Next {runNext :: IO a}
  deriving (Functor, Applicative)

now :: Behavior a -> a
now = _now

future :: Behavior a -> Next (Behavior a)
future = _future

always :: a -> Behavior a
always a = a `andThen` pure (always a)

andThen :: a -> Next (Behavior a) -> Behavior a
andThen = AndThen


never :: Event a
never = Later (pure never)

occured :: a -> Event a
occured = Occured

later :: Next (Event a) -> Event a
later = Later


delay :: a -> Next a
delay = pure

syncIO :: IO a -> Next a
syncIO = Next

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

instance (Applicative m) => Monad (EventT m) where
    return = pure
    Occured a >>= mf = mf a
    Later ma >>= mf = Later (fmap (>>= mf) ma)

data BehaviorT m a = AndThen {
    _now :: a,
    _future :: m (BehaviorT m a)}
      deriving (Functor)

instance (Applicative m) => Applicative (BehaviorT m) where
    pure a = a `AndThen` pure (pure a)
    (f `AndThen` mf) <*> (x `AndThen` mx) = f x `AndThen` liftA2 (<*>) mf mx


-- type Pull = Reader Time
-- Behavior Pull a
-- Behavior Push a
-- pullIO :: IO a -> Pull a
-- synchronous

-- type Push = Writer Time
-- instance Monoid Time where
--   mempty = negative infinity
--   mappend = max
-- Event Pull a
-- Event Push a
-- pushIO :: IO a -> Push a
-- asynchrounous


switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = case e of
    Occured b' -> b'
    Later e' -> now b `AndThen` liftA2 switch (future b) e'

whenJust :: Behavior (Maybe a) -> Event a
whenJust behavior = case now behavior of
    Just a -> occured a
    Nothing -> later (fmap whenJust (future behavior))

plan :: Event (Next a) -> Next (Event a)
plan event = case event of
  Occured nextA -> fmap occured nextA
  Later nextEvent -> fmap (later . plan) nextEvent

poll :: Behavior (Next a) -> Next (Behavior a)
poll behavior =
  liftA2 andThen (now behavior) (fmap poll (future behavior))

runEvent :: Event a -> IO a
runEvent event = case event of
  Occured a -> return a
  Later nextEvent -> do
    event' <- runNext nextEvent
    runEvent event'

nextLine :: Next (Event String)
nextLine = plan (Occured (syncIO getLine))

currentTime :: Next (Behavior UTCTime)
currentTime = poll (always (syncIO getCurrentTime))

nextPutStrLn :: String -> Next (Event ())
nextPutStrLn message = plan (Occured (syncIO (putStrLn message)))


whenTrue :: Behavior Bool -> Event ()
whenTrue behavior = case now behavior of
  True -> occured ()
  False -> later (fmap whenTrue (future behavior))

sample :: Event a -> Behavior b -> Event (a, b)
sample (Occured a) (AndThen b _) =
  Occured (a, b)
sample (Later nextEvent) (AndThen _ nextBehavior) =
  Later (liftA2 sample nextEvent nextBehavior)


loop :: Event String -> Behavior UTCTime -> Event ()
loop (Occured "exit") _ = do
  Occured ()
loop (Occured message) (AndThen time futureTime) =
  Later (
    syncIO (putStrLn (show time ++ ": " ++ message)) *>
    liftA2 loop nextLine futureTime)
loop (Later nextEvent) (AndThen _ futureTime) =
  Later (
    liftA2 loop nextEvent futureTime)


main :: IO ()
main = runEvent (Later (
  liftA2 loop nextLine currentTime))



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
