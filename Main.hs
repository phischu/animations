{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent



type Behavior = BehaviorT Next

type Event = EventT Next

type Next = IO


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
    now :: a,
    future :: m (BehaviorT m a)}
      deriving (Functor)

instance (Applicative m) => Applicative (BehaviorT m) where
    pure a = a `AndThen` pure (pure a)
    (f `AndThen` mf) <*> (x `AndThen` mx) = f x `AndThen` liftA2 (<*>) mf mx


always :: a -> Behavior a
always a = a `andThen` return (always a)

never :: Event a
never = Later (return never)


andThen :: a -> Next (Behavior a) -> Behavior a
andThen = AndThen

occured :: a -> Event a
occured = Occured

later :: Next (Event a) -> Event a
later = Later


delay :: a -> Next a
delay = return

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = case e of
    Occured b' -> b'
    Later e' -> now b `AndThen` liftA2 switch (future b) e'

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust b = case now b of
    Just a -> always (occured a)
    Nothing -> later e `AndThen` b'
      where
        b' = fmap whenJust (future b)
        e = fmap now b'

plan :: Event (IO a) -> IO (Event a)
plan e = case e of
    Occured io -> fmap occured io
    Later nextEvent -> return (later (nextEvent >>= plan))

poll :: Behavior (IO a) -> IO (Behavior a)
poll b = do
    a <- now b
    return (a `andThen` (future b >>= poll))

async :: IO a -> IO (Event a)
async io = do
    resultRef <- newIORef Nothing
    forkIO (io >>= writeIORef resultRef . Just)
    let go = do
            r <- readIORef resultRef
            case r of
                Nothing -> return (Later go)
                Just a -> return (Occured a)
    return (Later go)

waitEvent :: Event a -> IO ()
waitEvent (Occured _) = return ()
waitEvent (Later me) = me >>= waitEvent

runBehavior :: (Show a) => Behavior a -> IO ()
runBehavior(a `AndThen` mb) = do
    print a
    mb >>= runBehavior

runEvent :: (Show a) => Event a -> IO ()
runEvent (Occured a) = print a
runEvent (Later me) = me >>= (\e -> do
    putStrLn "."
    threadDelay 500000
    runEvent e)



main :: IO ()
main = do
    e <- test 11000
    waitEvent e

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

test :: Int -> IO (Event ())
test n = do b <- count
            e <- return (now (whenTrue ((n ==) <$> b)))
            return e

count :: IO (Behavior Int)
count = loop 0 where
    loop i = do e <- async (return ())
                e' <- plan (loop (i+1) <$ e)
                return (pure i `switch` e')

whenTrue :: Behavior Bool -> Behavior (Event ())
whenTrue = whenJust . fmap boolToMaybe where
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing


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