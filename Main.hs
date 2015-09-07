{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent
import System.IO.Unsafe

-- http://blog.openendings.net/2014/10/two-sound-axiom-systems-for-linear.html
-- http://plato.stanford.edu/entries/logic-temporal/
-- Completeness?
-- https://en.wikipedia.org/wiki/Normal_modal_logic

type Behavior = BehaviorT Next

type Event = EventT Next

type Next = Once

data Once a = Once { runOnce :: IO a }

instance Functor Once where
    fmap f (Once io) = Once (fmap f io)

instance Applicative Once where
    pure a = Once (pure a)
    Once f <*> Once x = Once (f <*> x)

instance Monad Once where
    return = pure
    ox >>= ok = joinOnce (fmap ok ox)

joinOnce :: Once (Once a) -> Once a
joinOnce (Once ioonce) = Once (do
    Once io <- ioonce
    a <- io
    return a)

liftOnce :: IO a -> Once a
liftOnce io = unsafePerformIO (do 
    r <- newIORef Nothing
    return (Once (do
        ma <- readIORef r
        case ma of
            Nothing -> do
                a <- io
                writeIORef r (Just a)
                return a
            Just a -> return a)))

liftOften :: IO a -> Once a
liftOften = Once

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

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust b = case now b of
    Just a -> always (occured a)
    Nothing -> later e `AndThen` b'
      where
        b' = fmap whenJust (future b)
        e = fmap now b'

plan :: Event (Next a) -> Next (Event a)
plan e = case e of
    Occured io -> fmap occured io
    Later nextEvent -> pure (later (nextEvent >>= plan))

poll :: Behavior (Next a) -> Next (Behavior a)
poll b = do
    a <- now b
    pure (a `andThen` (future b >>= poll))

async :: IO a -> Next (Event a)
async io = do
    r <- liftOnce (do
        resultRef <- newIORef Nothing
        forkIO (io >>= writeIORef resultRef . Just)
        return resultRef)
    b <- poll (always (liftOften (readIORef r)))
    return (later (pure (now (whenJust b))))


waitEvent :: Event a -> IO ()
waitEvent (Occured _) = return ()
waitEvent (Later me) = runOnce me >>= waitEvent

runBehavior :: (Show a) => Behavior a -> IO ()
runBehavior(a `AndThen` mb) = do
    print a
    runOnce mb >>= runBehavior

runEvent :: (Show a) => Event a -> IO ()
runEvent (Occured a) = print a
runEvent (Later me) = runOnce me >>= (\e -> do
    putStrLn "."
    threadDelay 500000
    runEvent e)



main :: IO ()
main = do
    waitEvent (later (test 11000))

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

test :: Int -> Next (Event ())
test n = do b <- count
            e <- return (now (whenTrue ((n ==) <$> b)))
            return e

count :: Next (Behavior Int)
count = loop 0 where
    loop i = do e <- async (return ())
                e' <- plan (loop (i+1) <$ e)
                return (pure i `switch` e')

whenTrue :: Behavior Bool -> Behavior (Event ())
whenTrue = whenJust . fmap boolToMaybe where
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing

{-
register :: Event a -> ((a -> IO ()) -> IO ())
register event handler = do
    plan (fmap handler event)
    return ()
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

snapshot :: Behavior a -> Event () -> Event a
snapshot b e = case e of
    Occured () -> Occured (now b)
    Later nextEvent -> Later (liftA2 snapshot (future b) nextEvent)

{-
what :: IO ()
what = do
    inputs <- poll (always getLine)
    runBehavior (accum "" (fmap (++) inputs))
-}

indo :: Behavior (a -> Next a) -> a -> Behavior a
indo = undefined

