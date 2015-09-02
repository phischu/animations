{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent


type Next = MaybeT IO

type Event = Free Next

type Behavior = Cofree Next


never :: Event a
never = Free (MaybeT (return Nothing))

occured :: a -> Event a
occured a = Pure a

later :: Next (Event a) -> Event a
later e = Free e

inspect :: Event a -> Either a (Next (Event a))
inspect (Pure a) = Left a
inspect (Free e) = Right e


always :: a -> Behavior a
always a = a :< MaybeT (return Nothing)

now :: Behavior a -> a
now (a :< _) = a

future :: Behavior a -> Next (Behavior a)
future (_ :< b) = b

andThen :: a -> Next (Behavior a) -> Behavior a
andThen a b = a :< b

delay :: a -> Next a
delay = return

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = case inspect e of
    Left b' -> b'
    Right e' -> a `andThen` liftA2 switch b' e'
      where
        a = now b
        b' = future b <|> delay (always a)

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust b = case now b of
    Just a -> always (occured a)
    Nothing -> later e `andThen` b'
      where
        b' = fmap whenJust (future b)
        e = fmap now b'

plan :: Event (IO a) -> IO (Event a)
plan e = case inspect e of
    Left io -> fmap occured io
    Right nextEvent -> return (later (do
        event <- nextEvent
        lift (plan event)))

poll :: Behavior (IO a) -> IO (Behavior a)
poll b = do
    let io = now b
    a <- io
    return (a `andThen` (do
        b' <- future b <|> delay (always io)
        lift (poll b')))

async :: IO a -> IO (Event a)
async io = do
    resultRef <- newIORef Nothing
    forkIO (io >>= writeIORef resultRef . Just)
    let go = do
            r <- lift (readIORef resultRef)
            case r of
                Nothing -> return (Free go)
                Just a -> return (Pure a)
    return (Free go)




runMaster :: Behavior (Event a) -> IO a
runMaster (Pure a :< _) = return a
runMaster (_ :< mb) =
    runMaybeT mb >>= maybe (error "runBehavior loop") (\b -> do
        threadDelay 500000
        runMaster b)

waitEvent :: Event () -> IO ()
waitEvent (Pure ()) = return ()
waitEvent (Free me) = do
    runMaybeT me >>= maybe (putStrLn "waitEvent loop") waitEvent

runBehavior :: (Show a) => Behavior a -> IO ()
runBehavior(a :< mb) = do
    print a
    runMaybeT mb >>= maybe (putStrLn "fini") runBehavior

runEvent :: (Show a) => Event a -> IO ()
runEvent (Pure a) = print a
runEvent (Free me) = runMaybeT me >>= maybe (putStrLn "never") (\e -> do
    putStrLn "."
    threadDelay 500000
    runEvent e)


what :: IO ()
what = do
    inputs <- poll (always getLine)
    runBehavior (accum "" (fmap (++) inputs))

main :: IO ()
main = do
    e <- test 11
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
wait e = case inspect e of
    Left a -> always (occured a)
    Right e' -> later e' `andThen` fmap wait e'