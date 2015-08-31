{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.IORef
import Control.Concurrent


type Event = Free (MaybeT IO)

never :: Event a
never = Free (MaybeT (return Nothing))

occured :: a -> Event a
occured a = Pure a

later :: Event a -> Event a
later e = Free (MaybeT (return (Just e)))


type Behavior = Cofree (MaybeT IO)

now :: Behavior a -> a
now (a :< _) = a

always :: a -> Behavior a
always a = a :< MaybeT (return Nothing)

andThen :: a -> Behavior a -> Behavior a
andThen a b = a :< MaybeT (return (Just b))


switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch _ (Pure b) = b
switch (a :< b) (Free e) = a :< liftM2 switch b' e where
    b' = b <|> return (always a)

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Just a :< _) = always (occured a)
whenJust (Nothing :< b) = Free e :< b' where
    e = fmap now b'
    b' = fmap whenJust b


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

poll :: IO a -> IO (Behavior a)
poll io = do
    a <- io
    return (a :< lift (poll io))

plan :: Event (IO a) -> IO (Event a)
plan (Pure io) = fmap occured io
plan (Free e) = return (Free (do
    e' <- e
    lift (plan e')))

main :: IO ()
main = do
    e <- test 11000
    runEvent e

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
