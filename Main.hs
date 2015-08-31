{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Applicative
import Data.IORef
import Control.Concurrent


type Event m = Free m

never :: (Applicative m) => Event m a
never = later never

occured :: (Functor m) => a -> Event m a
occured a = pure a

later :: (Applicative m) => Event m a -> Event m a
later e = Free (pure e)


type Behavior m = Cofree m

now :: Behavior m a -> a
now (a :< _) = a

always :: (Applicative m) => a -> Behavior m a
always a = a :< pure (always a)

andThen :: (Applicative m) => a -> Behavior m a -> Behavior m a
andThen a b = a :< pure b


switch :: (Applicative m) => Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch _ (Pure b) = b
switch (a :< b) (Free e) = a :< liftA2 switch b e

whenJust :: (Applicative m) => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust (Just a :< _) = always (occured a)
whenJust (Nothing :< b) = Free e :< b' where
    e = fmap now b'
    b' = fmap whenJust b

async :: (MonadIO m) => IO a -> m (Event m a)
async io = do
    resultRef <- liftIO (newIORef Nothing)
    liftIO (forkIO (io >>= writeIORef resultRef . Just))
    let go = do
            r <- liftIO (readIORef resultRef)
            case r of
                Nothing -> return (Free go)
                Just a -> return (Pure a)
    return (Free go)

poll :: (Monad m) => m a -> m (Behavior m a)
poll ma = do
    a <- ma
    return (a :< (poll ma))

plan :: (Monad m, Functor m) => Event m (m a) -> m (Event m a)
plan (Pure ma) = fmap occured ma
plan (Free me) = return (Free (me >>= plan))

waitEvent :: (Monad m) => Event m () -> m ()
waitEvent (Pure ()) = return ()
waitEvent (Free me) = me >>= waitEvent

runBehavior :: (Show a, MonadIO m) => Behavior m a -> m [a]
runBehavior(a :< mb) = do
    liftIO (print a)
    mb >>= runBehavior

runEvent :: (Show a, MonadIO m) => Event m a -> m ()
runEvent (Pure a) = liftIO (print a)
runEvent (Free me) = do
    e <- me
    liftIO (putStrLn "." >> threadDelay 500000)
    runEvent e


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

test :: Int -> IO (Event IO ())
test n = do b <- count
            e <- return (now (whenTrue ((n ==) <$> b)))
            return e

count :: IO (Behavior IO Int)
count = loop 0 where
    loop i = do e <- async (return ())
                e' <- plan (loop (i+1) <$ e)
                return (pure i `switch` e')

whenTrue :: (Applicative m) => Behavior m Bool -> Behavior m (Event m ())
whenTrue = whenJust . fmap boolToMaybe where
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing
