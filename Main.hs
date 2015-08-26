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


runBehavior :: Behavior (Event a) -> IO a
runBehavior (Pure a :< _) = return a
runBehavior (_ :< b) =
    runMaybeT b >>= maybe (error "runBehavior loop") runBehavior


runEvent :: Event a -> IO a
runEvent (Pure a) = return a
runEvent (Free (MaybeT io)) = io >>= maybe (error "runEvent loop") runEvent

async :: IO a -> IO (Event a)
async io = do
    resultRef <- newIORef Nothing
    forkIO (io >>= writeIORef resultRef . Just)
    b <- poll (readIORef resultRef)
    return (now (whenJust b))

poll :: IO a -> IO (Behavior a)
poll io = do
    a <- io
    return (a :< lift (poll io))

plan :: Event (IO a) -> IO (Event a)
plan (Pure io) = fmap occured io
plan (Free e) = runMaybeT e >>= maybe (return never) plan


n :: Int
n = 11

main :: IO ()
main = do
    e <- test n
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