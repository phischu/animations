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
import Data.Monoid


data Event next a =
    Occured a |
    Later (next (Event next a))
      deriving (Functor)

instance (Applicative next) => Applicative (Event next) where
    pure = Occured
    Occured f <*> Occured x = pure (f x)
    Occured f <*> Later mx = Later (liftA2 (<*>) (pure (Occured f)) mx)
    Later mf <*> Occured x = Later (liftA2 (<*>) mf (pure (Occured x)))
    Later mf <*> Later mx = Later (liftA2 (<*>) mf mx)

instance (Applicative next) => Monad (Event next) where
    return = pure
    Occured a >>= mf = mf a
    Later ma >>= mf = Later (fmap (>>= mf) ma)

data Behavior next a = AndThen {
    now :: a,
    future :: next (Behavior next a)}
      deriving (Functor)

instance (Applicative next) => Applicative (Behavior next) where
    pure a = a `AndThen` pure (pure a)
    (f `AndThen` mf) <*> (x `AndThen` mx) = f x `AndThen` liftA2 (<*>) mf mx



always :: (Applicative next) => a -> Behavior next a
always a = a `AndThen` pure (always a)


never :: (Applicative next) => Event next a
never = Later (pure never)


newtype Time = Time Float
  deriving (Show, Read, Eq, Ord, Num, Fractional)

instance Monoid Time where
  mempty = Time 0
  mappend (Time t) (Time u) = Time (max t u)

type Pull = (->) Time

type Push = (,) Time



switch :: (Applicative next) => Behavior next a -> Event next (Behavior next a) -> Behavior next a
switch _ (Occured behavior') =
  behavior'
switch (a `AndThen` nextBehavior) (Later nextEvent) =
  a `AndThen` liftA2 switch nextBehavior nextEvent

whenJust :: (Functor next) => Behavior next (Maybe a) -> Event next a
whenJust (Just a `AndThen` nextBehavior) =
  Occured a
whenJust (Nothing `AndThen` nextBehavior) =
  Later (fmap whenJust nextBehavior)

whenTrue :: (Functor next) => Behavior next Bool -> Event next ()
whenTrue (True `AndThen` _) =
  Occured ()
whenTrue (False `AndThen` nextBehavior) =
  Later (fmap whenTrue nextBehavior)

sample :: (Applicative next) => Event next a -> Behavior next b -> Event next (a, b)
sample (Occured a) (AndThen b _) =
  Occured (a, b)
sample (Later nextEvent) (AndThen _ nextBehavior) =
  Later (liftA2 sample nextEvent nextBehavior)

plan :: (Functor next) => Event next (next a) -> next (Event next a)
plan (Occured nextA) =
  fmap Occured nextA
plan (Later nextEvent) =
  fmap (Later . plan) nextEvent

poll :: (Applicative next) => Behavior next (next a) -> next (Behavior next a)
poll (nextA `AndThen` nextBehavior) =
  liftA2 AndThen nextA (fmap poll nextBehavior)

runEvent :: Event IO a -> IO a
runEvent (Occured a) =
  return a
runEvent (Later nextEvent) =
  nextEvent >>= runEvent

runBehaviorTiming :: (Show a) => Behavior IO a -> IO ()
runBehaviorTiming (a `AndThen` nextBehavior) = do
  before <- getCurrentTime
  print a
  behavior' <- nextBehavior
  after <- getCurrentTime
  print (diffUTCTime after before)
  runBehavior behavior'

main :: IO ()
main = runBehavior (fmap now (bad_const count))

runBehavior :: (Show a) => Behavior IO a -> IO ()
runBehavior (a `AndThen` nextBehavior) = do
  print a
  behavior' <- nextBehavior
  runBehavior behavior'

bad_const :: Behavior IO Int -> Behavior IO (Behavior IO Int)
bad_const ns = ns `AndThen` pure (bad_const ns)

count :: Behavior IO Int
count = loop 0 where
  loop i = i `AndThen` pure (loop (i+1))

lonelyChat :: IO ()
lonelyChat = runEvent (Later (
  liftA2 loop nextLine currentTime))

nextLine :: IO (Event IO String)
nextLine = plan (Occured getLine)

currentTime :: IO (Behavior IO UTCTime)
currentTime = poll (always getCurrentTime)

loop :: Event IO String -> Behavior IO UTCTime -> Event IO ()
loop (Occured "exit") _ = do
  Occured ()
loop (Occured message) (AndThen time futureTime) =
  Later (
    putStrLn (show time ++ ": " ++ message) *>
    liftA2 loop nextLine futureTime)
loop (Later nextEvent) (AndThen _ futureTime) =
  Later (
    liftA2 loop nextEvent futureTime)


















{-
mainProgram = do
>     render (Color white $ Scale 0.2 0.2 $ Text "Type some text")


>     forM_ [780, 760..] $ \ypos -> do
>         forM_ [0, 20..980] $ \xpos -> do



>             event <- iterateUntil keydown $ input



>             let key = case event of
>                         EventKey (Char key)            Down _ _ -> key
>                         EventKey (SpecialKey KeySpace) Down _ _ -> ' '



>             when (ypos == 780 && xpos == 0) $ cls
>             render $ Color white $ Translate (xpos-500) (ypos-400) $ Scale 0.2 0.2 $ Text $ [key]
-}


