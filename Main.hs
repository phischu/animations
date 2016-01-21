{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Main where

import Control.Comonad
import Control.Applicative
import Data.IORef
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Time
import Control.Monad
import Data.Monoid
import GHC.Dup

import Debug.Trace

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

runEvent :: Event Next a -> IO a
runEvent (Occured a) =
  return a
runEvent (Later nextEvent) =
  runNext nextEvent >>= runEvent

runBehaviorTiming :: (Show a) => Behavior IO a -> IO ()
runBehaviorTiming (a `AndThen` nextBehavior) = do
  before <- getCurrentTime
  print a
  behavior' <- nextBehavior
  after <- getCurrentTime
  putStrLn ("TIME: " ++ show (diffUTCTime after before))
  runBehaviorTiming behavior'

newtype Next a = Next (Copy (IO a))

instance Functor Next where
  fmap f (Next action) = Next (fmap (fmap f) action)

instance Applicative Next where
  pure a = Next (pure (pure a))
  Next f <*> Next a = Next (liftA2 (<*>) f a)

newtype Copy a = Copy a

copy :: a -> Copy a
copy a = Copy a

getCopy :: Copy a -> a
getCopy (Copy a) = case dup a of Box a' -> a'

instance Functor Copy where
  fmap f a = copy (f (getCopy a))

instance Applicative Copy where
  pure a = copy a
  f <*> a = copy ((getCopy f) (getCopy a))

runNext :: Next a -> IO a
runNext (Next action) = getCopy action

syncIO :: IO a -> Next a
syncIO action = Next (pure action)

runC :: (Show a) => Behavior Copy a -> IO ()
runC (_ `AndThen` as) = do
  runC (getCopy as)

runI :: (Show a) => Behavior Identity a -> IO ()
runI (a `AndThen` as) = do
  runI (runIdentity as)

runF :: (Show a) => Behavior ((->) ()) a -> IO ()
runF (a `AndThen` as) = do
  runF (as ())

runBehavior :: (Show a) => Behavior Next a -> IO ()
runBehavior (a `AndThen` nextBehavior) = do
  -- print a
  behavior' <- runNext nextBehavior
  runBehavior behavior'



data Behavior next a = AndThen {
    now :: a,
    future :: next (Behavior next a)}


instance (Functor next) => Functor (Behavior next) where

  fmap f (a `AndThen` nextAs) =
    (f a) `AndThen` (fmap (fmap f) nextAs)


instance (Applicative next) => Applicative (Behavior next) where

  pure a =
    a `AndThen` (pure (pure a))

  (f `AndThen` nextFs) <*> (x `AndThen` nextXs) =
    (f x) `AndThen` (liftA2 (<*>) nextFs nextXs)


countFrom :: (Applicative next) => Int -> Behavior next Int
countFrom i = i `AndThen` pure (countFrom (i + 1))

always :: (Applicative next) => a -> Behavior next a
always a = a `AndThen` pure (always a)

runList :: (Show a) => Behavior Identity a -> IO ()
runList (a `AndThen` identityAs) = do
  print a
  runList (runIdentity identityAs)

runCopying :: Behavior Copy (Int, Int) -> IO ()
runCopying (a `AndThen` copyAs) = do
  print a
  runCopying (getCopy copyAs)

list_example :: IO ()
list_example = do

  let numbers = countFrom 0

  let alwaysNumbers = fmap now (always numbers)

  let pairs = liftA2 (,) numbers alwaysNumbers

  runList pairs

copy_example :: IO ()
copy_example = do

  let numbers = countFrom 0

  let alwaysNumbers = fmap now (always numbers)

  let pairs = liftA2 (,) numbers alwaysNumbers

  runCopying pairs


buffer ::
  (Functor next) =>
  Int ->
  Behavior next Int ->
  Behavior next Int
buffer n (x `AndThen` nextXs) =
  n `AndThen` fmap (buffer x) nextXs

forward ::
  (Functor next) =>
  Behavior next Int ->
  next (Behavior next Int)
forward (x `AndThen` nextXs) =
  fmap (buffer x) nextXs

scary_const ::
  (Functor next) =>
  Behavior next Int ->
  Behavior next (Behavior next Int)
scary_const xs =
  xs `AndThen` fmap scary_const (forward xs)

scary_const_example :: IO ()
scary_const_example = do

  let numbers = countFrom 0

  let scary_const_numbers = fmap now (scary_const numbers)

  let pairs = liftA2 (,) numbers scary_const_numbers

  runCopying pairs


integral ::
  (Applicative next) =>
  Double ->
  Behavior next Double ->
  Behavior next Double
integral x (v `AndThen` nextVs) =
  x `AndThen` (fmap (integral (x + v)) nextVs)


e_example :: IO ()
e_example = do

  let e = integral 1 (1 `AndThen` pure e)

  runList e


main :: IO ()
main = scary_const_example


count_example :: IO ()
count_example = runBehavior count

diagonal_example :: IO ()
diagonal_example = runBehavior (fmap now (diagonal count))

always_example :: IO ()
always_example = runBehavior always5

test_example :: IO ()
test_example = do
  event <- runNext (test 1100000)
  runEvent event

diagonal :: Behavior Next Int -> Behavior Next (Behavior Next Int)
diagonal ns = ns `AndThen` fmap diagonal (future ns)

count :: (Num a) => Behavior Next a
count = loop 0 where
  loop i = i `AndThen` pure (loop (i+1))

always5 :: Behavior Next Double
always5 = 5 `AndThen` (pure always5)


test :: Int -> Next (Event Next ())
test n = undefined
  -- fmap (\b -> pure (whenTrue ((n ==) <$> b))) count'

count' :: Next (Behavior Next Int)
count' = undefined{-loop 0 where
  loop i = do
    e <- async (return ())
    e' <- plan (loop (i + 1) <$ e)
    return (pure i `switch` e')-}

async :: IO a -> Next (Event Next a)
async action = syncIO (do
  mvar <- newEmptyMVar
  forkIO (action >>= putMVar mvar)
  let nextEvent = syncIO (do
        maybeValue <- tryTakeMVar mvar
        case maybeValue of
          Nothing -> pure (Later nextEvent)
          Just value -> pure (Occured value))
  runNext nextEvent)


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


