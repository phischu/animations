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

data Behavior next a = AndThen {
    now :: a,
    future :: next (Behavior next a)}


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

instance Functor List where
  fmap f (Cons a as) = Cons (f a) (fmap (fmap f) as)

instance (Functor next) => Functor (Behavior next) where
  fmap f (a `AndThen` as) = f a `AndThen` fmap (fmap f) as

instance Applicative List where
  pure a = Cons a (pure (pure a))
  (Cons f fs) <*> (Cons a as) = Cons (f a) (liftA2 (<*>) fs as)

instance (Applicative next) => Applicative (Behavior next) where
    pure a = a `AndThen` pure (pure a)
    (f `AndThen` mf) <*> (x `AndThen` mx) = f x `AndThen` (liftA2 (<*>) mf mx)

runNext :: Next a -> IO a
runNext (Next action) = getCopy action

syncIO :: IO a -> Next a
syncIO action = Next (pure action)

data List a = Cons a (Next (List a))

runList :: (Show a) => List a -> IO a
runList (Cons a as) = do
  runNext as >>= runList
  -- runList (getCopy as)
  -- runNext as >>= runList

runC :: Behavior Copy a -> IO ()
runC (_ `AndThen` as) = do
  runC (getCopy as)

runI :: (Show a) => Behavior Identity a -> IO ()
runI (a `AndThen` as) = do
  runI (runIdentity as)

runBehavior :: (Show a) => Behavior Next a -> IO ()
runBehavior (a `AndThen` nextBehavior) = do
  -- print a
  behavior' <- runNext nextBehavior
  runBehavior behavior'

main :: IO ()
main = e_example

list_example :: IO ()
list_example = do
  putStrLn "starting"
  let z = repeat' 0
      l = liftA2 (,) (fmap head' (repeat' z)) z
  runList l
  putStrLn "ending"

bad_const_example :: IO ()
bad_const_example = do
  putStrLn "Starting"
  let count = bad_const 0
      b = liftA2 (,) (fmap now (bad_const count)) count
  runBehavior b
  putStrLn "ending"

e_example :: IO ()
e_example = do
  putStrLn "Starting"
  let z = bad_const 0
      e = integral 1 z
  runBehavior e
  putStrLn "Ending"

repeat' :: a -> List a
repeat' a = Cons a (pure (repeat' a))

bad_const :: (Applicative next) => a -> Behavior next a
bad_const a = AndThen a (pure (bad_const a))


data Copy a = Copy a

getCopy :: Copy a -> a
getCopy (Copy a) = case dup a of Box a' -> a'

instance Functor Copy where
  fmap f a = Copy (f (getCopy a))

instance Applicative Copy where
  pure a = Copy a
  f <*> a = Copy ((getCopy f) (getCopy a))



zip' :: List a -> List b -> List (a, b)
zip' (Cons a as) (Cons b bs) = Cons (a, b) (liftA2 zip' as bs)

map' :: (a -> b) -> List a -> List b
map' f (Cons a as) = Cons (f a) (fmap (map' f) as)

head' :: List a -> a
head' (Cons a _) = a

count_example :: IO ()
count_example = runBehavior count

diagonal_example :: IO ()
diagonal_example = runBehavior (fmap now (diagonal count))

integral_example :: IO ()
integral_example = runBehavior (integral 0 count)

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

integral :: (Functor next) => Double -> Behavior next Double -> Behavior next Double
integral position (velocity `AndThen` futureVelocities) =
  position `AndThen` fmap (integral (position + velocity)) futureVelocities

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


