{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad


type Event = Free Maybe

never :: Event a
never = Free Nothing

occured :: a -> Event a
occured a = Pure a

later :: Event a -> Event a
later e = Free (Just e)


type Behavior = Cofree Maybe

always :: a -> Behavior a
always a = a :< Nothing

andThen :: a -> Behavior a -> Behavior a
andThen a b = a :< Just b


switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b (Free Nothing) = b
switch _ (Pure b) = b
switch (a :< Nothing) (Free (Just e)) =
    a :< Just (always a `switch` e)
switch (a :< Just b) (Free (Just e)) =
    b `switch` e


count :: Behavior Int
count = loop 0

loop :: Int -> Behavior Int
loop i = always i `switch` later (occured (loop (i+1)))


whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Nothing :< Nothing) = always never
whenJust (Just a :< Nothing) = always (occured a)
whenJust (Just a :< Just b) =
    occured a `andThen` whenJust b
whenJust (Nothing :< Just b) = later e :< Just b'
  where
    b' = whenJust b
    e :< _ = b'


runBehavior :: Behavior (Event a) -> a
runBehavior (Pure a :< _) = a
runBehavior (Free Nothing :< _) = error "loop 1"
runBehavior (Free (Just e) :< Nothing) = error "loop 2"
runBehavior (_ :< Just b) = runBehavior b


test :: Int -> Behavior (Event ())
test n = whenTrue (do
    i <- count
    return (i == n))

whenTrue :: Behavior Bool -> Behavior (Event ())
whenTrue = whenJust . fmap boolToMaybe where
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing




main :: IO ()
main = putStrLn "hi"
