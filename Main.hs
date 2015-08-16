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
    always a `switch` e
switch (a :< Just b) (Free (Just e)) =
    b `switch` e


main :: IO ()
main = putStrLn "hi"
