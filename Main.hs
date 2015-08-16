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


whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Nothing :< Nothing) = always never
whenJust (Just a :< _) = always (occured a)
whenJust (Nothing :< Just b) = later e :< Just b'
  where
    b' = whenJust b
    e :< _ = b'


async :: IO a -> Behavior (Event a)
async = undefined

lastLine :: Behavior String
lastLine = loop "first line"
  where
    loop l = do
        nextLine <- async getLine
        always l `switch` fmap loop nextLine

sync :: IO a -> Behavior a
sync = undefined

putChanges :: Behavior String -> Behavior ()
putChanges b = do
    s <- b
    sync (putStrLn s)


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

count :: Behavior Int
count = loop 0 where
    loop i = i `andThen` loop (i+1)



main :: IO ()
main = putStrLn "hi"
