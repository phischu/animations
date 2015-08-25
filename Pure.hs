{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Pure where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad
import Control.Applicative


type Event = Free Maybe

never :: Event a
never = Free Nothing

occured :: a -> Event a
occured a = Pure a

later :: Event a -> Event a
later e = Free (Just e)


type Behavior = Cofree Maybe

now :: Behavior a -> a
now (a :< _) = a

always :: a -> Behavior a
always a = a :< Nothing

andThen :: a -> Behavior a -> Behavior a
andThen a b = a :< Just b


switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch _ (Pure b) = b
switch (a :< b) (Free e) = a :< liftM2 switch b' e where
    b' = b <|> return (always a)

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Just a :< _) = always (occured a)
whenJust (Nothing :< b) = Free e :< b' where
    e = fmap now b'
    b' = fmap whenJust b

runBehavior :: Behavior (Event a) -> a
runBehavior (Pure a :< _) = a
runBehavior (Free Nothing :< _) = error "loop 1"
runBehavior (Free (Just e) :< Nothing) = error "loop 2"
runBehavior (_ :< Just b) = runBehavior b


test :: Int -> Behavior (Event ())
test n = whenTrue (do
    i <- count
    return (i == n))

count :: Behavior Int
count = loop 0

loop :: Int -> Behavior Int
loop i = always i `switch` later (occured (loop (i+1)))

whenTrue :: Behavior Bool -> Behavior (Event ())
whenTrue = whenJust . fmap boolToMaybe where
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing

p :: Behavior (Int,Int)
p = do
    x <- always 5
    y <- count
    return (x,y)


main :: IO ()
main = putStrLn "hi"