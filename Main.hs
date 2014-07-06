{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Graphics.Gloss
import Pipes
import Pipes.Prelude (toList)
import Data.Functor.Identity (Identity(runIdentity))
import Control.Monad (forM_,forever)

type T = Float

newtype Event a r = Event {unEvent :: Event' a r}
type Event' a r = Producer (T,a) Identity (T,r)

now :: r -> Event a r
now = Event . now'

now' :: r -> Event' a r
now' r = return (0,r)

after :: Event a r -> (r -> Event a s) -> Event a s
after e1 re2 = Event (after' (unEvent e1) (unEvent . re2))

after' :: Event' a r -> (r -> Event' a s) -> Event' a s
after' p1 rp2 = do
    (t1,r) <- p1
    (t,s) <- for (rp2 r) (\(t2,a) -> yield (t1+t2,a))
    return (t1+t,s)

delay :: T -> Event a ()
delay = Event . delay'

delay' :: T -> Event' a ()
delay' t = return (t,())

instance Monad (Event a) where
    return = now
    (>>=) = after

merge :: Event a r -> Event a r -> Event a r
merge e1 e2 = Event (merge' (unEvent e1) (unEvent e2))

merge' :: Event' a r -> Event' a r -> Event' a r
merge' p1 p2 = do
    let np1 = runIdentity (next p1)
        np2 = runIdentity (next p2)
    case np1 of
        Left (t1,r1) -> case np2 of
            Left (t2,r2) -> if t1 < t2 then return (t2,r2) else return (t1,r1)
            Right ((t2,a2),p2') -> yield (t2,a2) >> p2'
        Right ((t1,a1),p1') -> case np2 of
            Left (t2,r2) -> yield (t1,a1) >> p1'
            Right ((t2,a2),p2') -> if t1 < t2
                then yield (t1,a1) >> merge' p1' (yield (t2,a2) >> p2')
                else yield (t2,a2) >> merge' (yield (t1,a1) >> p1') p2'

whenever :: Event a r -> (a -> Event b r) -> Event b r
whenever e1 ae2 = Event (whenever' (unEvent e1) (unEvent . ae2))

whenever' :: Event' a r -> (a -> Event' b r) -> Event' b r
whenever' p1 ap2 = do
    let np1 = runIdentity (next p1)
    case np1 of
        Left (t1,r1) -> return (t1,r1)
        Right ((t1,a1),p1') -> merge' (delay' t1 >> ap2 a1) (whenever' p1' ap2)

fire :: a -> Event a ()
fire = Event . fire'

fire' :: a -> Event' a ()
fire' a = yield (0,a) >> return (0,())

run :: Event Picture r -> IO ()
run e = animate
    (InWindow "animations" (600,600) (10,10))
    white
    (\t -> lastBefore t (occurences e))

lastBefore :: T -> [(T,Picture)] -> Picture
lastBefore _ [] = text "The End"
lastBefore _ [(_,a)] = a
lastBefore t1 ((_,a):(t2,a2):rest) = if t1 < t2 then a else lastBefore t1 ((t2,a2):rest)

occurences :: Event a r -> [(T,a)]
occurences = occurences' . unEvent

occurences' :: Event' a r -> [(T,a)]
occurences' e = toList (e >> return ())

animation :: Event Picture ()
animation = whenever (fire 10 >> delay 2 >> fire 100 >> delay 2) (\r -> fire (circle r))

test :: Event Int ()
test = do
    fire ( 50)
    delay 2
    fire ( 100)
    delay 2

main :: IO ()
main = run animation

