{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Main where

import Graphics.Gloss
import Pipes
import Data.Functor.Identity (Identity(runIdentity))

type T = Rational

type Event a r = Producer (T,a) Identity (T,r)

now :: r -> Event a r
now r = return (0,r)

after :: Event a r -> (r -> Event a s) -> Event a s
after p1 rp2 = do
    (t1,r) <- p1
    for (rp2 r) (\(t2,a) -> yield (t1+t2,a))

delay :: T -> Event a ()
delay t = return (t,())

merge :: Event a r -> Event a r -> Event a r
merge p1 p2 = do
    let np1 = runIdentity (next p1)
        np2 = runIdentity (next p2)
    case np1 of
        Left (t1,r1) -> case np2 of
            Left (t2,r2) -> if t1 < t2 then return (t2,r2) else return (t1,r1)
            Right ((t2,a2),p2') -> yield (t2,a2) >> p2'
        Right ((t1,a1),p1') -> case np2 of
            Left (t2,r2) -> yield (t1,a1) >> p1'
            Right ((t2,a2),p2') -> if t1 < t2
                then yield (t1,a1) >> merge p1' (yield (t2,a2) >> p2')
                else yield (t2,a2) >> merge (yield (t1,a1) >> p1') p2'

whenever :: Event a r -> (a -> Event b r) -> Event b r
whenever p1 ap2 = do
    let np1 = runIdentity (next p1)
    case np1 of
        Left (t1,r1) -> return (t1,r1)
        Right ((t1,a1),p1') -> merge (delay t1 >> ap2 a1) (whenever p1' ap2)

fire :: a -> Event a ()
fire a = yield (0,a) >> return (0,())

main :: IO ()
main = print "hallo"

