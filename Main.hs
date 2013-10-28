{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where

import Graphics.Gloss

type T = Float

data Clip a where
    Clip :: T -> (T -> a) -> Clip a
    (:>>:) :: Clip a -> Clip a -> Clip a
    (:==:) :: Clip a -> Clip b -> Clip (a,b)

dur :: Clip a -> T
dur (Clip l _) = l
dur (c :>>: d)  = dur c + dur d
dur (c :==: d)  = dur c `min` dur d

now :: Clip a -> a
now (Clip _ f) = f 0
now (c :>>: _) = now c
now (c :==: d) = (now c , now d)

end :: Clip a -> a
end (Clip l f) = f l
end (_ :>>: d) = end d
end (c :==: d)
    | dur c <  dur d = (end c , at (dur c) d)
    | dur c == dur d = (end c , end d)
    | dur c >  dur d = (at (dur d) c , end d)

at :: T -> Clip a -> a
at t (Clip l f) = f (min (max 0 t) l)
at t (c :>>: d)
    | dur c > t = at t c
    | otherwise = at (t - dur c) d
at t (c :==: d) = (at t c , at t d)

run :: Clip Picture -> IO ()
run c = animate (InWindow "Animation" (300,300) (600,600)) white (flip at c)

test :: Clip Picture
test = Clip 2 (\t -> circle (10*t))

main :: IO ()
main = run test


