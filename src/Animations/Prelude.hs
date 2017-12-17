module Animations.Prelude where


import Animations.Behavior
import Animations.Event

import Control.Applicative


never :: (Applicative next) => Event next a
never = Later (pure never)

switch :: (Applicative next) => Behavior next a -> Event next (Behavior next a) -> Behavior next a
switch _ (Occured behavior') =
  behavior'
switch (a `AndThen` nextBehavior) (Later nextEvent) =
  a `AndThen` liftA2 switch nextBehavior nextEvent

whenJust :: (Functor next) => Behavior next (Maybe a) -> Event next a
whenJust (Just a `AndThen` _) =
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

countFrom :: (Applicative next) => Int -> Behavior next Int
countFrom i = i `AndThen` pure (countFrom (i + 1))

always :: (Applicative next) => a -> Behavior next a
always a = a `AndThen` pure (always a)

unfold :: (Functor next) => (a -> next a) -> a -> Behavior next a
unfold f a = a `AndThen` (fmap (unfold f) (f a))



