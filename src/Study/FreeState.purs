module Study.FreeState where

import Prelude

import Data.Tuple (Tuple(Tuple))

import Data.Functor.Compose (Compose(Compose))
import Data.Identity (Identity(Identity))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Pair (Pair(Pair))

data Free f a
    = Pure a
    | Free (f (Free f a))

infixr 9 compose as ..

type OneOrTwo = Coproduct Pair Identity

type ArrayOf = Compose Array

type Labelled l = Compose (Tuple l)

label :: forall l f a. l -> f a -> Labelled l f a
label l f = Compose (Tuple l f)

type LabelledGrades s =
    Free (ArrayOf (Labelled s OneOrTwo))

type Grades = LabelledGrades String

grades :: forall f g a. f (g (Free (Compose f g) a)) -> Free (Compose f g) a
grades = wrap

percent :: forall f a. a -> Coproduct Pair Identity (Free f a)
percent = oneF

oneOf :: forall f a. a -> a -> Coproduct Pair Identity (Free f a)
oneOf = twoF

wrap :: forall f g a. f (g (Free (Compose f g) a)) -> Free (Compose f g) a
wrap = Free .. Compose

one :: forall a. a -> OneOrTwo a
one = right .. Identity

oneF :: forall f a. a -> Coproduct Pair Identity (Free f a)
oneF = one .. Pure

two :: forall a. a -> a -> OneOrTwo a
two a b = left (Pair a b)

twoF :: forall f a. a -> a -> OneOrTwo (Free f a)
twoF a b = map Pure (two a b)
