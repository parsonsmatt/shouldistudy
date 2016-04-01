module Study.Free.State where

import Prelude

import Data.Array (index)
import Data.List
import Data.Maybe
import Data.Either
import Data.Functor.Compose     (Compose(Compose), decompose)
import Data.Functor.Coproduct   (Coproduct(..), left, right)
import Data.Identity            (Identity(Identity))
import Data.Pair                (Pair(Pair))
import Data.Tuple               (Tuple(Tuple))

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

type Verbose l = Free (Compose Array (Compose (Tuple l) (Coproduct Pair Identity)))

type Grades = LabelledGrades String

data GradeView l a
    = Percent l a
    | OutOf l a a
    | Weighted l (Array (Tuple a (GradeView l a)))
    | Average l (Array (GradeView l a))

-- view :: forall a l. LabelledGrades l a -> GradeView l a
-- view (Free (Compose arr)) =  

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

data GradeZipper a
    = GradeZipper (Grades a) (Past a)

type Past a =
    List (Tuple Int (Grades a))

getGrades (GradeZipper g _) = g
getPast (GradeZipper _ p) = p

up :: forall a. GradeZipper a -> Maybe (GradeZipper a)
up (GradeZipper _ Nil) =
    Nothing
up (GradeZipper _ (Cons (Tuple _ oldTree) past)) =
    Just (GradeZipper oldTree past)

down :: forall a. Int -> GradeZipper a -> Maybe (GradeZipper a)
down n (GradeZipper g past) =
    case g of
         Free (Compose subtrees) -> do
             subtree <- index subtrees n
             case decompose subtree of
                  Tuple l (Coproduct e) ->
                      case e of
                           Left (Pair _ f@(Free _)) ->
                               Just (GradeZipper f (Tuple n g : past))
                           Right (Identity f@(Free _)) ->
                               Just (GradeZipper f (Tuple n g : past))
                           _ ->
                               Nothing
         Pure _ ->
             Nothing
