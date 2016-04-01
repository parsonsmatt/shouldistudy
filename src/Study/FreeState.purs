module Study.FreeState where

import Prelude

import Data.Tuple

import Data.Functor.Compose
import Data.Identity
import Data.Functor.Coproduct
import Data.Tree
import Data.Pair

data Free f a
    = Pure a
    | Free (f (Free f a))

infixr 9 compose as ..

type OneOrTwo = Coproduct Pair Identity

type Grades =
    Free (Compose Array OneOrTwo)

type ArrayOf = Compose Array

type Labelled l = Compose (Tuple l)

label :: forall l f a. l -> f a -> Labelled l f a
label l f = Compose (Tuple l f)

type GradesWithWeight w =
    Free (Compose (Labelled w Array) OneOrTwo)

type GradesWithName s =
    Free (ArrayOf (Labelled s OneOrTwo))

gwn :: Int -> GradesWithName String Int
gwn 0 = wrap [label "hello" (oneF 1)]
gwn 1 = wrap [label "hi" (twoF 1 2)]
gwn 2 =
    wrap 
        [ label "Exams" (two (pure 1) exams)
        , label "Homework" (one homework)
        , label "Projects" (twoF 2 [ label "asdf" $ oneF 1.0])
        ]

exams :: GradesWithName String Int
exams = wrap [ label "Exam 1" (oneF 100)
             , label "Exam 2" (oneF 95)
             ]

gradeSet = wrap

homework :: GradesWithName String Int
homework =
    wrap
        [ label "Thing" (oneF 80)
        , label "what" (oneF 90)
        ]

wrap :: forall a g f. f (g a) -> Free (Compose f g) a
wrap = Free .. Compose

one :: forall a. a -> OneOrTwo a
one = right .. Identity

oneF :: forall a f. a -> Coproduct Pair Identity (Free f a)
oneF = one .. Pure

two :: forall a. a -> a -> OneOrTwo a
two a b = left (Pair a b)

twoF :: forall a f. a -> a -> OneOrTwo (Free f a)
twoF a b = map Pure (two a b)
