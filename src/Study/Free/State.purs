module Study.Free.State where

import Prelude

import Data.List
import Data.Maybe
import Data.Functor.Compose     (Compose(Compose), decompose)
import Data.Functor.Coproduct   (Coproduct(..), left, right)
import Data.Identity            (Identity(Identity))
import Data.Pair                (Pair(Pair))
import Data.Tuple               (Tuple(Tuple))

data Free f a
    = Pure a
    | Free (f (Free f a))

infixr 9 compose as ..

data Product f g a = Product (f a) (g a)

infixr 9 type Compose as :.:
infixr 5 type Coproduct as :+:
infixr 6 type Product as :*:


type OneOrTwo = Pair :+: Identity

type ArrayOf = Compose Array

type Labelled = Tuple String

-- This type very nearly gets us what we want! We've recovered Percent, OutOf
-- and Average from the original data constructor. We don't have Weighted,
-- yet.
asdf :: Int -> Free (ArrayOf Labelled) (OneOrTwo Int)
asdf 0 =
    wrap
        [ "What" :* pct 2
        , "Hey" :* 2 </> 3
        , "A set" :* wrap
            [ "Cool" :* one 2
            , "Yess" :* two 4 5
            ]
        ]

percent = one
pct = one
outOf = two

infix 3 outOf as </>
one = Pure <<< right <<< Identity
two a = Pure <<< left <<< Pair a

single = right <<< Identity
infixr 0 Tuple as :*

wrap = Free .. Compose

-- we obviously just need another coproduct:
type Score a =
    Free 
        (Array :.: (Tuple String) :+: (Array :.: ((Tuple a) :.: (Tuple String))))
        (OneOrTwo a)

average
    :: forall a
     . Array (Labelled (Score a))
    -> Score a
average = Free .. left .. Compose

weights :: forall a. Array (Tuple a (Labelled (Score a)))
        -> Score a
weights = Free .. right .. Compose .. map Compose

-- alright i like this a lot
qwer :: Int -> Score Int
qwer 0 =
    average 
        [ "Hello" :* pct 2
        , "Outof" :* 10 </> 12
        , "Nested" :* average []
        , "Weighted" :* weights
            [ 2 :* "Hey" :* pct 3
            , 3 :* "Yo" :* average []
            ]
        ]


