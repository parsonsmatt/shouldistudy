module Recursive.Form where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List as L
import Data.List (List(..))
import Data.Array as Arr
import Control.Comonad.Cofree (Cofree, tail, head, mkCofree)

type Workflowy = Cofree Array String

ex1 :: Workflowy
ex1 = mkCofree "Item" [mkCofree "Todo" [], mkCofree "Asdf" []]

type Index r i a = r a -> i -> Maybe a

arrIndex :: forall a. Index Array Int a
arrIndex = Arr.index

listIndex :: forall a. Index List Unit a
listIndex = const <<< L.head

maybeIndex :: forall a. Index Maybe Unit a
maybeIndex = const

type RecursiveForm r i a =
    { index :: Index r i (Cofree r a)
    , state :: Cofree r a
    , past :: List (Tuple i (Cofree r a))
    }

type WorkForm = RecursiveForm Array Int String

form :: WorkForm
form = { index: arrIndex, state: ex1, past: Nil }

type OneOrZero = RecursiveForm Maybe Unit String

ooZex :: OneOrZero
ooZex = { index: \m _ -> m
        , state: mkCofree "Just" (Just (mkCofree "One" Nothing))
        , past: Nil
        }

zoomIn :: forall r i a. i -> RecursiveForm r i a -> Maybe (RecursiveForm r i a)
zoomIn i {index, state, past} =
    map f (index cs i)
  where
    f np = {index, state: np, past: Cons (Tuple i (mkCofree s cs)) past}
    s = head state
    cs = tail state

zoomOut :: forall r i a. RecursiveForm r i a -> Maybe (RecursiveForm r i a)
zoomOut {index, state, past} =
    (\{head: Tuple i p, tail} -> {index, state: p, past: tail}) <$> L.uncons past
