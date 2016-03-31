module Data.TreeZipper where

import Prelude

import Data.Array hiding ((:))
import Data.Maybe
import Data.Tree
import Data.List hiding (index)
import Data.Tuple

type Past a 
    = List (Tuple Int (Tree a))

data TreeZipper a
    = TreeZipper (Tree a) (Past a)

getTree :: forall a. TreeZipper a -> Tree a
getTree (TreeZipper s _) = s

getPast :: forall a. TreeZipper a -> Past a
getPast (TreeZipper _ p) = p

extractTree :: forall a. Tree a -> a
extractTree t =
    case out t of
         View a _ -> a

instance functorTreeZipper :: Functor TreeZipper where
    map f (TreeZipper tree past) =
        TreeZipper (map f tree) (map (map (map f)) past)

up :: forall a. TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper _ Nil) =
    Nothing
up (TreeZipper tree (Cons (Tuple _ oldTree) past)) =
    Just (TreeZipper oldTree past)

down :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
down i (TreeZipper t past) =
    case out t of
         View a subtrees ->
             TreeZipper <$> index subtrees i <*> pure (Tuple i t : past)

editTree :: forall a. (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
editTree f (TreeZipper t p) = TreeZipper (f t) p

editFocus :: forall a. (a -> a) -> TreeZipper a -> TreeZipper a
editFocus f = editTree g
  where
    g t = case out t of
               View a st ->
                   into (View (f a) st)

singleton :: forall a. a -> TreeZipper a
singleton a = TreeZipper (into (View a [])) Nil
