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
