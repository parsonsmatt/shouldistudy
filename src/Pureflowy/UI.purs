module Pureflowy.UI where

import Batteries (class Show, Tuple(Tuple), (..), ($), show, fst, map, sum, (<>), extract)

import Data.Foldable (intercalate)
import Global as G

import Pux.Html hiding (style, map)
import Pux.Html.Attributes hiding (label)
import Pux.Html.Events (onChange, onClick)

import Pux.Undo as Undo
import Grade
import Study.Util
import Data.Tree.Zipper (TreeZipper, getTree, extractTree)

