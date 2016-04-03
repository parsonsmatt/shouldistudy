module Pureflowy.State where

import Batteries

import Data.Array (snoc, (!!))
import Data.Tree
import Data.List

import Study.Util as Util

type State = Tree String

type ZoomState =
    { past :: List (Tuple Int State)
    , current :: State
    }

data ZoomAct = ZoomIn Int | ZoomOut | Next Action

updateZoom :: ZoomAct -> ZoomState -> ZoomState
updateZoom ZoomOut {past, current} =
    case past of
         Nil -> {past, current}
         Cons (Tuple _ p) ps -> {past: ps, current: p}
updateZoom (ZoomIn i) { past, current } =
    case out current of
         View a subtrees -> 
            case subtrees !! i of
                 Nothing -> { past, current }
                 Just s -> { past: Tuple i current : past, current: s }
updateZoom (Next a) {past, current} =
    { past, current: update a current }

data Action 
    = Noop
    | Add
    | Child Int Action
    | Update String

update :: Action -> State -> State
update action state =
    case out state of
         View c st ->
             into case action of
                       Child i a -> View c (Util.modMaybe i (update a) st)
                       Add -> View c (st `snoc` (into (View "" [])))
                       Update s -> View s st
                       
initialState :: State
initialState = into $ View "Todo" [into $ View "List" [], into $ View "Work" []]
