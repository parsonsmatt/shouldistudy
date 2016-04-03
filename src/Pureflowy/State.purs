module Pureflowy.State where

import Batteries

import Data.Array (snoc)
import Data.Tree

import Study.Util as Util

type State = Tree String

data Action 
    = Noop
    | Add
    | Child Int Action

update :: Action -> State -> State
update Noop state = state
update (Child i a) state =
    case out state of
         View c st ->
            into $
                View c $
                    Util.modMaybe i (update a) st 
update Add state =
    case out state of
         View c st -> into (View c (st `snoc` (into $ View "" [])))

initialState :: State
initialState = into $ View "Todo" [into $ View "List" [], into $ View "Work" []]
