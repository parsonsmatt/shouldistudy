module Pureflowy.State where

import Batteries

import Data.Tree

type State = Tree String

data Action = Noop

update :: Action -> State -> State
update _ = id

initialState :: State
initialState = into $ View "Todo" [into $ View "List" [], into $ View "Work" []]
