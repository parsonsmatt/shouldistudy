module Pureflowy.UI where

import Batteries (class Show, Tuple(Tuple), (..), ($), show, fst, map, sum, (<>), extract)

import Data.Tree as T

import Pux.Html hiding (style, map)
import Pux.Html.Attributes hiding (label)
import Pux.Html.Events (onChange, onClick)

import Pux.Undo as Undo
import Study.Util as Util

import Pureflowy.State

view :: State -> Html Action
view state = div # do
    h1 # text "Pureflowy"
    p # text "A Workflowy clone in PureScript"
    render (T.out state)

render :: T.View T.Tree String -> Html Action
render (T.View item children) =
    ul # do
        li # text item
        ul ## Util.forEachIndexed children \i c -> li # render (T.out c)
