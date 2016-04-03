module Pureflowy.UI where

import Batteries (class Show, Tuple(Tuple), (..), ($), show, fst, map, sum, (<>), extract)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Tree as T

import Data.Array ((!!))

import Data.Int as Int
import Pux.Html hiding (style, map)
import Pux.Html.Attributes hiding (label, form)
import Pux.Html.Events (onChange, onClick, onSubmit)
import Global as G

import Pux.Undo as Undo
import Study.Util as Util

import Pureflowy.State

zoomView :: ZoomState -> Html ZoomAct
zoomView { past, current } = div # do
    button ! onClick (\_ -> ZoomOut) # text "out"
    form ! onSubmit handleSubmit # do
        input [] []
        button # text "in"
    forwardTo Next (view current)
  where
    handleSubmit o = ZoomIn .. Int.ceil .. G.readFloat $
      case (unsafeCoerce (unsafeCoerce o.target)) !! 0 of
           Nothing -> "0"
           Just o -> o.value
view :: State -> Html Action
view state = div # do
    h1 # text "Pureflowy"
    p # text "A Workflowy clone in PureScript"
    render (T.out state)

render :: T.View T.Tree String -> Html Action
render (T.View item children) =
    ul # do
        li # do
            button ! onClick (\_ -> Add) # text "Add"
            input ! onChange (\o -> Update o.target.value)
                  ! value item
                  ## []
        ul ## Util.forEachIndexed children \i c ->
            forwardTo (Child i) (li # render (T.out c))
