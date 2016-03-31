module Study.Pux where

import Batteries hiding ((#), State, view)
import Data.Array as Arr
import Data.List (List(Nil), (:))
import Data.Bifunctor
import Debug.Trace as Trace

import Data.List.Zipper (Zipper(Zipper), up, down)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html)
import Pux.Html as H
import Pux.Html ((#), (##), (!))
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Signal.Channel (CHANNEL)
import Global as G

import Data.TreeZipper as TZ
import Study.Pux.UI
import Grade
import Study.Util

update :: Action -> State -> State
update Redo state = fromMaybe state $ down state
update Undo state = fromMaybe state $ up state
update act state = editToPast (updateZoom act) state

updateZoom :: Action -> TZ.TreeZipper (Score String) -> TZ.TreeZipper (Score String)
updateZoom (Child i ZoomIn) = idempotent (TZ.down i)
updateZoom ZoomOut = idempotent TZ.up
updateZoom a = TZ.editFocus (updateGrade a)

updateGrade :: Action -> Score String -> Score String
updateGrade AddGrade =
    overAverage (_ `Arr.snoc` Percent "1.0")
    .. overWeighted (_ `Arr.snoc` Tuple "1.0" (Percent "1.0")) 
updateGrade (Child i Remove) =
    overAverage (deleteAtMaybe i) .. overWeighted (deleteAtMaybe i)
updateGrade (Child i a) =
    overAverage (modMaybe i (updateGrade a)) 
    .. overWeighted (modMaybe i (map (updateGrade a)))
updateGrade (UpdateScore s) =
    const s
updateGrade (UpdateWeight i n) =
    overWeighted (modMaybe i (lmap (const n)))
updateGrade Remove =
    id
updateGrade ZoomIn = id
updateGrade ZoomOut = id
updateGrade Undo = id
updateGrade Redo = id

initialState :: State
initialState = Zipper Nil (TZ.singleton (map show ex)) Nil

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: initialState
        , update: fromSimple update
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
