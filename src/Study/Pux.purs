module Study.Pux where

import Batteries hiding ((#), State, view, attempt)
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

import Data.Tree.Zipper as TZ
import Pux.Undo as Undo
import Study.Pux.UI
import Grade
import Study.Util

update :: Undo.Action Action -> State -> State
update = Undo.update updateZoom

updateZoom :: Action -> TZ.TreeZipper (Score String) -> TZ.TreeZipper (Score String)
updateZoom (Child i ZoomIn) = attempt (TZ.down i)
updateZoom ZoomOut = attempt TZ.up
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
