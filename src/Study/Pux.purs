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

import Study.Pux.UI
import Grade
import Study.Util

update :: Action -> State -> State
update Redo state = fromMaybe state $ down state
update Undo state = fromMaybe state $ up state
update act state = editToPast (updateGrade act) state

updateGrade :: Action -> Grade -> Grade
updateGrade AddGrade =
    overAverage (_ `Arr.snoc` Percent 1.0)
    .. overWeighted (_ `Arr.snoc` Tuple 1.0 (Percent 1.0)) 
updateGrade (Child i a) =
    overAverage (modMaybe i (updateGrade a)) 
    .. overWeighted (modMaybe i (map (updateGrade a)))
updateGrade (UpdateScore s) =
    const s
updateGrade (UpdateWeight i n) =
    overWeighted (modMaybe i (lmap (const n)))
updateGrade Undo = id
updateGrade Redo = id

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: Zipper Nil ex Nil
        , update: fromSimple update
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
