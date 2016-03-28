module Study.Pux where

import Batteries hiding ((#))
import Data.Array as A

import Pux
import Pux.Html as H
import Pux.Html ((#), (##))
import Pux.Html.Events as H
import Signal.Channel

import Grade

type State = GradeR Number

data Action
    = Child Int Action
    | Null

view :: State -> H.Html Action
view (GradeR grade) = do
    H.div # do
        H.span # H.text ("weight: " ++ show grade.weight)
        H.span # H.text ("score: " <> show (getScore grade.score))
        case grade.score of
             Average gs -> H.div ## do
                 (flip map) (A.zip gs (A.range 1 $ A.length gs)) \(Tuple g i) -> do
                     H.forwardTo (Child i) (view g)
             g -> H.div # H.text (show g)
  where
    bind = H.bind

init = GradeR { weight: 1.0, score: Average [] }

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: ex
        , update: fromSimple (flip const)
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html

    return unit
