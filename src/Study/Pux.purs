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

viewG :: State -> H.Html Action
viewG (GradeR grade) = do
    H.p # H.text "Grade Set:"
    H.div # do
        H.p # H.text ("weight: " ++ show grade.weight)
        H.p # H.text ("score: " <> show (getScore grade.score))
        case grade.score of
             Average gs -> H.div ## do
                 mapIndexed (\i g -> H.forwardTo (Child i) (viewG g)) gs
             g -> H.div # H.text (show g)
  where
    bind = H.bind

mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = map (uncurry f) (A.zip (A.range 0 (A.length xs)) xs)

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: ex
        , update: fromSimple (flip const)
        , inputs: []
        , view: viewG
        }

    renderToDOM "#app" app.html

    return unit
