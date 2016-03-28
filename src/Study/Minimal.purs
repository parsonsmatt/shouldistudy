module Study.Minimal where

import Prelude

import Data.Array as A
import Data.Tuple 
import Pux
import Pux.Html as H
import Pux.Html ((#), (##))
import Pux.Html.Events as H


data State = State Int (Array State)

data Action 
    = Increment 
    | Child Int Action

mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = map (uncurry f) (A.zip (A.range 0 (A.length xs)) xs)

forIndexed = flip mapIndexed

view :: State -> H.Html Action
view (State count substates) = do
    H.p # H.text ("Count: " <> show count)
    H.div ## forIndexed substates \i g -> 
        H.forwardTo (Child i) (view g)
  where
    bind = H.bind


initialState = State 0 []

ui = do
    app <- start
        { initialState: initialState
        , update: fromSimple (flip const)
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
