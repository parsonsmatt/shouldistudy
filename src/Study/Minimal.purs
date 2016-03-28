module Study.Minimal where

import Prelude (Unit, const, flip, bind, unit)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html as H
import Pux.Html ((##))
import Pux.Html.Events as H

type State = Unit

data Action = Null

view :: Unit -> H.Html Action
view u =
    H.div ## [H.text "Hello", H.text "World"]

initialState = unit

main = do
    app <- start
        { initialState: initialState
        , update: fromSimple (flip const)
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
