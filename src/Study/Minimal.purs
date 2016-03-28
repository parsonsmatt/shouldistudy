module Study.Minimal where

import Prelude hiding ((#))

import Pux (renderToDOM, fromSimple, start)
import Pux.Html as H
import Pux.Html ((##), (#))

type State = Unit

data Action = Null

view :: Unit -> H.Html Action
view u = H.div # do
    H.p # H.text "hello"
    H.div ## map H.text ["Hello", "World"]
  where
    bind = H.bind

initialState = unit

main = do
    app <- start
        { initialState: initialState
        , update: fromSimple (flip const)
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
