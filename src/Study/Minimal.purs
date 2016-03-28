module Study.Minimal where

import Prelude (Unit, const, flip, unit, bind)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html as H
import Pux.Html ((#))

data Action = Null

view :: Int -> Unit -> H.Html Action
view i _ = H.p # case i of
    0 -> do
        H.text "hello"
    1 ->
        H.text "Hello"
    2 -> H.div # do
        H.text "hello"
    3 -> H.div #
        H.text "hello"
  where
    bind = H.bind

main = do
    app <- start
        { initialState: unit
        , update: fromSimple (flip const)
        , inputs: []
        , view: view 0
        }

    renderToDOM "#app" app.html
