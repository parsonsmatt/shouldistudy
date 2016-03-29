module Study.Minimal where

import Prelude (Unit, bind, const, show, (++), (-), (+))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, (#), button, (!), span, div)
import Pux.Html.Events (onClick)

data Action = Increment | Decrement

type State = Int

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: State -> Html Action
view count =
  div # do
    button ! onClick (const Increment) # text "Increment"
    span # text ("Counter: " ++ show count)
    button ! onClick (const Decrement) # text "Decrement"
  where bind = Pux.Html.bind

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: 0
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
