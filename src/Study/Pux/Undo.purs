module Study.Pux.Undo where

import Prelude

import Pux.Html as H
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Data.List.Zipper

import Study.Util

data Action n 
    = Undo
    | Redo
    | Next n

update :: forall a n. (n -> a -> a) -> Action n -> Zipper a -> Zipper a
update f Redo     = attempt down
update f Undo     = attempt up
update f (Next n) = editToPast (f n)

view :: forall n. H.Html n -> H.Html (Action n)
view c = 
     H.div [] [ H.button [E.onClick (\_ -> Undo)] [H.text "Undo"]
              , H.button [E.onClick (\_ -> Redo)] [H.text "Redo"]
              , H.forwardTo Next c
              ]
