module Study.Pux.Undo where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.List ((:))
import Pux.Html as H
import Pux.Html.Events as E
import Data.List.Zipper (Zipper(Zipper), up, down)

data Action n 
    = Undo
    | Redo
    | Next n

type History = Zipper

attempt :: forall a. (a -> Maybe a) -> a -> a
attempt f a = fromMaybe a (f a)

editToPast :: forall a. (a -> a) -> Zipper a -> Zipper a
editToPast g (Zipper p a f) = Zipper (a : p) (g a) f

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
