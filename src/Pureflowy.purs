module Pureflowy where

import Batteries hiding ((#), State, view, attempt)
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

import Data.Tree.Zipper as TZ
import Pux.Undo as Undo

import Pureflowy.State (State)
import Pureflowy.UI

-- ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
-- ui = do
--     app <- start
--         { initialState: initialState
--         , update: fromSimple update
--         , inputs: []
--         , view: view
--         }
-- 
--     renderToDOM "#app" app.html
-- 
