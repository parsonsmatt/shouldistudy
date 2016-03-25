module Main where

import Batteries

import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.HTML.Attributes as A

import Flare
import Flare.Smolder as H

main ::  Eff _ Unit
main = do
  log "Hello sailor!"
  H.runFlareHTML "input" "output" uiList

ui :: forall eff. Int -> UI eff Number
ui i = sum (A.replicate i (number "Grade" 0.0)) / pure (toNumber i)

actions = number "Add Assignment: " 100.0 <**> button "Add" (flip const) A.cons

list = foldp id [100.0, 80.0] actions

uiList = (H.ul <<< foldMap (H.li <<< H.text)) <$> map (map show) list

average :: Array Number -> Number
average fs = if l == 0 then 0.0 else sum fs / toNumber l
  where
    l = A.length fs
