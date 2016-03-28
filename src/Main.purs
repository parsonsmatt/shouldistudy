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

import Grade

main ::  Eff _ Unit
main = do
  log "Hello sailor!"
  runFlareShow "input" "output" (gradeValue <$> gradeUI ex)

ui :: forall eff. Int -> UI eff Number
ui i = sum (A.replicate i (number "Grade" 0.0)) / pure (toNumber i)

actions :: forall e. UI e (Array Number -> Array Number)
actions = number "Add Assignment: " 100.0 
    <**> button "Add" (\a b -> b) A.cons

list :: forall t23. UI t23 (Array Number)
list = foldp id [100.0, 80.0] actions

uiList = (H.ul <<< foldMap (H.li <<< H.text)) <$> map (map show) list

scoreUI :: forall e. Score Number -> UI e (Score Number)
scoreUI (OutOf a b) = OutOf <$> number "Points" a <*> number "Possible" b
scoreUI (Percent n) = Percent <$> number "Percent" n
scoreUI (Average grades) = Average <$> traverse gradeUI grades

gradeUI :: forall e. Grade -> UI e Grade
gradeUI (GradeR r) = f <$> number "Weight" r.weight <*> scoreUI r.score
  where
    f n s = GradeR { weight: n, score: s }
