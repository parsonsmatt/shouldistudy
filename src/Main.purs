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

gradeUI :: forall e. Grade -> UI e Grade
gradeUI (GradeR r) = f <$> number "Weight" r.weight <*> scoreUI r.score
  where
    f n s = GradeR { weight: n, score: s }

scoreUI :: forall e. Score Number -> UI e (Score Number)
scoreUI (OutOf a b) = OutOf <$> number "Points" a <*> number "Possible" b
scoreUI (Percent n) = Percent <$> number "Percent" n
scoreUI (Average grades) = Average <$> traverse gradeUI grades
