module Study.Flare where

import Batteries (Unit, traverse, (<$>), (<*>))

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Flare (UI, number, runFlareShow)
import Signal.Channel (CHANNEL)

import Grade

ui :: forall e. Eff (channel :: CHANNEL, dom :: DOM | e) Unit
ui = runFlareShow "input" "output" (gradeValue <$> gradeUI ex)

gradeUI :: forall e. Grade -> UI e Grade
gradeUI (GradeR r) = mkGrade <$> number "Weight" r.weight <*> scoreUI r.score

scoreUI :: forall e. Score Number -> UI e (Score Number)
scoreUI (OutOf a b) = OutOf <$> number "Points" a <*> number "Possible" b
scoreUI (Percent n) = Percent <$> number "Percent" n
scoreUI (Average grades) = Average <$> traverse gradeUI grades
