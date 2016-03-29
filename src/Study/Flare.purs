module Study.Flare where

import Batteries

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Flare (UI, number, runFlareShow)
import Signal.Channel (CHANNEL)

import Grade

ui :: forall e. Eff (channel :: CHANNEL, dom :: DOM | e) Unit
ui = runFlareShow "input" "output" (getScore <$> scoreUI ex)

scoreUI :: forall e. Score Number -> UI e (Score Number)
scoreUI (OutOf a b) = OutOf <$> number "Points" a <*> number "Possible" b
scoreUI (Percent n) = Percent <$> number "Percent" n
scoreUI (Average grades) = Average <$> traverse scoreUI grades
scoreUI (Weighted grades) = Weighted <$> for grades 
    (\(Tuple weight score) ->
        Tuple <$> number "Weight:" weight <*> scoreUI score
    )
