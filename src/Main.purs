module Main where

import Batteries

import Control.Monad.Eff (Eff)
import Signal.Channel (CHANNEL)

import Study.Pux as Pux
import Pureflowy as PF

main :: forall eff. Eff (err :: EXCEPTION, channel :: CHANNEL | eff) Unit
main = PF.ui
