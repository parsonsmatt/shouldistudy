module Main where

import Batteries

import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Study.Pux as Pux

import Grade

main :: Eff _ Unit
main = Pux.ui
