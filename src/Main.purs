module Main where

import Batteries

import Control.Monad.Eff (Eff)

import Study.Pux as Pux

main :: Eff _ Unit
main = Pux.ui
