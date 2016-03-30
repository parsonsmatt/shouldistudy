module Study.Pux.UI where

import Batteries ((..), const, show, (<>), extract, ($))

import Data.Tuple (Tuple(Tuple))
import Data.List.Zipper (Zipper(Zipper))

import Pux.Html hiding (style)
import Pux.Html.Attributes hiding (label)
import Pux.Html.Events (onChange, onClick)

import Grade
import Study.Util

type State = Zipper Grade

data Action
    = Child Int Action
    | UpdateWeight Int Number
    | UpdateScore (Score Number)
    | AddGrade
    | Undo
    | Redo

view :: State -> Html Action
view = viewGrade .. extract

actionDiv :: Grade -> Html Action
actionDiv g = div ! className "row" # do
    div ! className "col-xs-1" # do
        div ! className "row" # do
            button' "Undo" \_ -> Undo
        div ! className "row" # do
            button' "Redo" \_ -> Redo
    div ! className "col-xs-11" # do
        changeType g

button' t a = button ! onClick a ! className "btn btn-default" # text t

title :: Grade -> String -> Html Action
title g t = 
    div ! className "title" # do
        div ! className "col-sm-8" # do
            h2 # text t
        div ! className "col-sm-4" # do
            div ! className "alert alert-info" # do
                text .. show .. getScore $ g

title' a g =
    div ! className "title" # do
        div ! className "col-sm-8" # do
            a
        div ! className "col-sm-4" # do
            div ! className "alert alert-info" # do
                text .. show .. getScore $ g

viewGrade :: Grade -> Html Action
viewGrade g@(Weighted grades) = div # do
    title g "Weighted Grade Set"
    actionDiv g
    button' "Add Grade" \_ -> AddGrade
    ul ! className "" ## 
       forEachIndexed grades \i (Tuple w g) ->
           li ! className "" # do
               label # do
                   text "Weight: "
                   input
                       [ value (show w)
                       , type_ "number"
                       , onChange (UpdateWeight i .. eventNumber)
                       , className "form-control"
                       ] []
               forwardTo (Child i) (viewGrade g)
                  
viewGrade g@(Average grades) = div ! className "average" # do
    title g "Grade Set"
    actionDiv g
    button' "Add Grade" \_ -> AddGrade
    ul ! className "" ## forEachIndexed grades \i g ->
        li ! className "" # forwardTo (Child i) (viewGrade g)

viewGrade g@(OutOf a b) = div ! className "form-inline" # do
    label # do
        text "Points: "
        input
            [ value (show a)
            , type_ "number"
            , onChange (UpdateScore .. (_ `OutOf` b) .. eventNumber)
            , className "form-control"
            ] []
    label # do
        text "Out of: "
        input
            [ value (show b)
            , type_ "number"
            , onChange (UpdateScore .. OutOf a .. eventNumber)
            , className "form-control"
            ] []
    title g ""
    actionDiv g

viewGrade g@(Percent n) = div ! className "form-inline" # do
    let a = label # do
                text "Percent: "
                input 
                    [ value (show n)
                    , type_ "number"
                    , onChange (UpdateScore .. Percent .. eventNumber)
                    , className "form-control"
                    ] []
    title' a g 


changeType :: Grade -> Html Action
changeType grade = label # do
    text "Change Type: "
    select
        ! value (scoreToLabel grade)
        ! onChange (UpdateScore .. emptyScore .. targetValue) 
        ! className "form-control"
        ## forEach ["OutOf", "Percent", "Average", "Weighted"] \label ->
               option ! value label # text label
