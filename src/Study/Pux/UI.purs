module Study.Pux.UI where

import Batteries (Tuple(Tuple), (..), ($), show, fst, map, sum, (<>))

import Data.Foldable (intercalate)
import Global as G

import Pux.Html hiding (style, map)
import Pux.Html.Attributes hiding (label)
import Pux.Html.Events (onChange, onClick)

import Pux.Bootstrap (ColSize(..))
import Pux.Undo as Undo
import Grade
import Study.Util

type State = Undo.History (Score String)

data Action
    = Child Int Action
    | UpdateWeight Int String
    | UpdateScore (Score String)
    | Remove
    | AddGrade
    | ZoomIn
    | ZoomOut

view :: State -> Html (Undo.Action Action)
view = Undo.simpleView $ \t -> div # do
    button' "Zoom Out" \_ -> ZoomOut
    viewGrade t

div' :: forall a. ColSize -> Int -> Array (Attribute a) -> Array (Html a) -> Html a
div' b i attrs elems =
    div ([className (intercalate "-" ["col", show b, show i])] <> attrs)
        elems

actionDiv :: Score String -> Html Action
actionDiv g = div ! className "row" # do
    div' Xs 6 # do
        div ! className "text-center" # do
            text "no more undo here"
    div' Xs 6 # do
        changeType g

button' :: String -> (_ -> Action) -> Html Action
button' t a = button ! onClick a ! className "btn btn-default" # text t

zoomIn i = button' "Focus" \_ -> ZoomIn

viewGrade :: Score String -> Html Action
viewGrade g@(Weighted grades) = baseDiv # do
    setHeader g

    h2 # do
        let str = ("Total Weight " <>) .. show .. sum .. map (G.readFloat .. fst) $ grades
        text str

    ul ! className "list-unstyled" ## 
       forEachIndexed grades \i (Tuple w g) ->
           li # do
               div ! className "container-fluid" # do
                   div ! className "row" # do
                       div' Xs 2 # do
                           label # do
                               text "Weight: "
                               input
                                   ! value w
                                   ! type_ "number"
                                   ! onChange (UpdateWeight i .. targetValue)
                                   ! className "form-control"
                                   ## []
                       div' Xs 10 # do 
                           zoomIn i
                           forwardTo (Child i) (viewGrade g)
                  
viewGrade gr@(Average grades) = baseDiv # do
    setHeader gr

    ul ! className "list-unstyled" ## forEachIndexed grades \i g ->
        li # do
            zoomIn i
            forwardTo (Child i) (viewGrade g)

viewGrade g@(OutOf a b) = baseDiv # do
    gradeEdit g ! className "container-fluid" # do
        div ! className "row" # do
            div' Xs 6 # do
                label # do
                    text "Points: "
                    input
                        ! value a
                        ! type_ "number"
                        ! onChange (UpdateScore .. (_ `OutOf` b) .. targetValue)
                        ! className "form-control"
                        ## []
            div' Xs 6 # do
                label # do
                    text "Out of: "
                    input
                        ! value b
                        ! type_ "number"
                        ! onChange (UpdateScore .. OutOf a .. targetValue)
                        ! className "form-control"
                        ## []

viewGrade g@(Percent n) = baseDiv # do
    gradeEdit g # do
        label # do
            text "Percent: "
            div ! className "input-group" # do
                input 
                    ! value n
                    ! type_ "number"
                    ! onChange (UpdateScore .. Percent .. targetValue)
                    ! className " form-control"
                    ## []
                div ! className "input-group-addon" # do
                    text "%"

setHeader gr = 
    div ! className "row" # do
        div' Sm 8 # do
            h2 # text "Grade Set"
            button' "Add Grade" \_ -> AddGrade
            button' "Remove" \_ -> Remove
        div' Sm 4 # do
            div ! className "container-fluid" # do
                div ! className "row alert alert-info text-center" # do
                    div' Xs 6 # do
                        h4 # do
                            text "Score: "
                        h4 # do
                            text .. show 
                                 .. getScore 
                                 .. Prelude.map G.readFloat 
                                 $ gr
                    div' Xs 6 # do
                        changeType gr

gradeEdit :: _
gradeEdit g attrs elem = 
    div ! className "row" # do
        div' Xs 6 # do
            div attrs elem
        div' Xs 6 # do
            div ! className "text-center" # do
                changeType g
                button' "Remove" \_ -> Remove

baseDiv :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
baseDiv attrs elems =
    div ! className "container-fluid" # do
        div attrs elems

changeType :: Score String -> Html Action
changeType grade =
    label # do
        text "Change Type: "
        select
            ! value (scoreToLabel grade)
            ! onChange (UpdateScore .. emptyScore .. targetValue) 
            ! className "form-control"
            ## forEach ["OutOf", "Percent", "Average", "Weighted"] \label ->
                   option ! value label # text label
