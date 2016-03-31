module Study.Pux.UI where

import Batteries (class Show, class Functor, (..), show, ($), (<>), extract)

import Data.Foldable (intercalate)
import Data.List.Zipper (Zipper(Zipper))
import Global as G

import Pux.Html hiding (style)
import Pux.Html.Attributes hiding (label)
import Pux.Html.Events (onChange, onClick)

import Grade
import Study.Util

type State = Zipper (Score String)

data BSColSize = Xs | Sm | Md | Lg

instance showBSColSize :: Show BSColSize where
    show Xs = "xs"
    show Sm = "sm"
    show Md = "md"
    show Lg = "lg"

data Action
    = Child Int Action
    | UpdateWeight Int String
    | UpdateScore (Score String)
    | Remove
    | AddGrade
    | Undo
    | Redo

view :: State -> Html Action
view = viewGrade .. extract

undoRedo :: Html Action
undoRedo =
    div ! className "col-xs-6 text-center" # do
        button' "Undo" \_ -> Undo
        button' "Redo" \_ -> Redo

div' :: forall a. BSColSize -> Int -> Array (Attribute a) -> Array (Html a) -> Html a
div' b i attrs elems =
    div ([className (intercalate "-" ["col", show b, show i])] <> attrs)
        elems

divXs :: forall a. Int -> Array (Attribute a) -> Array (Html a) -> Html a
divXs = div' Xs

actionDiv :: Score String -> Html Action
actionDiv g = div ! className "row" # do
    divXs 6 # do
        div ! className "text-center" # do
            undoRedo
    divXs 6 # do
        changeType g

button' :: String -> (_ -> Action) -> Html Action
button' t a = button ! onClick a ! className "btn btn-default" # text t

viewGrade :: Score String -> Html Action
viewGrade g@(Weighted grades) = baseDiv # do
     actionDiv g
--     button' "Add Score String" \_ -> AddGrade
--     ul ! className "list-unstyled" ## 
--        forEachIndexed grades \i (Tuple w g) ->
--            li ! className "" # do
--                label # do
--                    text "Weight: "
--                    input
--                        [ value (show w)
--                        , type_ "number"
--                        , onChange (UpdateWeight i .. targetValue)
--                        , className "form-control"
--                        ] []
--                forwardTo (Child i) (viewGrade g)
--                   
viewGrade gr@(Average grades) = baseDiv # do
    div ! className "row" # do
        div' Sm 8 # do
            h2 # text "Grade Set"
            button' "Add Grade" \_ -> AddGrade
            undoRedo
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
                    divXs 6 # do
                        changeType gr

    ul ! className "list-unstyled" ## forEachIndexed grades \i g ->
        li # forwardTo (Child i) (viewGrade g)

viewGrade g@(OutOf a b) = baseDiv # do
    gradeEdit g # do
        label # do
            text "Points: "
            input
                ! value a
                ! type_ "number"
                ! onChange (UpdateScore .. (_ `OutOf` b) .. targetValue)
                ! className "form-control"
                ## []
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
            input 
                ! value n
                ! type_ "number"
                ! onChange (UpdateScore .. Percent .. targetValue)
                ! className "form-control"
                ## []

gradeEdit g attrs elem = 
    div ! className "row" # do
        divXs 8 # do
            div attrs elem
        divXs 4 # do
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
