module Study.Pux where

import Batteries hiding ((#))
import Data.Array as A
import Data.Int as I

import Pux
import Pux.Html as H
import Pux.Html ((#), (##), (!))
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Signal.Channel
import Global as G

import Grade

type State = GradeR Number

data Action
    = Child Int Action
    | UpdateWeight Number
    | UpdateScore (Score Number)
    | AddGrade

infixr 9 compose as ..

view :: State -> H.Html Action
view (GradeR grade) = H.div # do
    H.p # H.text "Grade Set:"
    H.ul # do
        H.li # do
            H.p # do
                H.span # H.text ("This score: " <> show (gradeValue (GradeR grade)))
                H.label # do
                    H.text "Weighted: " 
                    H.input
                        [ A.value (show grade.weight)
                        , A.type_ "number"
                        , E.onChange (UpdateWeight .. eventNumber)
                        ] []
        H.li # do
            H.label # do
                H.text "Change Type: "
                H.select
                    ! A.value (scoreToLabel grade.score)
                    ! E.onChange (\o -> UpdateScore (emptyScore o.target.value)) 
                    ## forEach ["OutOf", "Percent", "Average"] \label ->
                           H.option ! A.value label # H.text label
            H.div # renderScore grade.score
            
            
  where
    bind = H.bind

    eventNumber o = G.readFloat o.target.value

    emptyScore string
      | string == "Percent" = Percent 1.0
      | string == "OutOf" = OutOf 10.0 10.0
      | string == "Average" = Average []
      
    scoreToLabel s =
        case s of
             OutOf _ _ -> "OutOf"
             Percent _ -> "Percent"
             Average _ -> "Average"

    renderScore (Average gs) = do
         H.button ! E.onClick (const AddGrade) # H.text "Add Grade"
         H.ul ## 
            forEachIndexed gs \i g ->
                H.li # (H.forwardTo (Child i) (view g))

    renderScore (OutOf a b) = do
        H.label # do
            H.text "Points: "
            H.input
                [ A.value (show a)
                , A.type_ "number"
                , E.onChange (UpdateScore .. (`OutOf` b) .. eventNumber)
                ] []
        H.label # do
            H.text "Out of: "
            H.input
                [ A.value (show b)
                , A.type_ "number"
                , E.onChange (UpdateScore .. OutOf a .. eventNumber)
                ] []

    renderScore (Percent n) = do
        H.label # do
            H.text "Percent: "
            H.input 
                [ A.value (show n)
                , E.onChange (UpdateScore .. Percent .. eventNumber)
                ] []

update :: Action -> State -> State
update (UpdateWeight n) (GradeR o) =
    GradeR (o { weight = n })
update (Child i a) g@(GradeR o) =
    case o.score of
         Average gs -> GradeR (o { 
             score = Average (
                 fromMaybe gs (A.modifyAt i (update a) gs) 
                 ) 
             } )
         _ -> g
update (UpdateScore s) (GradeR g) =
    GradeR (g { score = s })
update AddGrade g@(GradeR o) =
    case o.score of
         Average gs -> GradeR (o {
             score = Average (
                 gs `A.snoc` GradeR { weight: 0.0, score: Percent 1.0 }
             ) 
         })
         _ -> g


mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = map (uncurry f) (A.zip (A.range 0 (A.length xs)) xs)

forEachIndexed = flip mapIndexed
forEach = flip map

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: ex
        , update: fromSimple update
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html

    return unit
