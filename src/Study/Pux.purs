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

type State = Grade

data Action
    = Child Int Action
    | UpdateWeight Int Number
    | UpdateScore (Score Number)
    | AddGrade

infixr 9 compose as ..

view :: State -> H.Html Action
view (OutOf a b) = H.div # do
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
  where
    bind = H.bind

view (Percent n) = do
    H.label # do
        H.text "Percent: "
        H.input 
            [ A.value (show n)
            , E.onChange (UpdateScore .. Percent .. eventNumber)
            ] []
  where
    bind = H.bind

view (Weighted grades) = H.div # do
    H.p # H.text "Weighted Grade Set:"
    H.ul # do
        H.li # do
            H.div # H.text "finish me"
  where
    bind = H.bind

view (Average grades) = H.div # do
    H.p # H.text "Grade Set:"
    H.ul # do
        H.li # do
            H.p # do
                H.span # H.text ("This score: " <> show (getScore grades))
        H.li # do
            H.div # renderScore grades
  where
    bind = H.bind

emptyScore string
  | string == "Percent" = Percent 1.0
  | string == "OutOf" = OutOf 10.0 10.0
  | string == "Average" = Average []
  | string == "Weighted" = Weighted []
  
renderScore gs = do
     H.button ! E.onClick (const AddGrade) # H.text "Add Grade"
     H.ul ## 
        forEachIndexed gs \i g ->
            H.li # (H.forwardTo (Child i) (view g))

scoreToLabel s =
    case s of
         OutOf _ _ -> "OutOf"
         Percent _ -> "Percent"
         Average _ -> "Average"


changeType value = H.label # do
    H.text "Change Type: "
    H.select
        ! A.value value 
        ! E.onChange (\o -> UpdateScore (emptyScore o.target.value)) 
        ## forEach ["OutOf", "Percent", "Average"] \label ->
               H.option ! A.value label # H.text label

eventNumber o = G.readFloat o.target.value

update :: Action -> State -> State
update (UpdateWeight i n) grades =
    case grades of
         Weighted gs -> modMaybe i (\(Tuple w s) -> Tuple n s) gs
         _ -> grades
update (Child i a) grades =
    let mm = modMaybe i (update a)
     in case grades of
         Average gs -> Average (mm gs)
         Weighted gs -> Weighted (mm gs)
         g -> g
update (UpdateScore s) score =
    s
update AddGrade grades =
    case grades of
         Average gs -> Average (gs `A.snoc` Percent 1.0) 
         Weighted gs -> Weighted (gs `A.snoc` Tuple 1.0 (Percent 1.0))
         _ -> grades

modMaybe i f xs = fromMaybe xs (A.modifyAt i f xs)

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
