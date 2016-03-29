module Study.Pux where

import Batteries hiding ((#))
import Data.Array as Arr
import Data.List

import Data.List.Zipper

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html)
import Pux.Html as H
import Pux.Html ((#), (##), (!))
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Signal.Channel (CHANNEL)
import Global as G

import Grade (Grade, Score(Percent, Weighted, Average, OutOf), ex, getScore)

type State = Zipper Grade

data Action
    = Child Int Action
    | UpdateWeight Int Number
    | UpdateScore (Score Number)
    | AddGrade
    | Undo
    | Redo

infixr 9 compose as ..

view :: State -> H.Html Action
view (Zipper _ g _) = H.div # do
    H.button ! E.onClick (const Undo) # H.text "Undo"
    H.button ! E.onClick (const Redo) # H.text "Redo"
    case g of
         OutOf a b -> H.div # do
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
            H.div # changeType g
         Percent n -> H.div # do
            H.label # do
                H.text "Percent: "
                H.input 
                    [ A.value (show n)
                    , E.onChange (UpdateScore .. Percent .. eventNumber)
                    ] []
            H.div # changeType g
         Weighted grades -> H.div # do
            H.p # H.text "Weighted Grade Set:"
            H.ul # do
                H.li # do
                    H.div # H.text "finish me"
            H.div # changeType g
         Average grades -> H.div # do
            H.p # H.text "Grade Set:"
            H.div # changeType g
            H.ul # do
                H.li # do
                    H.p # do
                        H.span # H.text ("This score: " <> show (getScore (Average grades)))
                H.li # do
                    H.div # renderScore grades
  where
    bind = H.bind


emptyScore :: String -> Score Number
emptyScore string
  | string == "Percent" = Percent 1.0
  | string == "OutOf" = OutOf 10.0 10.0
  | string == "Average" = Average []
  | string == "Weighted" = Weighted []
  | otherwise = Average []
  
renderScore :: Array (Score Number) -> Html Action
renderScore gs = H.div # do
     H.button ! E.onClick (const AddGrade) # H.text "Add Grade"
     H.ul ## 
        forEachIndexed gs \i g ->
            H.li # (H.forwardTo (Child i) (view (single g)))
  where
    bind = H.bind

scoreToLabel :: forall a. Score a -> String
scoreToLabel s =
    case s of
         OutOf _ _ -> "OutOf"
         Percent _ -> "Percent"
         Average _ -> "Average"
         Weighted _ -> "Weighted"


changeType :: Grade -> Html Action
changeType grade = H.label # do
    H.text "Change Type: "
    H.select
        ! A.value (scoreToLabel grade)
        ! E.onChange (\o -> UpdateScore (emptyScore o.target.value)) 
        ## forEach ["OutOf", "Percent", "Average", "Weighted"] \label ->
               H.option ! A.value label # H.text label
  where
    bind = H.bind

single a = Zipper Nil a Nil

eventNumber :: forall r t. { target :: { value :: String | r } | t } -> Number
eventNumber o = G.readFloat o.target.value

pushPast :: forall a. a -> Zipper a -> Zipper a
pushPast a (Zipper p c f) = Zipper (c:p) a f

update :: Action -> State -> State
update Redo state = fromMaybe state $ down state
update Undo state = fromMaybe state $ up state
update (UpdateWeight i n) state =
    case state of
         Zipper _ (Weighted gs) _ ->
            pushPast (Weighted (modMaybe i (\(Tuple w s) -> Tuple n s) gs)) state
         _ ->
            state
update (Child i a) state =
    case state of
         Zipper _ (Average gs) _ ->
            pushPast 
                (Average (map extract .. modMaybe i (update a) .. map single $ gs))
                state
         Zipper _ (Weighted gs) _ ->
            pushPast
                (Weighted (map (map extract)
                         .. modMaybe i (map (update a)) 
                         .. map (map single)
                         $ gs))
                state
         g ->
             state
update (UpdateScore s) state =
    pushPast s state
update AddGrade state =
    case state of
         Zipper _ (Average gs) _ ->
             pushPast (Average (gs `Arr.snoc` Percent 1.0)) state
         Zipper _ (Weighted gs) _ ->
            pushPast (Weighted (gs `Arr.snoc` Tuple 1.0 (Percent 1.0))) state
         _ ->
            state

modMaybe :: forall a. Int -> (a -> a) -> Array a -> Array a
modMaybe i f xs = fromMaybe xs (Arr.modifyAt i f xs)

mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = map (uncurry f) (Arr.zip (Arr.range 0 (Arr.length xs)) xs)

forEachIndexed :: forall a b. Array b -> (Int -> b -> a) -> Array a
forEachIndexed = flip mapIndexed

forEach :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
forEach = flip map

ui :: forall e. Eff ( err :: EXCEPTION , channel :: CHANNEL | e ) Unit
ui = do
    app <- start
        { initialState: Zipper Nil ex Nil
        , update: fromSimple update
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
