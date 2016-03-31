module Study.Util where

import Batteries

import Global as G
import Data.Array as Arr
import Data.List ((:))
import Data.List.Zipper (Zipper(Zipper))

---------------------
-- Array Functions --
---------------------
modMaybe :: forall a. Int -> (a -> a) -> Array a -> Array a
modMaybe i f xs = fromMaybe xs (Arr.modifyAt i f xs)

deleteAtMaybe :: forall a. Int -> Array a -> Array a
deleteAtMaybe i xs = fromMaybe xs (Arr.deleteAt i xs)

mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = map (uncurry f) (Arr.zip (Arr.range 0 (Arr.length xs)) xs)

forEachIndexed :: forall a b. Array b -> (Int -> b -> a) -> Array a
forEachIndexed = flip mapIndexed

forEach :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
forEach = flip map

---------------------------
-- List Zipper Functions --
---------------------------
single :: forall a. a -> Zipper a
single a = Zipper Nil a Nil

pushPast :: forall a. a -> Zipper a -> Zipper a
pushPast a (Zipper p c f) = Zipper (c:p) a f

editToPast :: forall a. (a -> a) -> Zipper a -> Zipper a
editToPast g (Zipper p a f) = Zipper (a : p) (g a) f

editFocus :: forall a. (a -> a) -> Zipper a -> Zipper a
editFocus g (Zipper p a f) = Zipper p (g a) f

------------------------------
-- Event Handling Functions --
------------------------------
targetValue :: forall r t. { target :: { value :: String | r } | t } -> String
targetValue o = o.target.value

eventNumber :: forall r t. { target :: { value :: String | r } | t } -> Number
eventNumber = G.readFloat .. targetValue

