module Pux.Bootstrap where

import Prelude hiding ((#), div)

import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
import Data.Foldable (intercalate)

data ColSize = Xs | Sm | Md | Lg

instance showColSize :: Show ColSize where
    show Xs = "xs"
    show Sm = "sm"
    show Md = "md"
    show Lg = "lg"

col :: forall a. ColSize -> Int -> Array (Attribute a) -> Array (Html a) -> Html a
col b i attrs elems =
    div ([className (intercalate "-" ["col", show b, show i])]) [div attrs elems]


button' :: forall a. String -> (_ -> a) -> Html a
button' t a = button ! onClick a ! className "btn btn-default" # text t
