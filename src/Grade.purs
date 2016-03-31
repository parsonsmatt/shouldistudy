module Grade where

import Batteries

import Data.Array as A

data Score n
    = OutOf n n
    | Percent n
    | Average (Array (Score n))
    | Weighted (Array (Tuple n (Score n)))

instance functorScore :: Functor Score where
    map f (OutOf a b) = OutOf (f a) (f b)
    map f (Percent n) = Percent (f n)
    map f (Average grades) = Average (map (map f) grades)
    map f (Weighted grades) =
        Weighted (map (\(Tuple a b) -> Tuple (f a) (map f b)) grades)

instance showScore :: Show n => Show (Score n) where
    show (OutOf a b) = "OutOf " <> show a <> " " <> show b
    show (Percent n) = "Percent " <> show n
    show (Average gs) = "Average " <> show gs
    show (Weighted gs) = "Weighted " <> show gs

isAverage :: forall n. Score n -> Boolean
isAverage (Average _) = true
isAverage _ = false

overAverage :: forall a. (Array (Score a) -> Array (Score a)) -> Score a -> Score a
overAverage f (Average gs) = Average (f gs)
overAverage _ xs = xs

isWeighted :: forall n. Score n -> Boolean
isWeighted (Weighted _) = true
isWeighted _ = false

overWeighted :: forall a. (Array (Tuple a (Score a)) -> Array (Tuple a (Score a))) -> Score a -> Score a
overWeighted f (Weighted gs) = Weighted (f gs)
overWeighted _ xs = xs

getScore :: Score Number -> Number
getScore (OutOf a b) = 100.0 * (a / b)
getScore (Percent n) = n
getScore (Average grades) = average (map getScore grades)
getScore (Weighted grades) = g (foldl f (Tuple 0.0 0.0) grades)
  where
    f (Tuple accWeight accTotal) (Tuple weight score) =
        Tuple (accWeight + weight) (weight * (getScore score) + accTotal)
    g (Tuple weight score) =
        score / weight


average :: Array Number -> Number
average fs = if l == 0 then 0.0 else sum fs / toNumber l
  where
    l = A.length fs

type Grade = Score Number

ex :: Grade
ex = Average
    [ Percent 1.0
    , 12.0 `OutOf` 12.0
    , Percent 1.0
    , Average [ Percent 1.0, Percent 2.0 ]
    ]

emptyScore :: String -> Score String
emptyScore string
  | string == "Percent" = Percent "100.0"
  | string == "OutOf" = OutOf "10.0" "10.0"
  | string == "Average" = Average []
  | string == "Weighted" = Weighted []
  | otherwise = Average []
  

scoreToLabel :: forall a. Score a -> String
scoreToLabel s =
    case s of
         OutOf _ _ -> "OutOf"
         Percent _ -> "Percent"
         Average _ -> "Average"
         Weighted _ -> "Weighted"

