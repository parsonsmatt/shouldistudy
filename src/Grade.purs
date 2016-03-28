module Grade where

import Batteries

import Data.Array as A

newtype GradeR n = GradeR { weight :: n, score :: Score n }

instance showGrade :: Show n => Show (GradeR n) where
    show (GradeR n) = fold
        [ "GradeR { weight: "
        , show n.weight
        , ", score: "
        , show n.score
        , " }"
        ]

instance functorGrade :: Functor GradeR where
    map f (GradeR rs) = GradeR { weight: f rs.weight, score: map f rs.score }

data Score n
    = OutOf n n
    | Percent n
    | Average (Array (GradeR n))

instance functorScore :: Functor Score where
    map f (OutOf a b) = OutOf (f a) (f b)
    map f (Percent n) = Percent (f n)
    map f (Average grades) = Average (map (map f) grades)

instance showScore :: Show n => Show (Score n) where
    show (OutOf a b) = "OutOf " <> show a <> show b
    show (Percent n) = "Percent " <> show n
    show (Average gs) = "Average " <> show gs

type Grade = GradeR Number

ex :: Grade
ex = GradeR
    { weight: 1.0
    , score: Average 
        [ GradeR { weight: 0.2, score: Percent 1.0 }
        , GradeR { weight: 0.8, score: 10.0 `OutOf` 12.0 }
        ]
    }

getScore :: Score Number -> Number
getScore (OutOf a b) = a / b
getScore (Percent n) = n
getScore (Average grades) = average (map gradeValue grades)

gradeValue :: GradeR Number -> Number
gradeValue (GradeR grade) = grade.weight * getScore grade.score


average :: Array Number -> Number
average fs = if l == 0 then 0.0 else sum fs / toNumber l
  where
    l = A.length fs
