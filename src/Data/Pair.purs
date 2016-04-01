module Data.Pair where

import Prelude
import Data.Monoid (class Monoid, mempty)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, sequenceDefault)

data Pair a = Pair a a

fst :: forall a. Pair a -> a
fst (Pair a _) = a

snd :: forall a. Pair a -> a
snd (Pair _ a) = a

first :: forall a. (a -> a) -> Pair a -> Pair a
first f (Pair a b) = Pair (f a) b

second :: forall a. (a -> a) -> Pair a -> Pair a
second f (Pair a b) = Pair a (f b)

instance showPair :: Show a => Show (Pair a) where
    show (Pair a b) = "Pair " <> show a <> " " <> show b

instance eqPair :: Eq a => Eq (Pair a) where
    eq (Pair a b) (Pair c d) = a == c && b == d

instance ordPair :: Ord a => Ord (Pair a) where
    compare (Pair a b) (Pair c d) = compare a c <> compare b d

instance functorPair :: Functor Pair where
    map f (Pair a b) = Pair (f a) (f b)

instance applyPair :: Apply Pair where
    apply (Pair f g) (Pair a b) = Pair (f a) (g b)

instance applicativePair :: Applicative Pair where
    pure a = Pair a a

instance semigroupPair :: Semigroup a => Semigroup (Pair a) where
    append (Pair a b) (Pair c d) = Pair (a <> c) (b <> d)

instance monoidPair :: Monoid a => Monoid (Pair a) where
    mempty = Pair mempty mempty

instance foldablePair :: Foldable Pair where
    foldMap f (Pair a b) = f a <> f b
    foldr k z (Pair a b) = k a (k b z)
    foldl k z (Pair a b) = k (k z a) b

instance traversablePair :: Traversable Pair where
    traverse f (Pair a b) = Pair <$> f a <*> f b
    sequence = sequenceDefault
