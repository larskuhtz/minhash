{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Data.MinHash
-- Copyright: Copyright © 2018 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- TODO:
--
-- * Add special case of large sets with constant size diffs
-- * Finish implementation of test suite
-- * add benchmarks
--
module Data.MinHash
( MinHash
, insert
, fromFoldable
, approxJaccardCoeff

-- * Testing
, jaccardCoeff
) where

import Data.Digest.Murmur64
import Data.Foldable
import Data.Proxy
import qualified Data.Set as S

import GHC.Generics
import GHC.TypeNats

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- MinHash

type role MinHash nominal
newtype MinHash (s ∷ Nat) = MinHash { _getMinHash ∷ S.Set Hash64 }
    deriving (Show, Eq, Ord, Generic)

instance KnownNat s ⇒ Semigroup (MinHash s) where
    a <> b = pruneSize $ MinHash $ _getMinHash a <> _getMinHash b
    {-# INLINE (<>) #-}

instance KnownNat s ⇒ Monoid (MinHash s) where
    mempty = MinHash mempty
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

insert ∷ ∀ s a . KnownNat s ⇒ Hashable64 a ⇒ a → MinHash s → MinHash s
insert a h = pruneSize $ MinHash $ S.insert (hash64 a) (_getMinHash h)
{-# INLINE insert #-}

approxJaccardCoeff ∷ ∀ s . KnownNat s ⇒ MinHash s → MinHash s → Double
approxJaccardCoeff (MinHash a) (MinHash b) =
    int (S.size y) / int n
  where
    n = min (intVal @s) (S.size x)
    x = S.take (intVal @s) (a <> b)
    y = x `S.intersection` (a `S.intersection` b)
{-# INLINE approxJaccardCoeff #-}

fromFoldable ∷ Foldable f ⇒ KnownNat s ⇒ Hashable64 a ⇒ f a → MinHash s
fromFoldable = foldl' (flip insert) mempty
{-# INLINE fromFoldable #-}

-- -------------------------------------------------------------------------- --
-- Utils

int ∷ Integral a ⇒ Num b ⇒ a → b
int = fromIntegral
{-# INLINE int #-}

intVal ∷ ∀ s a . Num a ⇒ KnownNat s ⇒ a
intVal = int $ natVal (Proxy @s)
{-# INLINE intVal #-}

pruneSize ∷ ∀ s . KnownNat s ⇒ MinHash s → MinHash s
pruneSize = MinHash ∘ S.take (intVal @s) ∘ _getMinHash
{-# INLINE pruneSize #-}

-- -------------------------------------------------------------------------- --
-- Testing

jaccardCoeff ∷ Eq a ⇒ Ord a ⇒ S.Set a → S.Set a → Double
jaccardCoeff a b =
    int (S.size (a `S.intersection` b)) / int (S.size (a `S.union` b))

