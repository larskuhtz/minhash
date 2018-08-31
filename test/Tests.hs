{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Data.Foldable
import Data.MinHash
import Data.Proxy
import qualified Data.Set as S

import GHC.TypeNats

import Numeric.Natural

import Prelude.Unicode

import System.Random.MWC

import Text.Printf

main ∷ IO Int
main = trials >>= \case
    True → return 0
    False → return 1
  where
    trials = (&&)
        <$> trial 10 10000 (100000, 200000) (100000, 200000)
        <*> trial 2 10000 (1000000, 2000000) (1000000, 2000000)

trial
    ∷ Natural
        -- ^ number of samples
    → Natural
        -- ^ size of MinHash
    → (Natural, Natural)
        -- ^ size of first set and size of domain from which to sample
    → (Natural, Natural)
        -- ^ size of second set and size of domain from which to sample
    → IO Bool
trial n s a b = do
    putStrLn $ "run " ++ show n ++ " experiments..."
    rs ← mapM (const $ test s a b) [0..n]
    let ds = abs ∘ uncurry (-) <$> rs
        dm = mean ds
        dv = var ds
    -- mapM_ (putStrLn ∘ show) rs
    putStrLn $ "max error: " ++ printf "%.6f" (maximum ds)
    putStrLn $ "mean of error: " ++ printf "%.6f" dm
    putStrLn $ "variance of error: " ++ printf "%.6f" dv
    return $ dm ≤ 0.01 ∧ dv ≤ 0.00001

-- | TODO add proper testing using OLS regression. Cover cases when
-- 's' is larger than size of one or both inputs.
--
test
    ∷ Natural
        -- ^ size of MinHash
    → (Natural, Natural)
        -- ^ size of first set and size of domain from which to sample
    → (Natural, Natural)
        -- ^ size of second set and size of domain from which to sample
    → IO (Double, Double)
test s (s₁, b₁) (s₂, b₂) = case someNatVal s of
    (SomeNat (_ ∷ Proxy s)) → do
        set₁ ← randomIntSet s₁ b₁
        set₂ ← randomIntSet s₂ b₂
        let !approx = approxJaccardCoeff (fromFoldable set₁) (fromFoldable set₂ ∷ MinHash s)
        let !actual = jaccardCoeff set₁ set₂
        return (approx, actual)

-- | n must not be larger than 'maxBound ∷ Int' and b must be smaller than n.
--
-- FIXME for b = n + o(1) this is very inefficient
--
randomIntSet ∷ Natural → Natural → IO (S.Set Int)
randomIntSet n b
    | int n > (maxBound ∷ Int) = error "size argument to randomIntSet must not be larger than maxBound ∷ Int"
    | b < n = error "bound argument to randomIntSet must not be smaller than size argument"
    | otherwise = do
        gen ← createSystemRandom
        let go !i !s
                | S.size s ≡ int n = return (i, s)
                | otherwise = go (i + 1) =<< flip S.insert s <$> uniformR (0, int b) gen
        (_i, s) ← go (0∷Int) mempty
        -- print (n,b,i)
        return s

int ∷ Integral a ⇒ Num b ⇒ a → b
int = fromIntegral
{-# INLINE int #-}

-- -------------------------------------------------------------------------- --
-- Quick and Dirty statistics, just enough for this test suite

mean ∷ Foldable f ⇒ f Double → Double
mean f = sum f / int (length f)
{-# INLINE mean #-}

var ∷ Foldable f ⇒ f Double → Double
var f = sum ((\x → (x - mean f)^(2∷Int)) <$> toList f) / int (length f)
{-# INLINE var #-}

{-
-- | A quick an dirty implementation of simple linear regression
--
-- y = α + β x + ε
--
slr
    ∷ Foldable v
    ⇒ v Double
        -- ^ response (dependent variable): y ^ n
    → v Double
        -- ^ regressor (independent variable): x ^ n
    → (Double, Double, Double)
        -- ^ (α, β, r)
slr y x = (α, β, r)
  where
    !n = int (length x)
    !x_ = sum x
    !y_ = sum y
    !xx_ = sum (zw (*) x x)
    !yy_ = sum (zw (*) y y)
    !xy_ = sum (zw (*) x y)

    β = (n * xy_ - x_ * y_) / (n * xx_ - x_^(2∷Int))
    α = y_ / n - (β * x_) / n
    r = (n * xy_ - x_ * y_) / sqrt ((n * xx_ - x_^(2∷Int)) * (n * yy_ - y_^(2∷Int)))

    zw o a b = zipWith o (toList a) (toList b)
-}

