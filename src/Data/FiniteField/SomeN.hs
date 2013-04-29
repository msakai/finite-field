{-# LANGUAGE ScopedTypeVariables, Rank2Types, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.SomeN
-- Copyright   :  (c) Masahiro Sakai 2013
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, Rank2Types, GADTs)
--
-- Utility for type-level manipulation of natural numbers
--
-----------------------------------------------------------------------------
module Data.FiniteField.SomeN
  ( SomeN (..)
  , fromInteger
  ) where

import Prelude hiding (fromInteger)
import Control.DeepSeq
import Data.Bits
import TypeLevel.Number.Nat

data SomeN where
  SomeN :: Nat n => n -> SomeN

instance Show SomeN where
  showsPrec d (SomeN n) = showParen (d > 10) $
    showString "fromInteger " . shows (toInt n :: Integer)

instance NFData SomeN

fromInteger :: Integer -> SomeN
fromInteger a | a < 0  = error "Data.FiniteField.SomeN.fromInteger: negative number"
fromInteger 0 = SomeN (undefined :: Z)
fromInteger a = f a (\n -> SomeN n) (\n -> SomeN n)
  where
    f :: Integer
      -> (forall n m. (Nat n, n ~ O m) => n -> SomeN)
      -> (forall n m. (Nat n, n ~ I m) => n -> SomeN)
      -> SomeN
    f 1 _  k1 = k1 (undefined :: I Z)
    f x k0 k1 = f (x `shiftR` 1) k0' k1'
      where
        k0' :: forall n m. (Nat n, n ~ O m) => n -> SomeN
        k0' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
        k1' :: forall n m. (Nat n, n ~ I m) => n -> SomeN
        k1' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
