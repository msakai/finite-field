{-# LANGUAGE ScopedTypeVariables, Rank2Types, GADTs, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.SomeNat
-- Copyright   :  (c) Masahiro Sakai 2013
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, Rank2Types, GADTs, DeriveDataTypeable)
--
-- Utility for type-level manipulation of natural numbers
--
-----------------------------------------------------------------------------
module Data.FiniteField.SomeNat
  ( SomeNat (..)
  , fromInteger
  ) where

import Prelude hiding (fromInteger)
import Control.DeepSeq
import Data.Bits
import Data.Typeable
import TypeLevel.Number.Nat

data SomeNat where
  SomeNat :: Nat n => n -> SomeNat
  deriving Typeable

instance Show SomeNat where
  showsPrec d (SomeNat n) = showParen (d > 10) $
    showString "fromInteger " . shows (toInt n :: Integer)

instance NFData SomeNat

fromInteger :: Integer -> SomeNat
fromInteger a | a < 0  = error "Data.FiniteField.SomeNat.fromInteger: negative number"
fromInteger 0 = SomeNat (undefined :: Z)
fromInteger a = f a (\n -> SomeNat n) (\n -> SomeNat n)
  where
    f :: Integer
      -> (forall n m. (Nat n, n ~ O m) => n -> SomeNat)
      -> (forall n m. (Nat n, n ~ I m) => n -> SomeNat)
      -> SomeNat
    f 1 _  k1 = k1 (undefined :: I Z)
    f x k0 k1 = f (x `shiftR` 1) k0' k1'
      where
        k0' :: forall n m. (Nat n, n ~ O m) => n -> SomeNat
        k0' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
        k1' :: forall n m. (Nat n, n ~ I m) => n -> SomeNat
        k1' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
