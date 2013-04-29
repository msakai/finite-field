{-# LANGUAGE ScopedTypeVariables, Rank2Types, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.UnknownN
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
module Data.FiniteField.UnknownN
  ( UnknownN (..)
  , intToUnknownN
  ) where

import Control.DeepSeq
import Data.Bits
import TypeLevel.Number.Nat

data UnknownN where
  UnknownN :: Nat n => n -> UnknownN

instance Show UnknownN where
  showsPrec d (UnknownN n) = showParen (d > 10) $
    showString "intToUnknownN " . shows (toInt n :: Integer)

instance NFData UnknownN

intToUnknownN :: Integer -> UnknownN
intToUnknownN a | a < 0  = error "intToUnknownN: negative number"
intToUnknownN 0 = UnknownN (undefined :: Z)
intToUnknownN a = f a (\n -> UnknownN n) (\n -> UnknownN n)
  where
    f :: Integer
      -> (forall n m. (Nat n, n ~ O m) => n -> UnknownN)
      -> (forall n m. (Nat n, n ~ I m) => n -> UnknownN)
      -> UnknownN
    f 1 _  k1 = k1 (undefined :: I Z)
    f x k0 k1 = f (x `shiftR` 1) k0' k1'
      where
        k0' :: forall n m. (Nat n, n ~ O m) => n -> UnknownN
        k0' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
        k1' :: forall n m. (Nat n, n ~ I m) => n -> UnknownN
        k1' _ =
          if testBit x 0
          then k1 (undefined :: I n)
          else k0 (undefined :: O n)
