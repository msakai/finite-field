{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.PrimeField
-- Copyright   :  (c) Masahiro Sakai 2013
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, MultiParamTypeClasses)
--
-- Finite field of prime order Fp.
--
-- References:
--
-- * <http://en.wikipedia.org/wiki/Finite_field>
--
-----------------------------------------------------------------------------
module Data.FiniteField.PrimeField
  ( PrimeField
  , toInteger
  ) where

import Prelude hiding (toInteger)
import Control.DeepSeq
import Data.Ratio (denominator, numerator)
import qualified Numeric.Algebra as Alg
import qualified TypeLevel.Number.Nat as TL

-- | Finite field of prime order Fp.
--
-- NB: Primality of @p@ is assumed, but not checked.
newtype PrimeField p = PrimeField Integer deriving (Eq)

-- | conversion to 'Integer'
toInteger :: PrimeField p -> Integer
toInteger (PrimeField a) = a

toInt :: Integral a => PrimeField p -> a
toInt = fromInteger . toInteger

instance Show (PrimeField p) where
  showsPrec n (PrimeField x) = showsPrec n x

instance TL.Nat p => Read (PrimeField p) where
  readsPrec n s = [(fromInteger a, s') | (a,s') <- readsPrec n s]

instance NFData (PrimeField p) where
  rnf (PrimeField a) = rnf a

instance TL.Nat p => Num (PrimeField p) where
  PrimeField a + PrimeField b = fromInteger $ a+b
  PrimeField a * PrimeField b = fromInteger $ a*b
  PrimeField a - PrimeField b = fromInteger $ a-b
  negate (PrimeField a)       = fromInteger $ negate a
  abs a         = a
  signum _      = 1
  fromInteger a = PrimeField $ a `mod` TL.toInt (undefined :: p)

instance TL.Nat p => Fractional (PrimeField p) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip a = a ^ (TL.toInt (undefined :: p) - 2 :: Integer)

instance TL.Nat p => Bounded (PrimeField p) where
  minBound = PrimeField 0
  maxBound = PrimeField (TL.toInt (undefined :: p) - 1)

instance TL.Nat p => Enum (PrimeField p) where
  toEnum x
    | toInt (minBound :: PrimeField p) <= x && x <= toInt (maxBound :: PrimeField p) = fromIntegral x
    | otherwise = error "PrimeField.toEnum: bad argument"
  fromEnum = toInt

instance Ord (PrimeField p) where
  PrimeField a `compare` PrimeField b = a `compare` b
  PrimeField a `max` PrimeField b = PrimeField (a `max` b)
  PrimeField a `min` PrimeField b = PrimeField (a `min` b)

-- ---------------------------------------------------------------------------

instance TL.Nat p => Alg.Multiplicative (PrimeField p) where
  (*) = (*)

instance TL.Nat p => Alg.Commutative (PrimeField p)

instance TL.Nat p => Alg.Unital (PrimeField p) where
  one = 1

instance TL.Nat p => Alg.Division (PrimeField p) where
  recip = recip

instance TL.Nat p => Alg.Additive (PrimeField p) where
  (+) = (+)

instance TL.Nat p => Alg.Abelian (PrimeField p)

instance TL.Nat p => Alg.Semiring (PrimeField p)

instance TL.Nat p => Alg.LeftModule Alg.Natural (PrimeField p) where
  n .* a = fromIntegral n * a

instance TL.Nat p => Alg.RightModule Alg.Natural (PrimeField p) where
  a *. n = a * fromIntegral n

instance TL.Nat p => Alg.Monoidal (PrimeField p) where
  zero = 0

instance TL.Nat p => Alg.LeftModule Integer (PrimeField p) where
  n .* a = fromIntegral n * a

instance TL.Nat p => Alg.RightModule Integer (PrimeField p) where
  a *. n = a * fromIntegral n

instance TL.Nat p => Alg.Group (PrimeField p) where
  negate = negate

instance TL.Nat p => Alg.Rig (PrimeField p)

instance TL.Nat p => Alg.Ring (PrimeField p)

instance TL.Nat p => Alg.Characteristic (PrimeField p) where
  char _ = TL.toInt (undefined :: p)

instance TL.Nat p => Alg.Field (PrimeField p)

-- ---------------------------------------------------------------------------

{-
type GF2 = PrimeField (SuccessorTo (SuccessorTo Zero))
type GF3 = PrimeField (SuccessorTo (SuccessorTo (SuccessorTo Zero)))
type GF5 = PrimeField (SuccessorTo (SuccessorTo (SuccessorTo (SuccessorTo (SuccessorTo Zero)))))
-}
