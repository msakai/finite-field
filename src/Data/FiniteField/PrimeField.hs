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
import TypeLevel.NaturalNumber

-- | Finite field of prime order Fp.
--
-- NB: Primality of @p@ is assumed, but not checked.
newtype PrimeField p = PrimeField Int deriving (Eq)

-- | conversion to 'Integer'
toInteger :: PrimeField p -> Integer
toInteger = fromIntegral . toInt

toInt :: PrimeField p -> Int
toInt (PrimeField a) = a

fromInt :: forall p. NaturalNumber p => Int -> PrimeField p
fromInt a = PrimeField (a `mod` p)
  where
    p = naturalNumberAsInt (undefined :: p)

instance Show (PrimeField p) where
  showsPrec n (PrimeField x) = showsPrec n x

instance NaturalNumber p => Read (PrimeField p) where
  readsPrec n s = [(fromInt a, s') | (a,s') <- readsPrec n s]

instance NFData (PrimeField p) where
  rnf (PrimeField a) = rnf a

instance NaturalNumber p => Num (PrimeField p) where
  PrimeField a + PrimeField b = fromInt $ a+b
  PrimeField a * PrimeField b = fromInteger $ fromIntegral a * fromIntegral b
  PrimeField a - PrimeField b = fromInt $ a-b
  negate (PrimeField a)       = fromInt $ negate a
  abs a         = a
  signum _      = 1
  fromInteger a = PrimeField $ fromIntegral (a `mod` p)
    where
      p = fromIntegral $ naturalNumberAsInt (undefined :: p)

instance NaturalNumber p => Fractional (PrimeField p) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip a = a ^ (p-2)
    where
      p = naturalNumberAsInt (undefined :: p)

instance NaturalNumber p => Bounded (PrimeField p) where
  minBound = PrimeField 0
  maxBound = PrimeField (p - 1)
    where
      p = fromIntegral $ naturalNumberAsInt (undefined :: p)

instance NaturalNumber p => Enum (PrimeField p) where
  toEnum x
    | toInt (minBound :: PrimeField p) <= x && x <= toInt (maxBound :: PrimeField p) = fromIntegral x
    | otherwise = error "PrimeField.toEnum: bad argument"
  fromEnum = toInt

instance Ord (PrimeField p) where
  PrimeField a `compare` PrimeField b = a `compare` b
  PrimeField a `max` PrimeField b = PrimeField (a `max` b)
  PrimeField a `min` PrimeField b = PrimeField (a `min` b)

-- ---------------------------------------------------------------------------

instance NaturalNumber p => Alg.Multiplicative (PrimeField p) where
  (*) = (*)

instance NaturalNumber p => Alg.Commutative (PrimeField p)

instance NaturalNumber p => Alg.Unital (PrimeField p) where
  one = 1

instance NaturalNumber p => Alg.Division (PrimeField p) where
  recip = recip

instance NaturalNumber p => Alg.Additive (PrimeField p) where
  (+) = (+)

instance NaturalNumber p => Alg.Abelian (PrimeField p)

instance NaturalNumber p => Alg.Semiring (PrimeField p)

instance NaturalNumber p => Alg.LeftModule Alg.Natural (PrimeField p) where
  n .* a = fromIntegral n * a

instance NaturalNumber p => Alg.RightModule Alg.Natural (PrimeField p) where
  a *. n = a * fromIntegral n

instance NaturalNumber p => Alg.Monoidal (PrimeField p) where
  zero = 0

instance NaturalNumber p => Alg.LeftModule Integer (PrimeField p) where
  n .* a = fromIntegral n * a

instance NaturalNumber p => Alg.RightModule Integer (PrimeField p) where
  a *. n = a * fromIntegral n

instance NaturalNumber p => Alg.Group (PrimeField p) where
  negate = negate

instance NaturalNumber p => Alg.Rig (PrimeField p)

instance NaturalNumber p => Alg.Ring (PrimeField p)

instance NaturalNumber p => Alg.Characteristic (PrimeField p) where
  char _ = fromIntegral p
    where
      p = naturalNumberAsInt (undefined :: p)

instance NaturalNumber p => Alg.Field (PrimeField p)

-- ---------------------------------------------------------------------------

{-
type GF2 = PrimeField (SuccessorTo (SuccessorTo Zero))
type GF3 = PrimeField (SuccessorTo (SuccessorTo (SuccessorTo Zero)))
type GF5 = PrimeField (SuccessorTo (SuccessorTo (SuccessorTo (SuccessorTo (SuccessorTo Zero)))))
-}
