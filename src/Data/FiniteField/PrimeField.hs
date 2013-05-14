{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.PrimeField
-- Copyright   :  (c) Masahiro Sakai 2013
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TemplateHaskell)
--
-- Finite field of prime order p, Fp = Z/pZ.
--
-- References:
--
-- * <http://en.wikipedia.org/wiki/Finite_field>
--
-----------------------------------------------------------------------------
module Data.FiniteField.PrimeField
  ( PrimeField
  , toInteger
  -- * Template haskell utilities
  -- $TH
  , primeField
  ) where

import Prelude hiding (toInteger)
import Control.DeepSeq
import Data.Hashable
import Data.Ratio (denominator, numerator)
import Data.Typeable
import qualified Language.Haskell.TH as TH
import qualified Numeric.Algebra as Alg
import qualified TypeLevel.Number.Nat as TL
import Data.FiniteField.Base

-- | Finite field of prime order p, Fp = Z/pZ.
--
-- NB: Primality of @p@ is assumed, but not checked.
newtype PrimeField p = PrimeField Integer deriving (Eq, Typeable)

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

instance TL.Nat p => FiniteField (PrimeField p) where
  order _   = TL.toInt (undefined :: p)
  char _    = TL.toInt (undefined :: p)
  pthRoot a = a
  allValues = [minBound .. maxBound]

instance TL.Nat p => Hashable (PrimeField p) where
  hashWithSalt s (PrimeField a) =
    s `hashWithSalt` (TL.toInt (undefined :: p) :: Int) `hashWithSalt` a

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

-- | Create a PrimeField type
primeField :: Integer -> TH.TypeQ
primeField n
  | n <= 0    = error "primeField: negative value"
  | otherwise = [t| PrimeField $(TL.natT n) |]

-- $TH
-- Here is usage example for primeField:
--
-- > a :: $(primeField 15485867)
-- > a = 1
