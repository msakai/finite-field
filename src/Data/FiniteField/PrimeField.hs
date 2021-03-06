{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TemplateHaskell, BangPatterns #-}
{-# LANGUAGE CPP, KindSignatures, DataKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteField.PrimeField
-- Copyright   :  (c) Masahiro Sakai 2013-2014
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TemplateHaskell, BangPatterns)
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
#if !defined(UseGHCTypeLits)
import qualified TypeLevel.Number.Nat as TL
#else
import GHC.TypeLits
#endif
import Data.FiniteField.Base

-- | Finite field of prime order p, Fp = Z/pZ.
--
-- NB: Primality of @p@ is assumed, but not checked.
#if !defined(UseGHCTypeLits)
newtype PrimeField p = PrimeField Integer deriving (Eq, Typeable)
#else
newtype PrimeField (p::Nat) = PrimeField Integer deriving (Eq, Typeable)
#endif

#if !defined(UseGHCTypeLits)
type KnownNat p = TL.Nat p
#endif

-- | conversion to 'Integer'
toInteger :: PrimeField p -> Integer
toInteger (PrimeField a) = a

toInt :: Integral a => PrimeField p -> a
toInt = fromInteger . toInteger

instance Show (PrimeField p) where
  showsPrec n (PrimeField x) = showsPrec n x

instance KnownNat p => Read (PrimeField p) where
  readsPrec n s = [(fromInteger a, s') | (a,s') <- readsPrec n s]

instance NFData (PrimeField p) where
  rnf (PrimeField a) = rnf a

instance KnownNat p => Num (PrimeField p) where
  PrimeField a + PrimeField b = fromInteger $ a+b
  PrimeField a * PrimeField b = fromInteger $ a*b
  PrimeField a - PrimeField b = fromInteger $ a-b
  negate (PrimeField a)       = fromInteger $ negate a
  abs a         = a
  signum _      = 1
  fromInteger a = ret
    where
      ret = PrimeField $ a `mod` char ret

instance KnownNat p => Fractional (PrimeField p) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
--  recip a = a ^ (char a - 2 :: Integer)
  recip x@(PrimeField a) =
    case exgcd a p of
      (_, r, _) -> fromInteger r
    where
      p :: Integer
      p = char x

instance KnownNat p => Bounded (PrimeField p) where
  minBound = PrimeField 0
  maxBound = ret
    where
      ret = PrimeField (char ret - 1)

instance KnownNat p => Enum (PrimeField p) where
  toEnum x
    | toInteger (minBound :: PrimeField p) <= x' && x' <= toInteger (maxBound :: PrimeField p) = PrimeField x'
    | otherwise = error "PrimeField.toEnum: bad argument"
    where
      x' = fromIntegral x
  fromEnum = toInt

instance Ord (PrimeField p) where
  PrimeField a `compare` PrimeField b = a `compare` b

instance KnownNat p => FiniteField (PrimeField p) where
  order x   = char x
#if !defined(UseGHCTypeLits)
  char _    = TL.toInt (undefined :: p)
#else
  char _    = natVal (Proxy :: Proxy p)
#endif
  pthRoot a = a
  allValues = [minBound .. maxBound]

instance KnownNat p => Hashable (PrimeField p) where
  hashWithSalt s x@(PrimeField a) =
    s `hashWithSalt` char x `hashWithSalt` a

-- | Extended GCD algorithm
exgcd :: Integral a => a -> a -> (a, a, a)
exgcd f1 f2 = f $ go f1 f2 1 0 0 1
  where
    go !r0 !r1 !s0 !s1 !t0 !t1
      | r1 == 0   = (r0, s0, t0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        (q, r2) = r0 `divMod` r1
        s2 = s0 - q*s1
        t2 = t0 - q*t1
    f (g,u,v)
      | g < 0 = (-g, -u, -v)
      | otherwise = (g,u,v)

-- ---------------------------------------------------------------------------

-- | Create a PrimeField type
primeField :: Integer -> TH.TypeQ
primeField n
  | n <= 0    = error "primeField: negative value"
#if !defined(UseGHCTypeLits)
  | otherwise = [t| PrimeField $(TL.natT n) |]
#else
  | otherwise = [t| PrimeField $(TH.litT (TH.numTyLit n)) |]
#endif

-- $TH
-- Here is usage example for primeField:
--
-- > a :: $(primeField 15485867)
-- > a = 1
