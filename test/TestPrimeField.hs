{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fcontext-stack=32 #-}

import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Control.Monad
import Data.List (genericLength)
import Data.Numbers.Primes (primes)
import Data.Ratio

import Data.FiniteField
import TypeLevel.Number.Nat

-- ----------------------------------------------------------------------
-- addition

prop_add_comm =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a + b == b + a

prop_add_assoc =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a + b) + c == a + (b + c)

prop_add_unitl =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 + a == a

prop_add_unitr =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + 0 == a

prop_negate =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + negate a == 0

prop_sub_negate =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \(b :: PrimeField p) ->
      a - b == a + negate b

-- ----------------------------------------------------------------------
-- multiplication

prop_mult_comm =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a * b == b * a

prop_mult_assoc =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a * b) * c == a * (b * c)

prop_mult_unitl =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      1 * a == a

prop_mult_unitr =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a * 1 == a

prop_mult_zero_l =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0*a == 0

prop_mult_zero_r =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
        forAll arbitrary $ \(a :: PrimeField p) ->
          a*0 == 0

-- ----------------------------------------------------------------------
-- distributivity

prop_distl =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      a * (b + c) == a*b + a*c

prop_distr =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (b + c) * a == b*a + c*a

-- ----------------------------------------------------------------------
-- misc Num methods

prop_abs =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      abs a == a

prop_signum =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      signum a == 1

-- ----------------------------------------------------------------------
-- Fractional

prop_fromRational =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(r :: Rational) ->
      (fromRational r :: PrimeField p) == fromInteger (numerator r) / fromInteger (denominator r)

prop_recip =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a /= 0 ==> a * (recip a) == 1

-- ----------------------------------------------------------------------
-- FiniteField

prop_pthRoot =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      pthRoot a ^ char a == a

prop_allValues = do
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    genericLength (allValues :: [PrimeField p]) == order (undefined :: PrimeField p)

-- ----------------------------------------------------------------------
-- Show / Read

prop_read_show =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      read (show a) == a  

-- ----------------------------------------------------------------------

case_primeFieldT = a @?= 1
  where
    a :: $(primeField 15485867)
    a = 15485867 + 1

------------------------------------------------------------------------

smallPrimes :: Gen SomeNat
smallPrimes = do
  i <- choose (0, 2^(16::Int))
  return $ withNat SomeNat (primes !! i)

instance Nat p => Arbitrary (PrimeField p) where
  arbitrary = liftM fromInteger arbitrary

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
