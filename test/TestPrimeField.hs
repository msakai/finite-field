{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Control.Monad
import Data.Numbers.Primes (primes)

import Data.FiniteField
import Data.FiniteField.SomeNat (SomeNat (..))
import qualified Data.FiniteField.SomeNat as SomeNat
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
-- recip

prop_recip =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a /= 0 ==> a * (recip a) == 1

-- ----------------------------------------------------------------------
-- pthRoot

prop_pthRoot =
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      pthRoot a ^ char a == a

-- ----------------------------------------------------------------------

prop_intToSomeNat = do
  forAll arbitrary $ \n ->
    case SomeNat.fromInteger (abs n) of
      SomeNat m -> abs n == toInt m

case_primeFieldT = a @?= 1
  where
    a :: $(primeField 15485867)
    a = 15485867 + 1

------------------------------------------------------------------------

smallPrimes :: Gen SomeNat
smallPrimes = do
  i <- choose (0, 2^(16::Int))
  return $ SomeNat.fromInteger $ primes !! i

instance Nat p => Arbitrary (PrimeField p) where
  arbitrary = liftM fromInteger arbitrary

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
