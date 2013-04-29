{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Control.Monad
import Data.Numbers.Primes (primes)

import Data.FiniteField.PrimeField (PrimeField)
import Data.NaturalNumber


-- ----------------------------------------------------------------------
-- addition

prop_add_comm =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a + b == b + a

prop_add_assoc =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a + b) + c == a + (b + c)

prop_add_unitl =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 + a == a

prop_add_unitr =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + 0 == a

prop_negate =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + negate a == 0

-- ----------------------------------------------------------------------
-- multiplication

prop_mult_comm =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a * b == b * a

prop_mult_assoc =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a * b) * c == a * (b * c)

prop_mult_unitl =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      1 * a == a

prop_mult_unitr =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a * 1 == a

prop_mult_zero_l =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0*a == 0

prop_mult_zero_r =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
        forAll arbitrary $ \(a :: PrimeField p) ->
          a*0 == 0

-- ----------------------------------------------------------------------
-- distributivity

prop_distl =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      a * (b + c) == a*b + a*c

prop_distr =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (b + c) * a == b*a + c*a

-- ----------------------------------------------------------------------
-- recip

prop_recip =
  forAll smallPrimes $ \(UnknownN (_ :: N p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a /= 0 ==> a * (recip a) == 1

-- ----------------------------------------------------------------------

smallPrimes :: Gen UnknownN
smallPrimes = elements $ map intToUnknownN $ takeWhile (<=1000) primes

instance NaturalNumber p => Arbitrary (PrimeField p) where
  arbitrary = liftM fromInteger arbitrary

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
