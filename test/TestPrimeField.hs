{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DataKinds, CPP, TypeOperators #-}
{-# OPTIONS_GHC -fcontext-stack=32 #-}

import Prelude hiding (toInteger)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Test.QuickCheck.Monadic as QM

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Either
import Data.Hashable
import Data.List (genericLength)
import Data.Numbers.Primes (primes)
import Data.Proxy
import Data.Ratio

import Data.FiniteField
#ifdef UseGHCTypeLits
import Data.Maybe
import GHC.TypeLits
#else
import TypeLevel.Number.Nat
#endif

-- ----------------------------------------------------------------------
-- addition

prop_add_comm =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a + b == b + a

prop_add_assoc =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a + b) + c == a + (b + c)

prop_add_unitl =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 + a == a

prop_add_unitr =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + 0 == a

prop_negate =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + negate a == 0

prop_sub_negate =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \(b :: PrimeField p) ->
      a - b == a + negate b

-- ----------------------------------------------------------------------
-- multiplication

prop_mult_comm =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a * b == b * a

prop_mult_assoc =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a * b) * c == a * (b * c)

prop_mult_unitl =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      1 * a == a

prop_mult_unitr =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a * 1 == a

prop_mult_zero_l =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0*a == 0

prop_mult_zero_r =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
        forAll arbitrary $ \(a :: PrimeField p) ->
          a*0 == 0

-- ----------------------------------------------------------------------
-- distributivity

prop_distl =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      a * (b + c) == a*b + a*c

prop_distr =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (b + c) * a == b*a + c*a

-- ----------------------------------------------------------------------
-- misc Num methods

prop_abs =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      abs a == a

prop_signum =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      signum a == 1

-- ----------------------------------------------------------------------
-- Fractional

prop_fromRational =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(r :: Rational) ->
      (fromRational r :: PrimeField p) == fromInteger (numerator r) / fromInteger (denominator r)

prop_recip =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      a /= 0 ==> a * (recip a) == 1

-- ----------------------------------------------------------------------
-- FiniteField

prop_pthRoot =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      pthRoot a ^ char a == a

prop_allValues = do
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    genericLength (allValues :: [PrimeField p]) == order (undefined :: PrimeField p)

-- ----------------------------------------------------------------------
-- Show / Read

prop_read_show =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      read (show a) == a  

-- ----------------------------------------------------------------------
-- Ord

prop_zero_minimum =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 <= a

-- ----------------------------------------------------------------------
-- NFData

prop_rnf =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      rnf a == ()

-- ----------------------------------------------------------------------
-- Enum

prop_toEnum_fromEnum =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      toEnum (fromEnum a) == a

prop_toEnum_negative = QM.monadicIO $ do
  SomeNat' (_ :: Proxy p) <- QM.pick smallPrimes
  let a :: PrimeField p
      a = toEnum (-1)
  (ret :: Either SomeException (PrimeField p)) <- QM.run $ try $ evaluate $ a
  QM.assert $ isLeft ret

-- https://github.com/msakai/finite-field/issues/2
case_toEnum_big_integer =
  (toEnum 7 :: $(primeField (2^127 - 1))) @?= 7  

-- ----------------------------------------------------------------------

prop_hash =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      hash a `seq` () == ()

-- ----------------------------------------------------------------------
-- misc

prop_fromInteger_toInteger =
  forAll smallPrimes $ \(SomeNat' (_ :: Proxy p)) ->
    forAll arbitrary $ \(a :: PrimeField p) ->
      fromInteger (toInteger a) == a

case_primeFieldT = a @?= 1
  where
    a :: $(primeField 15485867)
    a = 15485867 + 1

------------------------------------------------------------------------

#ifdef UseGHCTypeLits

data SomeNat' where
  SomeNat' :: KnownNat p => Proxy p -> SomeNat'

instance Show SomeNat' where
  showsPrec p (SomeNat' x) = showsPrec p (natVal x)

#else

data SomeNat' where
  SomeNat' :: Nat p => Proxy p -> SomeNat'

instance Show SomeNat' where
  showsPrec p (SomeNat' (x :: Proxy p)) = showsPrec p (toInt (undefined :: p))

#endif

smallPrimes :: Gen SomeNat'
smallPrimes = do
  i <- choose (0, 2^(16::Int))
#ifdef UseGHCTypeLits
  case fromJust $ someNatVal $ primes !! i of
    SomeNat proxy -> return $ SomeNat' proxy
#else
  let f :: forall p. Nat p => p -> SomeNat'
      f _ = SomeNat' (Proxy :: Proxy p)
  return $ withNat f (primes !! i)
#endif

#ifdef UseGHCTypeLits
instance KnownNat p => Arbitrary (PrimeField p) where
#else
instance Nat p => Arbitrary (PrimeField p) where
#endif
  arbitrary = liftM fromInteger arbitrary

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)

#if !MIN_VERSION_base(4,7,0)

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

#endif
