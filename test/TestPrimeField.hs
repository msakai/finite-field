{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, CPP #-}
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
import Data.Ratio

import Data.FiniteField
#if !defined(UseGHCTypeLits)
import TypeLevel.Number.Nat
#else
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
#endif

-- ----------------------------------------------------------------------
-- addition

prop_add_comm =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a + b == b + a

prop_add_assoc =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a + b) + c == a + (b + c)

prop_add_unitl =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 + a == a

prop_add_unitr =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + 0 == a

prop_negate =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      a + negate a == 0

prop_sub_negate =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \(b :: PrimeField p) ->
      a - b == a + negate b

-- ----------------------------------------------------------------------
-- multiplication

prop_mult_comm =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
      a * b == b * a

prop_mult_assoc =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (a * b) * c == a * (b * c)

prop_mult_unitl =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      1 * a == a

prop_mult_unitr =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      a * 1 == a

prop_mult_zero_l =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      0*a == 0

prop_mult_zero_r =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
        forAll arbitrary $ \(a :: PrimeField p) ->
          a*0 == 0

-- ----------------------------------------------------------------------
-- distributivity

prop_distl =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      a * (b + c) == a*b + a*c

prop_distr =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
    forAll arbitrary $ \b ->
    forAll arbitrary $ \c ->
      (b + c) * a == b*a + c*a

-- ----------------------------------------------------------------------
-- misc Num methods

prop_abs =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      abs a == a

prop_signum =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      signum a == 1

-- ----------------------------------------------------------------------
-- Fractional

prop_fromRational =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(r :: Rational) ->
      (fromRational r :: PrimeField p) == fromInteger (numerator r) / fromInteger (denominator r)

prop_recip =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      a /= 0 ==> a * (recip a) == 1

-- ----------------------------------------------------------------------
-- FiniteField

prop_pthRoot =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      pthRoot a ^ char a == a

prop_allValues = do
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    genericLength (allValues :: [PrimeField p]) == order (undefined :: PrimeField p)

-- ----------------------------------------------------------------------
-- Show / Read

prop_read_show =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      read (show a) == a  

-- ----------------------------------------------------------------------
-- Ord

prop_zero_minimum =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      0 <= a

-- ----------------------------------------------------------------------
-- NFData

prop_rnf =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      rnf a == ()

-- ----------------------------------------------------------------------
-- Enum

prop_toEnum_fromEnum =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      toEnum (fromEnum a) == a

prop_toEnum_negative = QM.monadicIO $ do
#if !defined(UseGHCTypeLits)
  SomeNat (_ :: p) <- QM.pick smallPrimes
#else
  SomeNat (_ :: Proxy p) <- QM.pick smallPrimes
#endif
  let a :: PrimeField p
      a = toEnum (-1)
  (ret :: Either SomeException (PrimeField p)) <- QM.run $ try $ evaluate $ a
  QM.assert $ isLeft ret

-- ----------------------------------------------------------------------

prop_hash =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      hash a `seq` () == ()

-- ----------------------------------------------------------------------
-- misc

prop_fromInteger_toInteger =
#if !defined(UseGHCTypeLits)
  forAll smallPrimes $ \(SomeNat (_ :: p)) ->
#else
  forAll smallPrimes $ \(SomeNat (_ :: Proxy p)) ->
#endif
    forAll arbitrary $ \(a :: PrimeField p) ->
      fromInteger (toInteger a) == a

case_primeFieldT = a @?= 1
  where
    a :: $(primeField 15485867)
    a = 15485867 + 1

------------------------------------------------------------------------

smallPrimes :: Gen SomeNat
smallPrimes = do
  i <- choose (0, 2^(16::Int))
#if !defined(UseGHCTypeLits)
  return $ withNat SomeNat (primes !! i)
#else
  return $ fromJust $ someNatVal $ primes !! i
#endif

#if !defined(UseGHCTypeLits)
instance Nat p => Arbitrary (PrimeField p) where
#else
instance KnownNat p => Arbitrary (PrimeField p) where
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
