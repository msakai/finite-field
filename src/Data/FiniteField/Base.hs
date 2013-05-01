-----------------------------------------------------------------------------
-- |
-- module      :  Data.FiniteField.Base
-- Copyright   :  (c) Masahiro Sakai 2013
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.FiniteField.Base
  ( FiniteField (..)
  ) where

import Data.Ratio
import qualified Numeric.Algebra as Alg

-- | Type class for finite fields
class Fractional k => FiniteField k where
  -- | The order is number of elements of a finite field @k@.
  -- It of the form @p^n@, where @p@ is  prime number called the characteristic
  -- of the field, and @n@ is a positive integer.
  order   :: k -> Integer

  -- | The characteristic of a field, @p@.
  char    :: k -> Integer

  -- | The inverse of Frobenius endomorphism @x@ ↦ @x^p@.
  pthRoot :: k -> k

  -- | All values of the field
  allValues :: [k]
