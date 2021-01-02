0.10.0
------
* fix `toEnum` to work when `p` is beyond Int's `maxBound`.

0.9.0
-----
* use `tasty` instead of `test-framework` for testing
* add `UseGHCTypeLits` flag to switch to use GHC's type-level natural numbers instead of `type-level-numbers` package

0.8.0
-----
* remove dependency on `algebra` package, since it is outdated and not compatible with recent version of other packages

0.7.0
-----
* use extended GCD to compute reciprocals
* conform with the addition of SomeNat type to type-level-numbers-0.1.1.0.

0.6.0
-----
* add Hashable instance
* add allValues to FiniteField class
  .
0.5.0
-----
* introduce FiniteField class
