4.4 [2018.01.28]
----------------
* Added `imapRep`, `ifoldMapRep`, `itraverseRep` to make it easier to define representable `FunctorWithIndex`, `FoldableWithIndex`, `TraversableWithIndex` instances from the `lens` package.
* Add `GHC.Generics`-based default implementation for `Data.Functor.Rep.Representable` instances
* Add `Data.Functor.Rep.Representable` instances for `Backwards`, `Reverse`, and the datatypes in `GHC.Generics`.
* Add `Data.Functor.Adjunction.Adjunction` instances for some datatypes in `GHC.Generics`
* Add `Data.Functor.Contravariant.Rep.Representable` instances for `U1` and `(:*:)` from `GHC.Generics`
* Add `collectRep` and `imapRep` functions to `Data.Functor.Rep`.
* Add `MINIMAL` pragmas to the `Adjunction` classes.
* Allow `free-5`.

4.3
---
* Removed a spurious superclass constraint for `Applicative (StoreT g w)`
* GHC 8 support
* `comonad` 5 support

4.2.2
-----
* Builds clean on GHC 7.10

4.2.1
-----
* `semigroupoids` 5 support.
* `profunctors` 5 support.

4.2
---
* `contravariant` 1.0 support. `Day` convolution moves to `kan-extensions`.

4.0.3
-----
* Silenced `Control.Monad.Instances` deprecation warnings on GHC 7.8

4.0.2
-----
* Added `mfixRep` to make it easier to define representable `MonadFix` instances.
* Added `mzipRep` and `mzipWithRep` to make it easier to define representable `MonadZip` instances.
* Added `duplicateRepBy`, `extendRepBy` and `extractRepBy` to make it easier to pick your own `Monoid`.
* Minor documentation fixes.

4.0.1
-----
* Increased lower bound on `contravariant` to match the actual requirement.

4.0
---
* Merged the contents of `representable-functors`.
* Removed the dependency on `keys`.
* Moved `Data.Functor.Contravariant.Representable` to `Data.Functor.Contravariant.Rep` and made the API mimic `Data.Profunctor.Rep`.
* Moved `Data.Functor.Representable` to `Data.Functor.Rep` and made the API mimic `Data.Profunctor.Rep`.
* Added `Tagged` and `Proxy` instances for `Data.Functor.Rep.Representable`
* Added a `Proxy` instance for `Data.Functor.Contravariant.Rep.Representable`

3.2.1.1
-------
* Updated the `array` dependency

3.2.1
-----
* Marked modules appropriately `Trustworthy`.

3.2
---
* Updated to `representable-functors` 3.1, which changed the API for contravariant representable functors.
