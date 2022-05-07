next [????.??.??]
-----------------
* Define the `Functor` instance for `Co` with `fmap = fmapRep`. This brings
  the `Functor` instance in line with `Co`'s other instances, which also have a
  `Representable` constraint in the instance context. Previously, it used the
  underlying `Functor` instance, which made it easy to write looping code if
  one were to write `deriving (Functor, Applicative, ..) via Co F`.
* The `(<*)` and `(*>)` methods in the `Applicative` instance for `Co` are
  now defined to be `as <* _ = as` and `_ *> bs = bs`, which run in _O(1)_
  time. These implementations follow from the `Representable` laws.
* Use more concise `MINIMAL` defaults for the `Adjunction` classes.
* TODO: Describe `cotraverse1`-related changes
* The dependencies on `semigroups` and `void` are both now conditional
  on the GHC version being old enough that they're not in `base`.

4.4.1 [2022.05.07]
------------------
* Allow building with `transformers-0.6.*` and `mtl-2.3.*`.

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
