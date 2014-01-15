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
