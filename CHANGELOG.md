# Revision history for aws-arn

## 0.3.3.0 -- 2025-01-16

* Remove direct dependency on `profunctors`.
* Add `Network.AWS.ARN.States` with support for State Machine ARNs.

## 0.3.2.0 -- 2024-08-22

* Use prisms and isos from `microlens-pro`.

## 0.3.1.0 -- 2023-03-01

* Add `Network.AWS.ARN.S3` with support for S3 bucket and object ARNs.

## 0.3.0.0 -- 2022-10-14

* Reinstate the improper `Iso'`s from version 0.1.x; the 0.2.0.0
  lenses were also unlawful and the `Iso'`s are more ergonomic.

## 0.2.0.0 -- 2022-07-19

* Rename `fromFoo`/`toFoo` to `renderFoo`/`parseFoo`:
  - `Network.AWS.ARN.toARN` -> `Network.AWS.ARN.parseARN`
  - `Network.AWS.ARN.fromARN` -> `Network.AWS.ARN.renderARN`
  - `Network.AWS.ARN.Lambda.fromFunction` -> `Network.AWS.ARN.renderFunction`
  - `Network.AWS.ARN.Lambda.toFunction` -> `Network.AWS.ARN.parseFunction`
* Remove the leading underscore and prefix on record names. This makes
  for a nicer interface when using the
  [generic-lens](https://hackage.haskell.org/package/generic-lens) and
  [generic-optics](https://hackage.haskell.org/package/generic-optics)
  libraries, which are now the preferred way to lens into individual
  fields.
* Remove dependency on `lens`; depend instead on `profunctors` (to
  provide `Prism'`s).
* `Network.AWS.ARN.colons`: Provide a `Lens'` instead of an unlawful `Iso'`.
* `Network.AWS.ARN.slashes`: Provide a `Lens'` instead of an unlawful `Iso'.`

## 0.1.0.1 -- 2021-12-16

* Support GHC 9.0.1, GHC 9.2.1, and `hashable <1.5`.

## 0.1.0.0 -- 2021-04-07

* First version. Released on an unsuspecting world.
