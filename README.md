# aws-arn

![CI Status](https://github.com/bellroy/aws-arn/actions/workflows/haskell-ci.yml/badge.svg)

This library provides a type representing [Amazon Resource Names
(ARNs)](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html),
and parsing/unparsing functions for them. The provided optics make it
very convenient to rewrite parts of ARNs.

Start reading at the `Network.AWS.ARN` module, which defines the core
data type and includes some examples.

The `ARN` type is not designed to be a 100% correct-by-construction
representation of only valid ARNs; it is designed to be a lightweight
way to destructure and reassemble ARNs to be used in place of string
munging.

The library aims to provide additional parsers for destructuring the
"resource" part of an ARN, but many are missing right now. PRs to add
this support for more AWS resource types are **especially** welcome.

## Guide: adding a resource

Cribbing from an existing module (e.g., `Network.AWS.ARN.Lambda`) is
probably the easiest way to start, but here is an explicit process to
add a new resource:

1. Create a module for the AWS service, if it doesn't already
   exist. Example: `src/Network/AWS/ARN/Lambda.hs`.

2. Define a record `Foo` to represent the parsed resource part of an
   ARN, and derive (at least) `Eq`, `Ord`, `Hashable`, `Show` and
   `Generic`. Also generate lenses for its fields:

   ```haskell
   data Function = Function
   { _fName :: Text,
     _fQualifier :: Maybe Text
   }
   deriving (Eq, Ord, Hashable, Show, Generic)

   $(makeLenses ''Function)
   ```

3. Define `toFoo` and `fromFoo` functions that attempt to parse and
   unparse the resource part of the ARN:

   ```haskell
   toFunction :: Text -> Maybe Function
   fromFunction :: Function -> Text
   ```

   **Remark:** While these names sound backwards compared to
   `fromText` and `toText`, it means we can have multiple parsing
   functions in a single service's module.

   **Remark:** If you need to write tests for these functions, the
   corresponding module should live at
   `test/Network/AWS/ARN/SomeAWSService/Test.hs`

4. Define a `_Foo` `Prism'` that combines the parsing/unparsing
   functions above:

   ```haskell
   _Function :: Prism' Text Function
   _Function = prism' fromFunction toFunction
   ```

5. Add the records, its fields, its parsing/unparsing functions, and
   its optics to the service module's export list:

   ```haskell
   module Network.AWS.ARN.Lambda
     ( -- * Functions
       Function (..),
       toFunction,
       fromFunction,

       -- ** Function Optics
       _Function,
       fName,
       fQualifier,
     )
   ```

6. Test your work and make a PR.

## Formatters

The formatters used in this repo are provided by `shell.nix`:

* `*.hs`: [`ormolu`](https://github.com/tweag/ormolu)
* `*.cabal`:
  [`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt)
  (`cabal-fmt --inplace wai-handler-hal.cabal`)
* `*.nix`:
  [`nixpkgs-fmt`](https://github.com/nix-community/nixpkgs-fmt)
  (`nixpkgs-fmt *.nix`)

## Regenerate CI

This repo uses `haskell-ci`, which is provided by `shell.nix`:

```shell
haskell-ci regenerate
```