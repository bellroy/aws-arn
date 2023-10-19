{
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        makePackage = haskellPackages:
          haskellPackages.callPackage ./aws-arn.nix { };
      in
      rec
      {
        packages = {
          default = makePackage pkgs.haskellPackages;
          ghc810 = makePackage pkgs.haskell.packages.ghc810;
          ghc90 = makePackage pkgs.haskell.packages.ghc90;
          ghc92 = makePackage pkgs.haskell.packages.ghc92;
          ghc94 = makePackage pkgs.haskell.packages.ghc94;
          ghc96 = makePackage pkgs.haskell.packages.ghc96;
        };

        devShells = builtins.mapAttrs
          (_: v: v.env.overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs
              ++ [ pkgs.nixpkgs-fmt ]
              ++ (with pkgs.haskellPackages; [
              cabal-install
              cabal-fmt
              doctest
              haskell-ci
              hlint
            ]);
          }))
          packages;
      });
}
