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
        makePackage = haskellPackages: (haskellPackages.override {
          overrides = final: prev: with pkgs.haskell.lib; {
            hedgehog = prev.callHackage "hedgehog" "1.2" { };
            tasty-discover = prev.callHackage "tasty-discover" "5.0.0" { };
            tasty-hedgehog = prev.callHackage "tasty-hedgehog" "1.4.0.0" { };
          };
        }).callPackage ./aws-arn.nix
          { };
      in
      rec
      {
        packages = {
          default = makePackage pkgs.haskellPackages;
          ghc884 = makePackage pkgs.haskell.packages.ghc884;
          ghc902 = makePackage pkgs.haskell.packages.ghc902;
          ghc925 = makePackage pkgs.haskell.packages.ghc925;
          ghc943 = makePackage pkgs.haskell.packages.ghc943;
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
