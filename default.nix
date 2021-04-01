{ sources ? import ./nix/sources.nix { }
, nixpkgs ? import sources.nixpkgs { }
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./aws-arn.nix { });

in
drv
