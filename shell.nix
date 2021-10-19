{ sources ? import ./nix/sources.nix
, nixpkgs ? import sources.nixpkgs { }
, compiler ? "default"
, doBenchmark ? false
}:
let
  env = (import ./. { inherit sources nixpkgs compiler doBenchmark; }).env;
in
env.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ [
    nixpkgs.nixpkgs-fmt
    nixpkgs.niv
  ] ++ (with nixpkgs.haskellPackages; [
    cabal-fmt
    doctest
    haskell-ci
    hlint
  ]);
})
