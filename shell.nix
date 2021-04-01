{ sources ? import ./nix/sources.nix
, nixpkgs ? import sources.nixpkgs { }
, compiler ? "default"
, doBenchmark ? false
}:
let
  env = (import ./. { inherit sources nixpkgs compiler doBenchmark; }).env;
  niv = (import sources.niv { }).niv;

  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: with nixpkgs.haskell.lib; {
      haskell-ci = unmarkBroken super.haskell-ci;
      lattices = unmarkBroken super.lattices;
      universe-reverse-instances = unmarkBroken super.universe-reverse-instances;
      zinza = doJailbreak super.zinza;
    };
  };
in
env.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ [ nixpkgs.nixpkgs-fmt niv ] ++ (with haskellPackages; [
    cabal-fmt
    haskell-ci
    hlint
  ]);
})
