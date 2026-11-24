{
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";

  inputs = {
    bellroy-nix-foss = {
      url = "github:bellroy/bellroy-nix-foss";
    };
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      # GHC 8.10, 9.0 and 9.2 are listed in tested-with in the cabal file
      # but are no longer available in our nixpkgs channel. CI
      # (haskell-ci) verifies these older versions still build.
      supportedCompilers = [
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
        "ghc914"
      ];
      defaultCompiler = "ghc914";
    };
}
