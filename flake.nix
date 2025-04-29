{
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";

  inputs = {
    bellroy-nix-foss = {
      url = "github:bellroy/bellroy-nix-foss";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixpkgs-24.11-darwin";
  };

  outputs = inputs: inputs.bellroy-nix-foss.lib.haskellProject {
    src = ./.;
    supportedCompilers = [
      "ghc810"
      "ghc90"
      "ghc92"
      "ghc94"
      "ghc96"
      "ghc98"
      "ghc910"
      "ghc912"
    ];
    defaultCompiler = "ghc96";
  };
}
