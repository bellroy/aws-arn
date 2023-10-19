{
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";

  inputs = {
    bellroy-nix-foss = {
      url = "github:bellroy/bellroy-nix-foss";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs: inputs.bellroy-nix-foss.lib.haskellProject {
    cabalPackages = [
      {
        name = "aws-arn";
        path = ./aws-arn.nix;
      }
    ];
    supportedCompilers = [ "ghc810" "ghc90" "ghc92" "ghc94" "ghc96" ];
    defaultCompiler = "ghc92";
  };
}
