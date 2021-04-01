{ mkDerivation, base, hashable, lens, stdenv, tasty, tasty-discover
, tasty-hunit, text
}:
mkDerivation {
  pname = "aws-arn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hashable lens text ];
  testHaskellDepends = [
    base lens tasty tasty-discover tasty-hunit text
  ];
  testToolDepends = [ tasty-discover ];
  description = "Optics for manipulating Amazon Resource Names (ARNs)";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
