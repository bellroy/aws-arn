{ mkDerivation, base, deriving-compat, hashable, lens, stdenv
, tasty, tasty-discover, tasty-hunit, text
}:
mkDerivation {
  pname = "aws-arn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deriving-compat hashable lens text
  ];
  testHaskellDepends = [
    base deriving-compat lens tasty tasty-discover tasty-hunit text
  ];
  testToolDepends = [ tasty-discover ];
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";
  license = stdenv.lib.licenses.bsd3;
}
