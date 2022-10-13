{ mkDerivation, base, deriving-compat, hashable, lib, profunctors
, tagged, tasty, tasty-discover, tasty-hunit, text
}:
mkDerivation {
  pname = "aws-arn";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deriving-compat hashable profunctors tagged text
  ];
  testHaskellDepends = [
    base deriving-compat profunctors tagged tasty tasty-hunit text
  ];
  testToolDepends = [ tasty-discover ];
  description = "Types and optics for manipulating Amazon Resource Names (ARNs)";
  license = lib.licenses.bsd3;
}
