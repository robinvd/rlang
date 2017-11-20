{ mkDerivation, attoparsec, base, bytestring, containers, mtl
, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell
, transformers, fetchgit
}:
mkDerivation {
  pname = "llvm-hs-pure";
  version = "5.1.0";
  src = fetchgit {
    url = https://github.com/llvm-hs/llvm-hs;
    rev = "872312a8c028cd298d3afd5e29f4ded29bd3f802";
    sha256 = "0c3hx889n22b170cv3gzh0kmhnprad8akh8cqp1ic57v7sdm6m4x";
  } + "/llvm-hs-pure";
  libraryHaskellDepends = [
    attoparsec base bytestring containers mtl template-haskell
    transformers
  ];
  testHaskellDepends = [
    base containers mtl tasty tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}
