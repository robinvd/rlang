{ mkDerivation, array, attoparsec, base, bytestring, Cabal
, containers, exceptions, llvm_5, llvm-hs-pure, mtl
, pretty-show, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, transformers
, utf8-string, fetchgit
}:
mkDerivation {
  pname = "llvm-hs";
  version = "5.1.0";
  src = fetchgit {
    url = https://github.com/llvm-hs/llvm-hs;
    rev = "872312a8c028cd298d3afd5e29f4ded29bd3f802";
    sha256 = "0c3hx889n22b170cv3gzh0kmhnprad8akh8cqp1ic57v7sdm6m4x";
  } + "/llvm-hs";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers exceptions llvm-hs-pure
    mtl template-haskell transformers utf8-string
  ];
  libraryToolDepends = [ llvm_5 ];
  testHaskellDepends = [
    base bytestring containers llvm-hs-pure mtl pretty-show QuickCheck
    tasty tasty-hunit tasty-quickcheck temporary transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
