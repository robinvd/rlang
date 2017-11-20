{ mkDerivation, base, bytestring, containers, llvm-hs
, llvm-hs-pretty, llvm-hs-pure, mtl, parsec, pretty-simple, stdenv
, text
}:
mkDerivation {
  pname = "rlang";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers llvm-hs llvm-hs-pretty llvm-hs-pure mtl
    parsec pretty-simple text
  ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/robinvd/rlang#readme";
  description = "A new Haskeleton package";
  license = stdenv.lib.licenses.mit;
}
