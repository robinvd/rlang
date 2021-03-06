{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, llvm-hs
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
