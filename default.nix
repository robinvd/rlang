{system ? builtins.currentSystem}:
let
  pkgs = import <nixpkgs> {inherit system; };
  myCall = pkgs.lib.callPackageWith (pkgs // pkgs.haskellPackages // jobs);
  s = pkgs.fetchgit {
    url = https://github.com/llvm-hs/llvm-hs-pretty;
    rev = "7e426813eeb3a7e6337ce78674260bbae35d40ed";
    sha256 = "0l8dkxk36l5zlhmv26bxmxbck4g9nhdw6kakpcp32v8xr76i9bid";
  };

  jobs = pkgs.lib.attrsets.mapAttrs (name: value: pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck value)) {
    llvm-hs-pretty = myCall s {};
    llvm-hs = myCall ./llvm-hs.nix {};
    llvm-hs-pure = myCall ./llvm-hs-pure.nix {};

  };
  rlang = myCall ./app.nix {};
in
  if pkgs.lib.inNixShell then rlang.env else rlang
