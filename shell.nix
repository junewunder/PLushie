{ pkgs ? import <nixpkgs> {} }:
  with pkgs;
  mkShell {
    nativeBuildInputs = [
      llvm
      ghc
      stack
      haskell-language-server
    ];
  }