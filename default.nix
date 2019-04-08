{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./pty-wrapper.nix { }
