{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hasktags.nix { }
