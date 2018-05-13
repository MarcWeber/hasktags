{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
