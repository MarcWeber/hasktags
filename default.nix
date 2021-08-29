{ sources ? import ./nix/sources.nix
, haskellNix ? import sources."haskell.nix" {}
, hnixPkgs ? haskellNix.sources.nixpkgs-unstable
, hnixArgs ? haskellNix.nixpkgsArgs
, pkgs ? import hnixPkgs hnixArgs
, compiler ? "ghc8105" }:

pkgs.haskell-nix.project {
  compiler-nix-name = compiler;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "hasktags";
    src = ./.;
  };
}
