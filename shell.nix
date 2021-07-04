{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.ormolu
  ];
}
