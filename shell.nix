{ sources ? import ./nix/sources.nix }@args:

let pkgs = import ./nix/pkgs.nix args;
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.optics-by-example ];
  nativeBuildInputs = [ pkgs.cabal-install pkgs.haskellPackages.ormolu ];
}
