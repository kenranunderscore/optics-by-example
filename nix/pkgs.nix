{ sources ? import ./sources.nix }:

let
  overlay = hfinal: hprev: {
    optics-by-example = hfinal.callCabal2nix "optics-by-example" ../. { };
  };
in import sources.nixpkgs {
  overlays = [
    (final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides =
          prev.lib.composeExtensions (old.overrides or (_: _: { })) overlay;
      });
    })
  ];
}
