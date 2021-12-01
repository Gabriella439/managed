let
  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        pkgsNew.lib.composeExtensions (old.overrides or (_: _: { }))
          (pkgsNew.haskell.lib.packageSourceOverrides {
            managed = ./.;
          });
    });
  };

  pkgs = import <nixpkgs> { config = { }; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskellPackages) managed;
  }
