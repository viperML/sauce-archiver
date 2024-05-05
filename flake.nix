{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
      ] (system: function (nixpkgs.legacyPackages.${system}.extend self.overlays.default));

    inherit (nixpkgs) lib;

    src = lib.fileset.toSource {
      root = ./.;
      fileset =
        lib.fileset.intersection
        (lib.fileset.fromSource (lib.sources.cleanSource ./.))
        (lib.fileset.unions [
          ./sauce-archiver.cabal
          ./app
        ]);
    };
  in {
    overlays.default = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = hfinal: hprev: {
          sauce-archiver = final.haskell.lib.overrideCabal (hfinal.callCabal2nix "sauce-archiver" src {}) (old:
            lib.optionalAttrs (final.targetPlatform.isMusl) {
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              configureFlags =
                (old.configureFlags or [])
                ++ [
                  "--ghc-option=-optl=-static"
                ];
            });
        };
      };
    };

    legacyPackages = forAllSystems (pkgs: pkgs);

    packages = forAllSystems (pkgs: {
      default = pkgs.haskellPackages.sauce-archiver;
      static = pkgs.pkgsMusl.haskellPackages.sauce-archiver;
    });

    devShells = forAllSystems (pkgs: {
      pkgShell = pkgs.haskellPackages.developPackage {
        root = src;
        returnShellEnv = true;
      };

      shell = with pkgs;
        mkShellNoCC {
          packages = [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ];
        };
    });
  };
}
