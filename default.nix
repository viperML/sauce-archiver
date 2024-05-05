_:
let
  pkgs = import <nixpkgs> { };
  haskellPackages = pkgs.haskellPackages;
  inherit (pkgs) lib;
in {
  devPkg = haskellPackages.developPackage {
    root = lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.intersection
        (lib.fileset.fromSource (lib.sources.cleanSource ./.))
        (lib.fileset.unions [ ./sauce-archiver.cabal ]);
    };
    returnShellEnv = true;
  };
  shell = pkgs.mkShellNoCC {
    packages =
      [ haskellPackages.cabal-install haskellPackages.haskell-language-server ];
  };
}
