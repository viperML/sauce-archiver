let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages;
in
  haskellPackages.shellFor {
    packages = p: [
    ];
    buildInputs = [
      pkgs.zlib
      haskellPackages.haskell-language-server
      haskellPackages.cabal-install
      haskellPackages.cabal-fmt.bin
    ];
  }
