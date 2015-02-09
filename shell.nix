{ }:

with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./. {};
        mvc = self.callPackage ./mvc.nix {};
      };
    };

in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: {
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
       };
     };
     inherit (haskellPackages) base deepseq ghcjsBase lensFamily monadsTf transformers void;
   }
