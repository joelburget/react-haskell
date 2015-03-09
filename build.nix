{ }:

with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        react-haskell = self.callPackage ./. {};
      };
    };
    cabal = haskellPackages.cabal.override {
      extension = self: super: {
        buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
      };
    };

in with haskellPackages; cabal.mkDerivation (self: {
  pname = "reactHaskellGhcjs";
  version = "1.3.0.0";
  src = ./.;
  buildDepends = [
    deepseq ghcjsBase ghcjsDom ghcjsPrim lensFamily monadsTf
    transformers void
  ];
  meta = {
    homepage = "https://github.com/joelburget/react-haskell";
    description = "Haskell React bindings";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
