with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        ghcjs-base = haskellngPackages.ghcjs-base;
        ghcjs-prim = haskellngPackages.ghcjs-prim;
        react-haskell = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.react-haskell.env
