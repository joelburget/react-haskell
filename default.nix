{ cabal, base, deepseq, ghcjsBase, lensFamily, monadsTf
, stdenv, transformers, void
}:
cabal.mkDerivation (self: {
  pname = "react-haskell";
  version = "1.3.0.0";
  src = ./.;
  buildDepends = [
    base deepseq ghcjsBase lensFamily monadsTf transformers void
  ];
  homepage = "https://github.com/joelburget/react-haskell";
  description = "Haskell React bindings";
  meta = {
    license = stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
