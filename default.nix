{ mkDerivation, base, deepseq, ghcjs-base, ghcjs-prim
, lens-family, monads-tf, stdenv, transformers, void
}:
mkDerivation {
  pname = "react-haskell-ghcjs";
  version = "1.3.0.0";
  src = ./.;
  buildDepends = [
    base deepseq ghcjs-base ghcjs-prim lens-family monads-tf
    transformers void
  ];
  homepage = "https://github.com/joelburget/react-haskell";
  description = "Haskell React bindings";
  license = stdenv.lib.licenses.mit;
}
