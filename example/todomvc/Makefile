.PHONY: all

all: .cabal-sandbox
	cabal install --ghcjs
	cp .cabal-sandbox/bin/react-haskell-todomvc.jsexe/all.js .

.cabal-sandbox:
	if [ ! -d .cabal-sandbox ]; then \
		cabal sandbox init; \
		cabal sandbox add-source ../..; \
	fi
