.PHONY: all sandbox

all: sandbox
	cabal install --ghcjs
	cp .cabal-sandbox/bin/animation.jsexe/all.js .

sandbox:
	if [ ! -d .cabal-sandbox ]; then \
		cabal sandbox init; \
		cabal sandbox add-source ../..; \
	fi
