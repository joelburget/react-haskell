all.js: .cabal-sandbox
	cabal install --ghcjs
	cp .cabal-sandbox/bin/foreign.jsexe/all.js .

build.js:
	webpack

.cabal-sandbox:
	if [ ! -d .cabal-sandbox ]; then \
		cabal sandbox init; \
		cabal sandbox add-source ../..; \
	fi
