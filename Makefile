all: logic.js examples.js

logic/dist-ghcjs/build/js-interface/js-interface.jsexe/all.js:
	cd logic && \
	cabal configure --distdir=dist-ghcjs --ghcjs --disable-library-profiling && \
	cabal build --distdir=dist-ghcjs

logic.js: logic/dist-ghcjs/build/js-interface/js-interface.jsexe/all.js
	cp -v $< $@

logic/dist/build/bundle-examples/bundle-examples:
	cd logic && \
	cabal configure --enable-tests --disable-library-profiling && \
	cabal build

examples.js: examples/*/* logic/dist/build/bundle-examples/bundle-examples
	logic/dist/build/bundle-examples/bundle-examples examples > $@
