targets := logic.js examples.js

all: ${targets}

clean:
	${RM} ${targets}
	cd logic && \
	cabal clean && \
	cabal clean --distdir=dist-ghcjs

vendor:
	mkdir vendor

js-libs:
	./install-jslibs.sh


prepare: js-libs
	cd logic && \
	cabal install --dependencies-only --enable-tests && \
	cabal install --ghcjs --dependencies-only --disable-tests

docs:
	cd logic && \
	cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/$$pkg/docs'

test: all
	cd logic && \
	cabal test --show-details=streaming

logic/dist-ghcjs/build/js-interface/js-interface.jsexe/all.js: logic/*.hs logic/js/*.hs
	cd logic && \
	cabal configure --distdir=dist-ghcjs --ghcjs --disable-library-profiling && \
	cabal build --distdir=dist-ghcjs

logic.js: logic/dist-ghcjs/build/js-interface/js-interface.jsexe/all.js
	cp -v $< $@

logic/dist/build/bundle-examples/bundle-examples: logic/*.hs logic/examples/*.hs
	cd logic && \
	cabal configure --enable-tests --disable-library-profiling && \
	cabal build

examples.js: examples/*/* logic/dist/build/bundle-examples/bundle-examples
	logic/dist/build/bundle-examples/bundle-examples examples > $@

sessions.js: sessions.yaml
	cd logic cabal && \
	(echo -n "sessions = "; cabal run -v0 yaml2json -- ../sessions.yaml ; echo \;) > ../sessions.js
