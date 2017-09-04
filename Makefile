targets := logic.js logics.js sessions.js

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


prepare-ghcj: js-libs
	cd logic && cabal install -j1 --ghcjs --dependencies-only --disable-tests

prepare-ghc:
	cd logic && cabal install -j1 --dependencies-only --enable-tests

prepare: prepare-ghc prepare-ghcjs

docs:
	cd logic && \
	cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/$$pkg/docs'

test: sessions.js logics.js
	cd logic && \
	cabal test --show-details=streaming
	! which jshint || jshint webui/*.js sessions.js logics.js

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

logics.js: logics/* logic/dist/build/bundle-examples/bundle-examples
	logic/dist/build/bundle-examples/bundle-examples logics > $@

sessions.js: sessions.yaml
	cd logic cabal && \
	(echo -n "sessions = "; cabal run -v0 yaml2json -- ../sessions.yaml ; echo \;) > ../sessions.js
