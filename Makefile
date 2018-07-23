targets := logic.js logics.js sessions.js

CABAL_FLAGS=--allow-newer=template-haskell --constraint 'primitive<0.6.4'

all: ${targets}

clean:
	${RM} ${targets} bundle-examples
	rm -rf logic/dist-*

vendor:
	mkdir vendor

js-libs:
	./install-jslibs.sh


prepare-ghcjs: js-libs
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-build --distdir=dist-newstyle-ghcjs --ghcjs $(CABAL_FLAGS) --dependencies-only --disable-tests

prepare-ghc:
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-build $(CABAL_FLAGS) --dependencies-only --disable-tests

prepare: prepare-ghc prepare-ghcjs

test: sessions.js logics.js
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-test $(CABAL_FLAGS) --show-details=streaming
	! which jshint || jshint webui/*.js sessions.js logics.js

logic.js: logic/*.cabal logic/*.hs logic/js/*.hs
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-build --distdir=dist-newstyle-ghcjs --ghcjs $(CABAL_FLAGS)
	cp -v logic/dist-newstyle-ghcjs/build/*/*/incredible-logic-0.1/x/js-interface/build/js-interface/js-interface.jsexe/all.js $@

bundle-examples: logic/*.cabal logic/*.hs logic/examples/*.hs
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-build $(CABAL_FLAGS)
	cp -v logic/dist-newstyle/build/*/*/incredible-logic-0.1/x/bundle-examples/build/bundle-examples/bundle-examples $@

logics.js: logics/* bundle-examples
	./bundle-examples logics > $@

yaml2json: logic/*.cabal logic/*.hs logic/examples/*.hs
	rm -vf logic/.ghc.environment*
	cd logic && cabal new-build $(CABAL_FLAGS)
	cp -v logic/dist-newstyle/build/*/*/incredible-logic-0.1/x/yaml2json/build/yaml2json/yaml2json $@


sessions.js: yaml2json sessions.yaml
	(echo -n "sessions = "; ./yaml2json sessions.yaml ; echo \;) > $@
