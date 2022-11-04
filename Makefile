targets := logic.js logics.js sessions.js


all: ${targets}

clean:
	${RM} -f ${targets} bundle-examples yaml2json
	rm -rf logic/dist*

vendor:
	mkdir vendor

js-libs:
	./install-jslibs.sh

prepare-ghcjs: js-libs
	cd logic && cabal build --distdir=dist-newstyle-ghcjs --ghcjs --dependencies-only --disable-tests

prepare-ghc:
	cd logic && cabal build --dependencies-only --disable-tests

prepare: prepare-ghc prepare-ghcjs


test-bench:
	# check that the benchmarks have not bitrotted
	cd logic && cabal run bench -- -n 1

test-hs:
	cd logic && cabal test --test-show-details=direct

test-js: webui/*.js sessions.js logics.js
	jshint webui/*.js sessions.js logics.js

test: test-js test-hs test-bench

logic.js: logic/*.cabal logic/*.hs logic/js/*.hs logic/js/*.js
	cd logic && cabal new-build --distdir=dist-newstyle-ghcjs --ghcjs
	# js-sources in the cabal file stopped working? So concatenate manually
	# related to https://github.com/haskell/cabal/pull/5443 maybe?
	cat logic/js/js-interface-wrapper.js > $@
	cat logic/dist-newstyle-ghcjs/build/*/*/incredible-logic-0.1/x/js-interface/build/js-interface/js-interface.jsexe/all.js >> $@

bundle-examples: logic/*.cabal logic/*.hs logic/examples/*.hs
	cd logic && cabal new-build
	cp -v logic/dist-newstyle/build/*/*/incredible-logic-0.1/x/bundle-examples/build/bundle-examples/bundle-examples $@

logics.js: logics/* bundle-examples
	./bundle-examples logics > $@

yaml2json: logic/*.cabal logic/*.hs logic/examples/*.hs
	cd logic && cabal new-build
	cp -v logic/dist-newstyle/build/*/*/incredible-logic-0.1/x/yaml2json/build/yaml2json/yaml2json $@


sessions.js: yaml2json sessions.yaml
	(echo -n "sessions = "; ./yaml2json sessions.yaml ; echo \;) > $@
