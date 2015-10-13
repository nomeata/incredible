# The Incredible Proof Machine

Welcome to **The Incredible Proof Machine**. The Incredible Proof Machine is a
non-textual interactive theorem prover, or at least it will hopefully become
one.

If you want to try it out, go to <http://incredible.nomeata.de/> for the
current state of the graphical UI and <http://incredible.nomeata.de/text-ui.html>
for the text-based debug interface.


The project consists of both Haskell and JavaScript code, so there are a few
dependencies to install.

## Building the Logic Core

The Logic core is implemented in Haskell, and compiled to JavaScript using
[GHCJS](https://github.com/ghcjs/ghcjs). See there for more detailed
instructions, but here is a quick way.

  * Install GHC version 7.8 and cabal-install version 1.22. On Ubuntu, run

        $ add-apt-repository -y ppa:hvr/ghc
        $ apt-get update
        $ apt-get install cabal-install-1.22  alex-3.1.4 happy-1.19.5 ghc-7.8.4
        $ export PATH=$HOME/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/cabal/1.22/bin:/opt/alex/3.1.4/bin:/opt/happy/1.19.5/bin:$PATH

  * Run `cabal update`
  * Make sure the Cabal library is at least version 1.22. You can check that
    using `ghc-pkg find Cabal` and install the latest version using `cabal
    install Cabal`
  * Now you can install GHCJS including most of its libraries

        git clone --recursive https://github.com/ghcjs/ghcjs.git -b old-base-ghc-7.8
        git clone https://github.com/ghcjs/haddock-internal.git
        git clone https://github.com/ghcjs/ghcjs.git
        cabal install ./ghcjs ./ghcjs/lib/ghcjs-prim ./haddock-internal
        ghcjs-boot --dev --no-prof --ghcjs-boot-dev-branch old-base-ghc-7.8 --shims-dev-branch old-base-ghc-7.8

  * Install any further dependencies

        cd logic; cabal install --dependencies-only --enable-tests
        cd logic; cabal install --ghcjs --dependencies-only --disable-tests

    or run `make prepare` in the project root.

  * Now you should be able to compile both `logic.js` and `examples.js` by running `make`

  * To run the testsuite, run `cabal test` in the `logic/` directory or `make
    test` in the project root

It should also work with GHC-7.10. In this case, replace `old-base-ghc-7.8` by
`old-base-ghc-7.10` in the above instructions.

Alternatively, if you do not want to hack on these parts of the project, but simply run them locally, you can run

    wget http://incredible.nomeata.de/logic.js -O logic.js
    wget http://incredible.nomeata.de/examples.js -O examples.js

You can check that this part is working by opening `text-ui.html` in your browser.

## Installing JavaScript dependencies

The JavaScript part of the project uses a few external libraries. To obtain these, run `./install-jslib.sh`.

## Continuous integration

Every push to the repository is tested on
[Travis](https://travis-ci.org/nomeata/incredible). Until we have proper tests,
this makes sure that the Haskell code compiles both under GHC and GHCJS, and
that the JavaScript dependencies can be installed.

## Deployment

Running the script `./deploy.sh dir/` should put all files required for the
runtime into the directory `dir/`, which should not exist before.

## Continuous deployment

If Travis thinks the build succeeds, it uses the above deploy script, together
with `.travis-push-gh-pages.sh`, to push the final result to to
<http://incredible.nomeata.de/>.
As this is a [Github Pages](http://pages.github.com/) page, this means that the
sources are always the contents of the `gh-pages` branch of the
`incredible-demo` repository, which is otherwise unused.

## Contact

Please join [our the mailing list](https://lists.nomeata.de/mailman/listinfo/incredible) (or directly Joachim Breitner <mail@joachim-breitner.de> if preferred) if you have question or want to help out.
