# The Incredible Proof Machine

Welcome to **The Incredible Proof Machine**. The Incredible Proof Machine is a
non-textual interactive theorem prover, or at least it will hopefully become
one.

If you want to try it out, go to <http://nomeata.github.io/incredible/> for the
current state of the graphical UI and <http://nomeata.github.io/incredible/>
for the text-based debug interface.


The project consists of both Haskell and JavaScript code, so there are a few
dependencies to install.

## Building the Logic Core

The Logic core is implemented in Haskell, and compiled to JavaScript using
[GHCJS](https://github.com/ghcjs/ghcjs). See there for more detailed
instructions, but here is a quick way.

  * Install GHC version 7.8 and cabal-install version 1.22. On Ubuntu, run

	$ add-apt-repository -y ppa:hvr/ghc
	$ apt-get install cabal-install-1.22  alex-3.1.4 happy-1.19.5 ghc-7.8.4
	$ export PATH=$HOME/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/cabal/1.22/bin:/opt/alex/3.1.4/bin:/opt/happy/1.19.5/bin:$PATH

  * Run `cabal update`
  * Make sure the Cabal library is at least version 1.22. You can check that
    using `ghc-pkg find Cabal` and install the latest version using `cabal
    install Cabal`
  * Now you can install GHCJS including most of its libraries

	git clone https://github.com/ghcjs/ghcjs-prim.git
	git clone https://github.com/ghcjs/haddock-internal.git
	git clone https://github.com/ghcjs/ghcjs.git
	cabal install ./ghcjs-prim ./haddock-internal ./ghcjs
	ghcjs-boot --dev --no-prof

  * Now you should be able to compile both `logic.js` and `examples.js` by running `make`

Alternatively, if you do not want to hack on these parts of the project, but simply run them locally, you can run

    git checkout origin/gh-pages logic.js examples.js


You can check that this part is working by opening `text-ui.html` in your browser.

## Installing JavaScript dependencies

The JavaScript part of the project uses a few external libraries. These are most easily obtained using npm.

  * Install npm, either using `apt-get install npm` or [by other
    means](http://blog.npmjs.org/post/85484771375/how-to-install-npm)
  * Run `npm install`
  * Open `index.html`

## Continuous integration

Every push to the repository is tested on
[Travis](https://travis-ci.org/nomeata/incredible). Until we have proper tests,
this makes sure that the Haskell code compiles both under GHC and GHCJS, and
that the JavaScript dependencies can be installed.

## Continuous deployment

If Travis thinks the build succeeds, it uses the deploy script in
`deploy-gh-pages.sh` to push all files required to run the project to
<http://nomeata.github.io/incredible/>. As this is a [Github
Pages](http://pages.github.com/) page, this means that it pushes the files to
the `gh-pages` branch of the repository. You can ignore that branch otherwise.

## Contact

Please contact Joachim Breitner <mail@joachim-breitner.de> if you have
questions or want to help out.


