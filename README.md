# The Incredible Proof Machine

Welcome to **The Incredible Proof Machine**. The Incredible Proof Machine is a
non-textual interactive theorem prover, or at least it will hopefully become
one.

If you want to try it out, go to <http://incredible.pm/>.

The project consists of both Haskell and JavaScript code, so there are a few
dependencies to install.

## Building the Logic Core

The Logic core is implemented in Haskell, and compiled to JavaScript using
[GHCJS](https://github.com/ghcjs/ghcjs). Installing GHCJS is not trivial, so we
are using [nix](https://nixos.org/) to obtain all build instructions. So to
build the Haskell parts

1. Install `nix` as described at <https://nixos.org/download.html>

2. Add the Kaleidogen nix cache at <https://kaleidogen.cachix.org/>.

   (This step is optional, but without it you will rebuild GHCJS which will take a long time.

3. Download or build all build dependencies:
   ```
   nix-build --no-out-link shell.nix
   ```
   (Also optional, the next step will do it if you missed it.)
4. Enter a nix-shell:
   ```
   nix-shell
   ```
   You should now have `ghcjs` in your path.
5. Build everything
   ```
   make
   ```
   (See `Makefile` for the individual steps)
6. Open `./index.html` in your browser and play!

7. To run the testsuite, run `make test`

## Hacking on the UI without building Haskell

If you only want to run this locally, or work on the UI (which is written in
plain JavaScript in folder `webui/`), you can also run

    wget http://incredible.pm/logic.js -O logic.js
    wget http://incredible.pm/sessions.js -O sessions.js

The JavaScript part of the project uses a few external libraries. To obtain
these, run `./install-jslib.sh`.

## Continuous integration and deployment

Every push to the repository is tested on
[GithubActions](https://github.com/nomeata/incredible/actions), and if the
tests succeed pushes to `master` are automatically pushed to
`https://incredible.pm/`.

This uses the script script `./deploy.sh dir/`, which copies all files needed
to run the Incredible Proof Machien into the directory `dir/`, which should not
exist before.

## Continuous deployment


## Contact

Please contact Joachim Breitner <mail@joachim-breitner.de> if you have question or want to help out.
