#!/bin/bash

# Small script to create one large java script file with the whole logic.
# Requires a working ghcjs and cabal installation in the path.

set -e

cd logic
cabal configure --ghcjs
cabal build
cp dist/build/js-interface/js-interface.jsexe/all.js ../logic.js

