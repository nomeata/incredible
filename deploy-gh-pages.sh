#!/bin/bash

set -e

# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

# Prepare an empty directory
rm -rf gh-page
mkdir gh-page
cd gh-page
git init
git config user.name "Incredible CI"
git config user.email "mail@joachim-breitner.de"


# Make sure the project is built
make -C ..

# Copy static HTML files and generated JS files
cp -v ../*.html ../*.js .

# Copy the required javascript files from the node package. Is there
# npm-support to do this properly?
cd ..
cp --parents -v -t gh-page \
	./node_modules/jointjs/dist/joint.all.css \
	./node_modules/jquery/dist/jquery.min.js \
	./node_modules/lodash/index.js \
	./node_modules/backbone/backbone-min.js \
	./node_modules/jointjs/dist/joint.all.js \
	./webui/*.css \
	./webui/*.js \
    ./images/*
cd gh-page

git add .
git commit -m "Deploy to GitHub Pages"
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1


