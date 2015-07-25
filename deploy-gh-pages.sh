#!/bin/bash

# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

make

rm -rf gh-page
mkdir gh-page
cd gh-page
git init
git config user.name "Incredible CI"
git config user.email "mail@joachim-breitner.de"
cp -v ../*.html ../*.js .
git add .
git commit -m "Deploy to GitHub Pages"
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1


