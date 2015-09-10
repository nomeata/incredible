#!/bin/bash


# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

./deploy.sh gh-page

cp -vr logic/dist/doc/html/incredible-logic/ gh-page/doc

# Prepare an empty directory
cd gh-page
git init
git config user.name "Incredible CI"
git config user.email "mail@joachim-breitner.de"
git add .
git commit -m "Deploy to GitHub Pages"
# The diversion to /dev/null is required to keep the GH_TOKEN secret
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1


