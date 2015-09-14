#!/bin/bash

set -e

dirname="$1"
shift

if [ -z "$dirname" -o -e "$dirname" ]
then
	echo "Usage: $0 dir/"
	echo
	echo "Deploys The Incredible Proof Machine to dir/"
	echo "The output directory dir/ should *not* exist"
	exit 1
fi

if [ ! -e logic.js -o ! -e examples.js ]
then
	echo "Please run \"make\" first."
	exit 1
fi

mkdir $dirname

cp --parents -v -t $dirname \
	*.html  \
	*.js  \
	./vendor/joint.min.css \
	./vendor/jquery-2.1.4.min.js \
	./vendor/jquery-ui/jquery-ui.js \
	./vendor/lodash.min.js \
	./vendor/backbone-min.js \
	./vendor/joint.min.js \
	./webui/*.css \
	./webui/*.js \
	./images/*

if [ -d ".git" ]
then
	desc="$(git describe --tags --always --dirty)"
	echo "incredibleVersion = \"$desc\";" > $dirname/version.js
else
	echo "incredibleVersion = \"UNKNOWN\";" > $dirname/version.js
fi

