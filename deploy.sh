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

cp --parents -r -v -t $dirname \
	*.html  \
	*.js  \
	./vendor \
	./fonts

if [ -d ".git" ]
then
	desc="$(git describe --tags --always --dirty)"
	echo "incredibleVersion = \"$desc\";" > $dirname/version.js
else
	echo "incredibleVersion = \"UNKNOWN\";" > $dirname/version.js
fi

