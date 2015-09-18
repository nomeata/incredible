#!/bin/bash

set -e
set -x

test -d vendor || mkdir vendor
cd vendor
test -e jquery-2.1.4.min.js ||
	wget -c http://code.jquery.com/jquery-2.1.4.min.js
test -d jquery-ui || {
        wget -c http://jqueryui.com/resources/download/jquery-ui-1.11.4.zip
	unzip jquery-ui-1.11.4.zip
	mv jquery-ui-1.11.4 jquery-ui
	rm -f jquery-ui-1.11.4.zip
	}
test -e lodash.min.js ||
	wget -c  http://jointjs.com/js/vendor/lodash/lodash.min.js
test -e backbone-min.js ||
	wget -c  http://jointjs.com/js/vendor/backbone/backbone-min.js
test -e joint.min.js ||
	wget -c http://jointjs.com/downloads/joint.min.js
test -e joint.min.css ||
	wget -c http://jointjs.com/downloads/joint.min.css
