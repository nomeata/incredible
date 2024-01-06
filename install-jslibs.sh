#!/usr/bin/env bash

set -e
set -x

test -d vendor || mkdir vendor
cd vendor
test -e jquery-3.6.4.min.js ||
	wget -c http://code.jquery.com/jquery-3.6.4.min.js
test -d jquery-ui || {
        wget -c http://jqueryui.com/resources/download/jquery-ui-1.11.4.zip
	unzip jquery-ui-1.11.4.zip
	mv jquery-ui-1.11.4 jquery-ui
	rm -f jquery-ui-1.11.4.zip
	}
test -e jquery.ui.touch-punch.min.js ||
	wget -c https://raw.githubusercontent.com/furf/jquery-ui-touch-punch/master/jquery.ui.touch-punch.min.js
test -e lodash.min.js ||
	wget -c https://cdnjs.cloudflare.com/ajax/libs/lodash.js/3.10.1/lodash.min.js
test -e backbone-min.js ||
	wget -c https://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.2.1/backbone-min.js
test -e joint.min.js ||
	wget -c https://cdnjs.cloudflare.com/ajax/libs/jointjs/0.9.6/joint.min.js
test -e joint.min.css ||
	wget -c https://cdnjs.cloudflare.com/ajax/libs/jointjs/0.9.6/joint.min.css
test -e i18next-1.10.1.min.js ||
        wget -c https://raw.githubusercontent.com/i18next/i18next/1.10.1/release/i18next-1.10.1.min.js
test -e FileSaver.min.js ||
        wget -c https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js
