#! /bin/sh

mycurl() {
	google-chrome --incognito --user-data-dir=/tmp/aaa --headless=new --disable-gpu --dump-dom $1
}

mycurl "https://xinrenfei.wang/smart-to-do-list-manager-cli/" \
| xmllint --html --xpath '//button' - \
| sed -e 's#.*>\(.*\)<.*#\1#g'

