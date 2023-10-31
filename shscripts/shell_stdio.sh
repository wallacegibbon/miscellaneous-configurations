#! /bin/sh

# Usage: date | shscripts/shell_stdio.sh -e 's/[a-z]/\u&/g'
sed $@

## When calling this `sed`, the standard input of `sed` is the standard input of this script.

