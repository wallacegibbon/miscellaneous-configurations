#! /bin/sh

## this works like the `realpath` command.

abs_filename() {
	echo $(cd "$(dirname "$1")" && pwd)/$(basename "$1")
}

abs_filename $1

