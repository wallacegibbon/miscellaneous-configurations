#! /bin/sh

if test $# -lt 1; then
	echo Usage: fifo_demo.sh /path/to/fifo_file.
	exit 1
fi

if test -e $1; then
	echo File "$1" already exist, choose another file instead.
	exit 2
fi

mkfifo $1

for i in $(seq 3); do
	cat $1 | sed -e 's/[a-z]/\u&/g'
done

rm $1

