#! /bin/bash

blah1() {
	echo pid of blah1: $BASHPID
}

blah2() (
	echo pid of blah2: $BASHPID
)

echo pid of parent: $BASHPID

echo ======================================================================
echo Call function directly
echo ======================================================================
blah1
blah2
echo

echo ======================================================================
echo Call function in pipe
echo ======================================================================
echo hello | blah1
echo hello | blah2
echo

echo ======================================================================
echo Call function in subshell
echo ======================================================================
echo $(blah1)
echo

echo ======================================================================
echo Call function background
echo ======================================================================
blah1&
blah2&

