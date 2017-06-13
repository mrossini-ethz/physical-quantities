#!/bin/bash

if [[ $# != 1 ]]
then
	echo "Argument error!"
	echo "Usage: $0 <sbcl|cmucl|ecl|clisp|all>"
	exit 1
fi

if [[ "$1" == "sbcl" || "$1" == "all" ]]
then
	sbcl	--noinform \
		--disable-ldb \
		--lose-on-corruption \
		--end-runtime-options \
		--no-sysinit \
		--no-userinit \
		--disable-debugger \
		--eval '(require :asdf)' \
		--eval '(asdf:test-system :physical-quantities)' \
		--quit \
		--end-toplevel-options
fi

if [[ "$1" == "cmucl" || "$1" == "all" ]]
then
	lisp	-batch \
		-quiet \
		-eval '(require :asdf)' \
		-eval '(asdf:test-system :physical-quantities)' \
		-eval '(quit)'
fi

if [[ "$1" == "ecl" || "$1" == "all" ]]
then
	ecl	-q \
                -eval '(setf *suppress-compiler-messages* t)' \
		-eval '(require :asdf)' \
		-eval '(asdf:test-system :physical-quantities)' \
		-eval '(quit)'
fi

if [[ "$1" == "clisp" || "$1" == "all" ]]
then
	clisp	-q \
                -q \
		-x '(require :asdf)' \
		-x '(asdf:test-system :physical-quantities)'
fi
