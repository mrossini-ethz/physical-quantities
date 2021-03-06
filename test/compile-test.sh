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
		--eval '(asdf:load-system :physical-quantities :force t)' \
		--quit \
		--end-toplevel-options
fi

if [[ "$1" == "cmucl" || "$1" == "all" ]]
then
	lisp	-batch \
		-eval '(require :asdf)' \
		-eval '(asdf:load-system :physical-quantities :force t)' \
		-eval '(quit)'
fi

if [[ "$1" == "ecl" || "$1" == "all" ]]
then
	ecl	-q \
		-eval '(require :asdf)' \
		-eval '(asdf:load-system :physical-quantities :force t)' \
		-eval '(quit)'
fi

if [[ "$1" == "clisp" || "$1" == "all" ]]
then
	clisp	-q \
		-x '(require :asdf)' \
		-x '(asdf:load-system :physical-quantities :force t)'
fi
