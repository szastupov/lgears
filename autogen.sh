#!/bin/sh

if [ "$1" = "clean" ]; then
	if [ -f Makefile ]; then
		make maintainer-clean
	fi
	find . -name Makefile.in -delete
	rm -rf aclocal.m4 config.h.in configure depcomp install-sh missing
else
	autoreconf -i && ./configure $@
fi
