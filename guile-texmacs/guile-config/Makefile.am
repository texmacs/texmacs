## Process this file with Automake to create Makefile.in
## Jim Blandy <jimb@red-bean.com> --- September 1997
##
##  	Copyright (C) 1998, 1999, 2001, 2006, 2007, 2008 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##   
##   GUILE is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##   
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public
##   License along with GUILE; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
##   Floor, Boston, MA 02110-1301 USA

bin_SCRIPTS=guile-config
CLEANFILES=guile-config
EXTRA_DIST=guile-config.in guile.m4 ChangeLog-2008

## FIXME: in the future there will be direct automake support for
## doing this.  When that happens, switch over.
aclocaldir = $(datadir)/aclocal
aclocal_DATA = guile.m4

## We use @-...-@ as the substitution brackets here, instead of the
## usual @...@, so autoconf doesn't go and substitute the values
## directly into the left-hand sides of the sed substitutions.  *sigh*
guile-config: guile-config.in ${top_builddir}/libguile/libpath.h
	rm -f guile-config.tmp
	sed < ${srcdir}/guile-config.in > guile-config.tmp \
	    -e 's|@-bindir-@|${bindir}|' \
	    -e s:@-GUILE_VERSION-@:${GUILE_VERSION}:
	chmod +x guile-config.tmp
	mv guile-config.tmp guile-config

## Get rid of any copies of the configuration script under the old
## name, so people don't end up running ancient copies of it.
install-exec-local:
	rm -f ${bindir}/build-guile
