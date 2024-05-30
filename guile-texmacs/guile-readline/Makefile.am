## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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

SUBDIRS = ice-9

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I. -I.. -I$(srcdir)/..

GUILE_SNARF = ../libguile/guile-snarf

lib_LTLIBRARIES = libguilereadline-v-@LIBGUILEREADLINE_MAJOR@.la

libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_SOURCES = readline.c
libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_LIBADD = ../libguile/libguile.la
libguilereadline_v_@LIBGUILEREADLINE_MAJOR@_la_LDFLAGS = -version-info @LIBGUILEREADLINE_INTERFACE@ -export-dynamic -no-undefined


BUILT_SOURCES = readline.x

pkginclude_HEADERS = readline.h

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

EXTRA_DIST = LIBGUILEREADLINE-VERSION ChangeLog-2008

MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

CLEANFILES = *.x

dist-hook:
	(temp="/tmp/mangle-deps.$$$$"; \
	 trap "rm -f $$temp" 0 1 2 15; \
	 sed -e 's|\([ 	]\)\(\.\./\(\|libguile/\|guile-readline/\)[a-z_-]*\.h\)|\1$$(srcdir)/\2|g' $(distdir)/Makefile.in \
	 | sed -e 's|\$$(srcdir)/\(\.\./libguile/\(libpath\|scmconfig\|versiondat\)\)|\1|g' \
	 | sed -e 's|\.\./guile-readline/\([a-z_-]*\.x\)|\1|g' > $$temp \
	 && chmod u+w $(distdir)/Makefile.in && cp -p $$temp $(distdir)/Makefile.in)

