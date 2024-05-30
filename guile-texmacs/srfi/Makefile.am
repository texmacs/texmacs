## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
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

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@ @EXTRA_DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
AM_CPPFLAGS = -I.. -I$(srcdir)/..

srfiincludedir = $(pkgincludedir)/srfi

# These headers are visible as <guile/srfi/mumble.h>
srfiinclude_HEADERS = srfi-1.h srfi-4.h srfi-13.h srfi-14.h srfi-60.h

lib_LTLIBRARIES = \
  libguile-srfi-srfi-1-v-@LIBGUILE_SRFI_SRFI_1_MAJOR@.la \
  libguile-srfi-srfi-4-v-@LIBGUILE_SRFI_SRFI_4_MAJOR@.la \
  libguile-srfi-srfi-13-14-v-@LIBGUILE_SRFI_SRFI_13_14_MAJOR@.la \
  libguile-srfi-srfi-60-v-@LIBGUILE_SRFI_SRFI_60_MAJOR@.la

BUILT_SOURCES = srfi-1.x srfi-4.x srfi-13.x srfi-14.x srfi-60.x

libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_SOURCES = srfi-1.x srfi-1.c
libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_LIBADD = ../libguile/libguile.la
libguile_srfi_srfi_1_v_@LIBGUILE_SRFI_SRFI_1_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_1_INTERFACE@

libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_SOURCES = srfi-4.x srfi-4.c
libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_LIBADD = ../libguile/libguile.la
libguile_srfi_srfi_4_v_@LIBGUILE_SRFI_SRFI_4_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_4_INTERFACE@

libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_SOURCES = srfi-13.x srfi-13.c srfi-14.x srfi-14.c
libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_LIBADD = ../libguile/libguile.la
libguile_srfi_srfi_13_14_v_@LIBGUILE_SRFI_SRFI_13_14_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_13_14_INTERFACE@

libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_SOURCES = srfi-60.x srfi-60.c
libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_LIBADD = ../libguile/libguile.la
libguile_srfi_srfi_60_v_@LIBGUILE_SRFI_SRFI_60_MAJOR@_la_LDFLAGS = -no-undefined -export-dynamic -version-info @LIBGUILE_SRFI_SRFI_60_INTERFACE@

srfidir = $(datadir)/guile/$(GUILE_EFFECTIVE_VERSION)/srfi
srfi_DATA = srfi-1.scm \
            srfi-2.scm \
	    srfi-4.scm \
            srfi-6.scm \
            srfi-8.scm \
            srfi-9.scm \
            srfi-10.scm \
            srfi-11.scm \
            srfi-13.scm \
            srfi-14.scm \
	    srfi-16.scm \
            srfi-17.scm \
            srfi-19.scm \
            srfi-26.scm \
            srfi-31.scm \
            srfi-34.scm \
	    srfi-35.scm \
            srfi-37.scm \
            srfi-39.scm \
            srfi-60.scm \
	    srfi-69.scm \
	    srfi-88.scm

EXTRA_DIST = $(srfi_DATA) ChangeLog-2008
TAGS_FILES = $(srfi_DATA)

GUILE_SNARF = ../libguile/guile-snarf

MKDEP = gcc -M -MG $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
