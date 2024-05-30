## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2006, 2007, 2008 Free Software Foundation, Inc.
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

# want automake 1.10 or higher so that AM_GNU_GETTEXT can tell automake that
# config.rpath is needed
#
AUTOMAKE_OPTIONS = 1.10

SUBDIRS = oop libguile ice-9 guile-config guile-readline emacs \
	  scripts srfi doc examples test-suite benchmark-suite lang am

bin_SCRIPTS = guile-tools

include_HEADERS = libguile.h

EXTRA_DIST = LICENSE HACKING GUILE-VERSION guile-1.8.pc.in	\
	     ChangeLog-2008 m4/autobuild.m4

TESTS = check-guile

ACLOCAL_AMFLAGS = -I m4

DISTCLEANFILES = check-guile.log

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-1.8.pc

# Makefile.am ends here
