## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2000, 2001, 2004, 2005, 2006 Free Software Foundation, Inc.
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

# These should be installed and distributed.
goops_sources =							\
    active-slot.scm compile.scm composite-slot.scm describe.scm	\
    dispatch.scm internal.scm save.scm stklos.scm util.scm      \
    old-define-method.scm accessors.scm simple.scm

subpkgdatadir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)/oop/goops
subpkgdata_DATA = $(goops_sources)
TAGS_FILES = $(subpkgdata_DATA)

EXTRA_DIST = $(goops_sources)
