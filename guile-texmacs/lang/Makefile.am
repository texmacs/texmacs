## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2000, 2006, 2009 Free Software Foundation, Inc.
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

elisp_sources =					\
	elisp/base.scm				\
	elisp/example.el			\
	elisp/interface.scm			\
	elisp/transform.scm			\
	elisp/variables.scm			\
						\
	elisp/primitives/buffers.scm		\
	elisp/primitives/char-table.scm		\
	elisp/primitives/features.scm		\
	elisp/primitives/fns.scm		\
	elisp/primitives/format.scm		\
	elisp/primitives/guile.scm		\
	elisp/primitives/keymaps.scm		\
	elisp/primitives/lists.scm		\
	elisp/primitives/load.scm		\
	elisp/primitives/match.scm		\
	elisp/primitives/numbers.scm		\
	elisp/primitives/pure.scm		\
	elisp/primitives/read.scm		\
	elisp/primitives/signal.scm		\
	elisp/primitives/strings.scm		\
	elisp/primitives/symprop.scm		\
	elisp/primitives/syntax.scm		\
	elisp/primitives/system.scm		\
	elisp/primitives/time.scm		\
						\
	elisp/internals/evaluation.scm		\
	elisp/internals/format.scm		\
	elisp/internals/fset.scm		\
	elisp/internals/lambda.scm		\
	elisp/internals/load.scm		\
	elisp/internals/null.scm		\
	elisp/internals/set.scm			\
	elisp/internals/signal.scm		\
	elisp/internals/time.scm		\
	elisp/internals/trace.scm

subpkgdatadir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)/lang
nobase_subpkgdata_DATA = $(elisp_sources)
TAGS_FILES = $(nobase_subpkgdata_DATA)

EXTRA_DIST = $(elisp_sources) elisp/ChangeLog-2008
