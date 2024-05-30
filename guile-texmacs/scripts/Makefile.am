## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2002, 2006, 2008 Free Software Foundation, Inc.
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
scripts_sources =				\
	PROGRAM					\
	autofrisk				\
	display-commentary			\
	doc-snarf				\
	frisk					\
	generate-autoload			\
	lint					\
	punify					\
	read-scheme-source			\
	read-text-outline			\
	use2dot					\
	snarf-check-and-output-texi		\
	summarize-guile-TODO			\
	scan-api				\
	api-diff				\
	read-rfc822				\
	snarf-guile-m4-docs

subpkgdatadir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)/scripts
subpkgdata_SCRIPTS = $(scripts_sources)

EXTRA_DIST = $(scripts_sources) ChangeLog-2008

list:
	@echo $(scripts_sources)

include $(top_srcdir)/am/pre-inst-guile

overview: $(scripts_sources)
	@echo '----------------------------'
	@echo Overview
	@echo I. Commentaries
	@echo II. Module Interfaces
	@echo '----------------------------'
	@echo I. Commentaries
	@echo '----------------------------'
	$(preinstguiletool)/display-commentary $^
	@echo '----------------------------'
	@echo II. Module Interfaces
	@echo '----------------------------'
	$(preinstguiletool)/frisk $^

# Makefile.am ends here
