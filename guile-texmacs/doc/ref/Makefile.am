## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 2004, 2006, 2008 Free Software Foundation, Inc.
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

info_TEXINFOS = guile.texi

guile_TEXINFOS = preface.texi			\
                 intro.texi			\
                 libguile-program.texi		\
                 scheme-intro.texi		\
		 api-scm.texi			\
		 api-snarf.texi			\
		 api-smobs.texi			\
		 scheme-ideas.texi		\
		 api-data.texi			\
		 api-procedures.texi		\
		 api-utility.texi		\
		 api-binding.texi		\
		 api-control.texi		\
		 api-io.texi			\
		 api-evaluation.texi		\
		 api-memory.texi		\
		 api-modules.texi		\
		 api-scheduling.texi		\
		 api-options.texi		\
		 api-translation.texi		\
		 api-i18n.texi			\
		 api-debug.texi			\
		 scheme-reading.texi		\
		 scheme-indices.texi		\
		 slib.texi			\
		 posix.texi			\
		 expect.texi			\
		 scsh.texi			\
		 tcltk.texi			\
		 scheme-scripts.texi		\
		 gh.texi			\
		 api-overview.texi		\
		 scheme-debugging.texi		\
		 scheme-using.texi		\
		 indices.texi			\
		 script-getopt.texi		\
		 data-rep.texi			\
		 repl-modules.texi		\
		 srfi-modules.texi		\
		 misc-modules.texi		\
		 api-compound.texi		\
		 autoconf.texi			\
		 autoconf-macros.texi		\
		 tools.texi			\
		 fdl.texi			\
		 libguile-concepts.texi		\
		 libguile-smobs.texi		\
		 libguile-snarf.texi		\
		 libguile-linking.texi		\
	 	 libguile-extensions.texi	\
		 api-init.texi			\
		 mod-getopt-long.texi

ETAGS_ARGS = $(info_TEXINFOS) $(guile_TEXINFOS)

EXTRA_DIST = ChangeLog-2008

include $(top_srcdir)/am/pre-inst-guile

# Automated snarfing

autoconf.texi: autoconf-macros.texi
autoconf-macros.texi: $(top_srcdir)/guile-config/guile.m4
	GUILE="$(GUILE_FOR_BUILD)" $(top_srcdir)/scripts/snarf-guile-m4-docs $(top_srcdir)/guile-config/guile.m4 \
	  > $(srcdir)/$@

MAINTAINERCLEANFILES = autoconf-macros.texi
