## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 2002, 2006, 2008 Free Software Foundation, Inc.
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

SUBDIRS = ref tutorial goops r5rs

dist_man1_MANS = guile.1

EXAMPLE_SMOB_FILES = \
    ChangeLog-2008 Makefile README image-type.c image-type.h myguile.c

OLDFMT = oldfmt.c

dist-hook:
	cp $(srcdir)/$(OLDFMT) $(distdir)/
	mkdir $(distdir)/example-smob
	for f in $(EXAMPLE_SMOB_FILES); do \
	  cp $(srcdir)/example-smob/$$f $(distdir)/example-smob/; \
        done

EXTRA_DIST = groupings.alist ChangeLog-2008 # guile-api.alist

include $(top_srcdir)/am/maintainer-dirs
guile-api.alist: guile-api.alist-FORCE
	( cd $(top_builddir) ; $(mscripts)/update-guile-api.alist )
guile-api.alist-FORCE:

