## Process this file with automake to produce Makefile.in.
##
## Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Software Foundation, Inc.
##
## This file is part of GUILE.
##
## GUILE is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2, or
## (at your option) any later version.
##
## GUILE is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with GUILE; see the file COPYING.  If not, write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301 USA

SUBDIRS = standalone

SCM_TESTS = tests/alist.test			\
	    tests/and-let-star.test		\
	    tests/arbiters.test			\
	    tests/bit-operations.test		\
	    tests/c-api.test			\
	    tests/chars.test			\
	    tests/common-list.test		\
	    tests/continuations.test		\
	    tests/elisp.test			\
	    tests/environments.test		\
	    tests/eval.test			\
	    tests/exceptions.test		\
	    tests/filesys.test			\
	    tests/format.test			\
	    tests/fractions.test		\
	    tests/ftw.test			\
	    tests/gc.test			\
	    tests/getopt-long.test		\
	    tests/goops.test			\
	    tests/guardians.test		\
	    tests/hash.test			\
	    tests/hooks.test			\
	    tests/import.test			\
	    tests/interp.test			\
	    tests/list.test			\
	    tests/load.test			\
	    tests/modules.test			\
	    tests/multilingual.nottest		\
	    tests/numbers.test			\
	    tests/optargs.test			\
	    tests/options.test			\
	    tests/procprop.test			\
	    tests/poe.test			\
	    tests/popen.test			\
	    tests/ports.test			\
	    tests/posix.test			\
	    tests/q.test			\
	    tests/r4rs.test			\
	    tests/r5rs_pitfall.test		\
	    tests/ramap.test			\
	    tests/reader.test			\
	    tests/receive.test			\
	    tests/regexp.test			\
	    tests/signals.test			\
	    tests/socket.test			\
	    tests/srcprop.test			\
	    tests/srfi-1.test			\
	    tests/srfi-6.test			\
	    tests/srfi-10.test			\
	    tests/srfi-11.test			\
	    tests/srfi-13.test			\
	    tests/srfi-14.test			\
	    tests/srfi-19.test			\
	    tests/srfi-26.test			\
	    tests/srfi-31.test			\
	    tests/srfi-34.test			\
	    tests/srfi-35.test			\
	    tests/srfi-37.test			\
	    tests/srfi-39.test			\
	    tests/srfi-60.test			\
	    tests/srfi-69.test			\
	    tests/srfi-88.test			\
	    tests/srfi-4.test			\
	    tests/srfi-9.test			\
	    tests/strings.test			\
	    tests/structs.test			\
	    tests/symbols.test			\
	    tests/syncase.test			\
	    tests/syntax.test			\
	    tests/threads.test			\
	    tests/time.test			\
	    tests/unif.test			\
	    tests/version.test			\
	    tests/weaks.test

SCM_TESTS_DIRS = tests/asmobs \
                 tests/c-api

EXTRA_DIST = guile-test lib.scm $(SCM_TESTS) ChangeLog-2008

## Automake should be able to handle the distribution of tests/asmobs
## etc without any help, but not all version can handle 'deep'
## directories.  So we do it on our own.
dist-hook:
	for d in $(SCM_TESTS_DIRS); do \
	  cp -pR $(srcdir)/$$d $(distdir)/$$d; \
          rm -rf $(distdir)/$$d/CVS; \
        done
