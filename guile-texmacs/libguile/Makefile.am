## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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
DEFS = @DEFS@

# Override Automake's `DEFAULT_INCLUDES'.  By default, it contains
# "-I$(srcdir)", which causes problems on Tru64 where our "random.h"
# is picked up by <stdlib.h> instead of the libc's <random.h>.
DEFAULT_INCLUDES =

## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
AM_CPPFLAGS = -I.. -I$(top_srcdir) -I$(top_builddir)

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

lib_LTLIBRARIES = libguile.la
bin_PROGRAMS = guile

noinst_PROGRAMS = guile_filter_doc_snarfage gen-scmconfig

gen_scmconfig_SOURCES = gen-scmconfig.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
gen-scmconfig.$(OBJEXT): gen-scmconfig.c
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) -c -o $@ $<; \
	else \
		$(COMPILE) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
gen-scmconfig$(EXEEXT): $(gen_scmconfig_OBJECTS) $(gen_scmconfig_DEPENDENCIES) 
	@rm -f gen-scmconfig$(EXEEXT)
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(gen_scmconfig_OBJECTS); \
	else \
		$(LINK) $(gen_scmconfig_OBJECTS) $(LDADD) $(LIBS); \
	fi

scmconfig.h: ${top_builddir}/config.h gen-scmconfig$(EXEEXT)
	rm -f scmconfig.h.tmp
	cat $(srcdir)/scmconfig.h.top > scmconfig.h.tmp
	./gen-scmconfig$(EXEEXT) >> scmconfig.h.tmp
	chmod 444 scmconfig.h.tmp
	rm -f scmconfig.h
	mv scmconfig.h.tmp scmconfig.h

guile_filter_doc_snarfage_SOURCES = c-tokenize.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
c-tokenize.$(OBJEXT): c-tokenize.c
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) $(DEFS) $(AM_CPPFLAGS) -c -o $@ $<; \
	else \
		$(COMPILE) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
guile_filter_doc_snarfage$(EXEEXT): $(guile_filter_doc_snarfage_OBJECTS) $(guile_filter_doc_snarfage_DEPENDENCIES) 
	@rm -f guile_filter_doc_snarfage$(EXEEXT)
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(guile_filter_doc_snarfage_OBJECTS); \
	else \
		$(LINK) $(guile_filter_doc_snarfage_OBJECTS) $(LDADD) $(LIBS); \
	fi


guile_SOURCES = guile.c
guile_CFLAGS = $(GUILE_CFLAGS)
guile_LDADD = libguile.la
guile_LDFLAGS = $(GUILE_CFLAGS)

libguile_la_CFLAGS = $(GUILE_CFLAGS)

libguile_la_SOURCES = alist.c arbiters.c async.c backtrace.c boolean.c	\
    chars.c continuations.c convert.c debug.c deprecation.c		\
    deprecated.c discouraged.c dynwind.c environments.c eq.c error.c	\
    eval.c evalext.c extensions.c feature.c fluids.c fports.c		\
    futures.c gc.c gc-mark.c gc-segment.c gc-malloc.c gc-card.c		\
    gc-freelist.c gc_os_dep.c gdbint.c gh_data.c gh_eval.c gh_funcs.c	\
    gh_init.c gh_io.c gh_list.c gh_predicates.c goops.c gsubr.c		\
    guardians.c hash.c hashtab.c hooks.c i18n.c init.c inline.c		\
    ioext.c keywords.c lang.c list.c load.c macros.c mallocs.c		\
    modules.c numbers.c objects.c objprop.c options.c pairs.c ports.c	\
    print.c procprop.c procs.c properties.c random.c rdelim.c read.c	\
    root.c rw.c scmsigs.c script.c simpos.c smob.c sort.c srcprop.c	\
    stackchk.c stacks.c stime.c strings.c srfi-4.c srfi-13.c srfi-14.c	\
    strorder.c strports.c struct.c symbols.c threads.c null-threads.c	\
    throw.c values.c variable.c vectors.c version.c vports.c weaks.c	\
    ramap.c unif.c

DOT_X_FILES = alist.x arbiters.x async.x backtrace.x boolean.x chars.x	\
    continuations.x debug.x deprecation.x deprecated.x discouraged.x	\
    dynl.x dynwind.x environments.x eq.x error.x eval.x evalext.x	\
    extensions.x feature.x fluids.x fports.x futures.x gc.x gc-mark.x	\
    gc-segment.x gc-malloc.x gc-card.x goops.x gsubr.x guardians.x	\
    hash.x hashtab.x hooks.x i18n.x init.x ioext.x keywords.x lang.x	\
    list.x load.x macros.x mallocs.x modules.x numbers.x objects.x	\
    objprop.x options.x pairs.x ports.x print.x procprop.x procs.x	\
    properties.x random.x rdelim.x read.x root.x rw.x scmsigs.x		\
    script.x simpos.x smob.x sort.x srcprop.x stackchk.x stacks.x	\
    stime.x strings.x srfi-4.x srfi-13.x srfi-14.x strorder.x		\
    strports.x struct.x symbols.x threads.x throw.x values.x		\
    variable.x vectors.x version.x vports.x weaks.x ramap.x unif.x

EXTRA_DOT_X_FILES = @EXTRA_DOT_X_FILES@

DOT_DOC_FILES = alist.doc arbiters.doc async.doc backtrace.doc		\
    boolean.doc chars.doc continuations.doc debug.doc deprecation.doc	\
    deprecated.doc discouraged.doc dynl.doc dynwind.doc			\
    environments.doc eq.doc error.doc eval.doc evalext.doc		\
    extensions.doc feature.doc fluids.doc fports.doc futures.doc	\
    gc.doc goops.doc gsubr.doc gc-mark.doc gc-segment.doc		\
    gc-malloc.doc gc-card.doc guardians.doc hash.doc hashtab.doc	\
    hooks.doc i18n.doc init.doc ioext.doc keywords.doc lang.doc		\
    list.doc load.doc macros.doc mallocs.doc modules.doc numbers.doc	\
    objects.doc objprop.doc options.doc pairs.doc ports.doc print.doc	\
    procprop.doc procs.doc properties.doc random.doc rdelim.doc		\
    read.doc root.doc rw.doc scmsigs.doc script.doc simpos.doc		\
    smob.doc sort.doc srcprop.doc stackchk.doc stacks.doc stime.doc	\
    strings.doc srfi-4.doc srfi-13.doc srfi-14.doc strorder.doc		\
    strports.doc struct.doc symbols.doc threads.doc throw.doc		\
    values.doc variable.doc vectors.doc version.doc vports.doc		\
    weaks.doc ramap.doc unif.doc

EXTRA_DOT_DOC_FILES = @EXTRA_DOT_DOC_FILES@

BUILT_SOURCES = cpp_err_symbols.c cpp_sig_symbols.c libpath.h \
    version.h scmconfig.h \
    $(DOT_X_FILES) $(EXTRA_DOT_X_FILES)

EXTRA_libguile_la_SOURCES = _scm.h		\
    inet_aton.c memmove.c putenv.c strerror.c	\
    dynl.c regex-posix.c			\
    filesys.c posix.c net_db.c socket.c		\
    debug-malloc.c mkstemp.c	\
    win32-uname.c win32-dirent.c win32-socket.c

## delete guile-snarf.awk from the installation bindir, in case it's
## lingering there due to an earlier guile version not having been
## wiped out.
install-exec-hook:
	rm -f $(DESTDIR)$(bindir)/guile-snarf.awk

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included.  So instead we list them here.
## Perhaps we can deal with them normally once the merge seems to be
## working.
noinst_HEADERS = convert.i.c					\
                 conv-integer.i.c conv-uinteger.i.c		\
                 srfi-4.i.c					\
                 quicksort.i.c                                  \
                 win32-uname.h win32-dirent.h win32-socket.h	\
	         private-gc.h

libguile_la_DEPENDENCIES = @LIBLOBJS@
libguile_la_LIBADD = @LIBLOBJS@
libguile_la_LDFLAGS = @LTLIBINTL@ -version-info @LIBGUILE_INTERFACE_CURRENT@:@LIBGUILE_INTERFACE_REVISION@:@LIBGUILE_INTERFACE_AGE@ -export-dynamic -no-undefined

# These are headers visible as <guile/mumble.h>
pkginclude_HEADERS = gh.h

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(includedir)/libguile
modinclude_HEADERS = __scm.h alist.h arbiters.h async.h backtrace.h	\
    boolean.h chars.h continuations.h convert.h debug.h debug-malloc.h	\
    deprecation.h deprecated.h discouraged.h dynl.h dynwind.h		\
    environments.h eq.h error.h eval.h evalext.h extensions.h		\
    feature.h filesys.h fluids.h fports.h futures.h gc.h		\
    gdb_interface.h gdbint.h goops.h gsubr.h guardians.h hash.h		\
    hashtab.h hooks.h i18n.h init.h inline.h ioext.h iselect.h		\
    keywords.h lang.h list.h load.h macros.h mallocs.h modules.h	\
    net_db.h numbers.h objects.h objprop.h options.h pairs.h ports.h	\
    posix.h regex-posix.h print.h procprop.h procs.h properties.h	\
    random.h ramap.h rdelim.h read.h root.h rw.h scmsigs.h validate.h	\
    script.h simpos.h smob.h snarf.h socket.h sort.h srcprop.h		\
    stackchk.h stacks.h stime.h strings.h srfi-4.h srfi-13.h srfi-14.h	\
    strorder.h strports.h struct.h symbols.h tags.h threads.h		\
    pthread-threads.h null-threads.h throw.h unif.h values.h		\
    variable.h vectors.h vports.h weaks.h

nodist_modinclude_HEADERS = version.h scmconfig.h

bin_SCRIPTS = guile-snarf

# We can re-enable install for some of these if/when they are documented
# and people feel like maintaining them.  For now, this is not the case.
noinst_SCRIPTS = guile-doc-snarf guile-snarf-docs guile-func-name-check

EXTRA_DIST = ChangeLog-gh ChangeLog-scm ChangeLog-threads		\
    ChangeLog-1996-1999 ChangeLog-2000 ChangeLog-2008 cpp_signal.c	\
    cpp_errno.c cpp_err_symbols.in cpp_err_symbols.c			\
    cpp_sig_symbols.c cpp_sig_symbols.in cpp_cnvt.awk			\
    c-tokenize.lex version.h.in						\
    scmconfig.h.top gettext.h measure-hwm.scm
#    $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) \
#    guile-procedures.txt guile.texi

## We use @-...-@ as the substitution brackets here, instead of the
## usual @...@, so autoconf doesn't go and substitute the values
## directly into the left-hand sides of the sed substitutions.  *sigh*
version.h: version.h.in
	sed < $(srcdir)/version.h.in > $@.tmp \
	  -e s:@-GUILE_MAJOR_VERSION-@:${GUILE_MAJOR_VERSION}: \
	  -e s:@-GUILE_MINOR_VERSION-@:${GUILE_MINOR_VERSION}: \
	  -e s:@-GUILE_MICRO_VERSION-@:${GUILE_MICRO_VERSION}:
	mv $@.tmp $@

## FIXME: Consider using timestamp file, to avoid unnecessary rebuilds.
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@echo "Generating libpath.h..."
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "$(pkgdatadir)"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "$(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "$(pkgdatadir)/site"' >> libpath.tmp
	@echo '#define SCM_BUILD_INFO { \' >> libpath.tmp
	@echo '	{ "srcdir", "'"`cd @srcdir@; pwd`"'" }, \' >> libpath.tmp
	@echo ' { "top_srcdir",    "@top_srcdir_absolute@" }, \' >> libpath.tmp
	@echo '	{ "prefix",	   "@prefix@" }, \' >> libpath.tmp
	@echo '	{ "exec_prefix",   "@exec_prefix@" }, \' >> libpath.tmp
	@echo '	{ "bindir",	   "@bindir@" }, \' >> libpath.tmp
	@echo '	{ "sbindir",	   "@sbindir@" }, \' >> libpath.tmp
	@echo '	{ "libexecdir",	   "@libexecdir@" }, \' >> libpath.tmp
	@echo '	{ "datadir",	   "@datadir@" }, \' >> libpath.tmp
	@echo '	{ "sysconfdir",	   "@sysconfdir@" }, \' >> libpath.tmp
	@echo '	{ "sharedstatedir", "@sharedstatedir@" }, \' >> libpath.tmp
	@echo '	{ "localstatedir", "@localstatedir@" }, \' >> libpath.tmp
	@echo '	{ "libdir",	   "@libdir@" }, \' >> libpath.tmp
	@echo '	{ "infodir",	   "@infodir@" }, \' >> libpath.tmp
	@echo '	{ "mandir",	   "@mandir@" }, \' >> libpath.tmp
	@echo '	{ "includedir",	   "@includedir@" }, \' >> libpath.tmp
	@echo '	{ "pkgdatadir",	   "$(datadir)/@PACKAGE@" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "$(libdir)/@PACKAGE@" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "$(includedir)/@PACKAGE@" }, \' \
		>> libpath.tmp
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileinterface", "@LIBGUILE_INTERFACE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '	{ "CFLAGS",	   "@GUILE_CFLAGS@" }, \' >> libpath.tmp
	@echo '	{ "buildstamp",    "'"`date`"'" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	@mv libpath.tmp libpath.h


snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x .doc
.c.x:
	./guile-snarf -o $@ $< $(snarfcppopts)
.c.doc:
	-$(AWK) -f ./guile-func-name-check $<
	(./guile-snarf-docs $(snarfcppopts) $< | \
	./guile_filter_doc_snarfage$(EXEEXT) --filter-snarfage) > $@ || { rm $@; false; }

$(DOT_X_FILES) $(EXTRA_DOT_X_FILES): scmconfig.h snarf.h guile-snarf.in

$(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES): scmconfig.h snarf.h guile-snarf-docs.in guile_filter_doc_snarfage$(EXEEXT)

error.x: cpp_err_symbols.c
posix.x: cpp_sig_symbols.c
load.x: libpath.h

include $(top_srcdir)/am/pre-inst-guile

alldotdocfiles    = $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES)
snarf2checkedtexi = GUILE="$(GUILE_FOR_BUILD)" $(top_srcdir)/scripts/snarf-check-and-output-texi
dotdoc2texi       = cat $(alldotdocfiles) | $(snarf2checkedtexi)

guile.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi) --manual > $@ || { rm $@; false; }

guile-procedures.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi)          > $@ || { rm $@; false; }

if HAVE_MAKEINFO

guile-procedures.txt: guile-procedures.texi
	rm -f $@
	makeinfo --force -o $@ guile-procedures.texi || test -f $@

else

guile-procedures.txt: guile-procedures.texi
	cp guile-procedures.texi $@

endif

# Stack limit calibration for `make check'.  (For why we do this, see
# the comments in measure-hwm.scm.)  We're relying here on a couple of
# bits of Automake magic.
#
# 1. The fact that "libguile" comes before "test-suite" in SUBDIRS in
# our toplevel Makefile.am.  This ensures that the
# stack-limit-calibration.scm "test" will be run before any of the
# tests under test-suite.
#
# 2. The fact that each test is invoked as $TESTS_ENVIRONMENT $test.
# This allows us to ensure that the test will be considered to have
# passed, by using `true' as TESTS_ENVIRONMENT.
#
# Why don't we care about the test "actually passing"?  Because the
# important thing about stack-limit-calibration.scm is just that it is
# generated in the first place, so that other tests under test-suite
# can use it.
TESTS = stack-limit-calibration.scm
TESTS_ENVIRONMENT = true

stack-limit-calibration.scm: measure-hwm.scm guile$(EXEEXT)
	$(preinstguile) -s $(srcdir)/measure-hwm.scm > $@

c-tokenize.c: c-tokenize.lex
	flex -t $(srcdir)/c-tokenize.lex > $@ || { rm $@; false; }

schemelibdir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)
schemelib_DATA = guile-procedures.txt

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

cpp_err_symbols.c: cpp_err_symbols.in cpp_cnvt.awk
	$(AWK) -f $(srcdir)/cpp_cnvt.awk < $(srcdir)/cpp_err_symbols.in > \
		cpp_err_symbols.tmp
	mv cpp_err_symbols.tmp cpp_err_symbols.c

cpp_sig_symbols.c: cpp_sig_symbols.in cpp_cnvt.awk
	$(AWK) -f $(srcdir)/cpp_cnvt.awk < $(srcdir)/cpp_sig_symbols.in > \
		cpp_sig_symbols.tmp
	mv cpp_sig_symbols.tmp cpp_sig_symbols.c

## Create a new version of the cpp_sig_symbols.in file, including all SIGXXX
## macros defined on this platform.
check_signals:
	gcc -undef -dM -E $(srcdir)/cpp_signal.c | egrep ' SIG[A-Z]+' \
	  | cut -f2 -d' ' | sort > cpp_sig_symbols_here
	diff -u $(srcdir)/cpp_sig_symbols.in cpp_sig_symbols_here \
          | egrep '^\+S' \
	  | cut -c2- > cpp_sig_symbols_diff
	if test -s cpp_sig_symbols_diff ; then \
	  cat $(srcdir)/cpp_sig_symbols.in cpp_sig_symbols_diff \
	    | sort > cpp_sig_symbols_new ;\
	  echo "cpp_sig_symbols_new has the following additions:" ;\
	  cat cpp_sig_symbols_diff ;\
	else echo "No new symbols found."; \
	fi

## Likewise for cpp_err_symbols.in.
check_errnos:
	gcc -undef -dM -E $(srcdir)/cpp_errno.c | egrep ' E.+' \
	  | cut -f2 -d' ' | sort > cpp_err_symbols_here
	diff -u $(srcdir)/cpp_err_symbols.in cpp_err_symbols_here \
          | egrep '^\+E' \
	  | cut -c2- > cpp_err_symbols_diff
	if test -s cpp_err_symbols_diff ; then \
	  cat $(srcdir)/cpp_err_symbols.in cpp_err_symbols_diff \
	    | sort > cpp_err_symbols_new ;\
	  echo "cpp_err_symbols_new has the following additions:" ;\
	  cat cpp_err_symbols_diff ;\
	else echo "No new symbols found."; \
	fi

MOSTLYCLEANFILES = \
	cpp_err_symbols_here cpp_err_symbols_diff cpp_err_symbols_new \
	cpp_sig_symbols_here cpp_sig_symbols_diff cpp_sig_symbols_new \
	version.h version.h.tmp \
	scmconfig.h scmconfig.h.tmp stack-limit-calibration.scm

CLEANFILES = libpath.h *.x *.doc guile-procedures.txt guile-procedures.texi guile.texi

MAINTAINERCLEANFILES = c-tokenize.c
