
#--------------------------------------------------------------------
#
# MODULE      : tm_install.m4
# DESCRIPTION : Final installation and packaging settings for TeXmacs
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_INSTALL],[
  if test "$prefix" = ""; then
    prefix=/usr/local
  fi
  if test "$prefix" = "NONE"; then
    prefix=/usr/local
  fi
  if test "$exec_prefix" = ""; then
    exec_prefix='${prefix}'
  fi
  if test "$exec_prefix" = "NONE"; then
    exec_prefix='${prefix}'
  fi
  if test "$exec_prefix" = '${prefix}'; then
    exec_prefix='${prefix}'
  fi
  if test "$includedir" = ""; then
    includedir='${prefix}/include'
  fi
  if test "$includedir" = "NONE"; then
    includedir='${prefix}/include'
  fi
  if test "$includedir" = '${prefix}/include'; then
    includedir='${prefix}/include'
  fi
  if test "$libdir" = ""; then
    libdir='${exec_prefix}/lib'
  fi
  if test "$libdir" = "NONE"; then
    libdir='${exec_prefix}/lib'
  fi
  if test "$libdir" = '${exec_prefix}/lib'; then
    libdir='${exec_prefix}/lib'
  fi
  if test "$bindir" = ""; then
    bindir='${exec_prefix}/bin'
  fi
  if test "$bindir" = "NONE"; then
    bindir=${exec_prefix}/bin
  fi
  if test "$bindir" = '${exec_prefix}/bin'; then
    bindir='${exec_prefix}/bin'
  fi
  if test "$datarootdir" = ""; then
    datarootdir='${prefix}/share'
  fi
  if test "$datarootdir" = "NONE"; then
    datarootdir='${prefix}/share'
  fi
  if test "$datarootdir" = '${prefix}/share'; then
    datarootdir='${prefix}/share'
  fi
  if test "$datadir" = ""; then
    datadir='${prefix}/share'
  fi
  if test "$datadir" = "NONE"; then
    datadir='${prefix}/share'
  fi
  if test "$datadir" = '${prefix}/share'; then
    datadir='${prefix}/share'
  fi
  if test "$mandir" = ""; then
    mandir='${datarootdir}/man'
  fi
  if test "$mandir" = "NONE"; then
    mandir='${datarootdir}/man'
  fi
  if test "$mandir" = '${datarootdir}/man'; then
    mandir='${datarootdir}/man'
  fi
  if test "$libexecdir" = ""; then
    libexecdir='${exec_prefix}/libexec'
  fi
  if test "$libexecdir" = "NONE"; then
    libexecdir='${exec_prefix}/libexec'
  fi
  if test "$libexecdir" = '${exec_prefix}/libexec'; then
    libexecdir='${exec_prefix}/libexec'
  fi

  curdir="`pwd`"
  tmorig=${curdir}
  tmdir=${PACKAGE}
  tmsrc=${curdir}/${tmdir}
  tmbin=${libexecdir}/${tmdir}
  tmdata=${datadir}/${tmdir}
  AC_SUBST(tmorig)
  AC_SUBST(tmdir)
  AC_SUBST(tmsrc)
  AC_SUBST(tmbin)
  AC_SUBST(tmdata)

	if test -z "$VERSION_BUILD"
	then tm_devel_release="${PACKAGE}-${DEVEL_VERSION}.0"
	else tm_devel_release="${PACKAGE}-${DEVEL_VERSION}"
	fi

	if test $SVNREV != $DEVEL_RELEASE
	then 	tm_devel=$tm_devel_release
				DEVEL_RELEASE=$SVNINT
				tm_devel="$tm_devel_release.$DEVEL_RELEASE"
	else 	tm_devel="${PACKAGE}-${DEVEL_VERSION}"
	fi

	if test -z "$VERSION_BUILD"
	then tm_windows_release="$VERSION_MAJOR,$VERSION_MINOR,0,0"
	else tm_windows_release="$VERSION_MAJOR,$VERSION_MINOR,$VERSION_BUILD,0"
	fi
	
  tm_devel_release="$tm_devel_release.$DEVEL_RELEASE"
  tm_underscore_devel=${tm_devel/-/_/}
  tm_debian_name_devel=${DEBIAN_NAME}_${DEVEL_VERSION}

  AC_SUBST(tm_devel)
  AC_SUBST(tm_devel_release)
  AC_SUBST(tm_windows_release)
  AC_SUBST(tm_underscore_devel)
  AC_SUBST(tm_debian_name_devel)

  if test "$STABLE_RELEASE" = "1"; then
    tm_stable=${PACKAGE}-${STABLE_VERSION}
  else
    tm_stable=${PACKAGE}-${STABLE_VERSION}-R${STABLE_RELEASE}
  fi
  tm_stable_release=${PACKAGE}-${STABLE_VERSION}-${STABLE_RELEASE}
  tm_underscore_stable=${PACKAGE}_${STABLE_VERSION}

  AC_SUBST(tm_stable)
  AC_SUBST(tm_stable_release)
  AC_SUBST(tm_underscore_stable)
])
