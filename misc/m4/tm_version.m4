
#--------------------------------------------------------------------
#
# MODULE      : tm_version.m4
# DESCRIPTION : Version management settings
# COPYRIGHT   : (C) 2000-2020  Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
# Change to this file will modify the DEVEL_REVISION field.
# Do not change it manually.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_VERSION],[
  PACKAGE="TeXmacs"
  DEBIAN_NAME="texmacs"

  VERSION_MAJOR="1"
  VERSION_MINOR="99"
  VERSION_BUILD="19"
  
  # the svn revision of this version
  DEVEL_REVISION="$Rev$:"
  
  DEVEL_VERSION="$VERSION_MAJOR.$VERSION_MINOR.$VERSION_BUILD"
  DEVEL_RELEASE="1"         # I think we should use SVNREV here
  STABLE_VERSION="$VERSION_MAJOR.$VERSION_MINOR.$VERSION_BUILD"
  STABLE_RELEASE="1"

  if test -n $VERSION_BUILD
  then DEVEL_VERSION="$VERSION_MAJOR.$VERSION_MINOR.$VERSION_BUILD"
  else DEVEL_VERSION="$VERSION_MAJOR.$VERSION_MINOR"
  fi
  STABLE_VERSION=$DEVEL_VERSION
  STABLE_RELEASE=$DEVEL_RELEASE

  AC_SUBST(PACKAGE)
  AC_SUBST(DEVEL_VERSION)
  AC_SUBST(DEVEL_RELEASE)
  AC_SUBST(STABLE_VERSION)
  AC_SUBST(STABLE_RELEASE)
  AC_SUBST(VERSION_MAJOR)
  AC_SUBST(VERSION_MINOR)
  AC_SUBST(VERSION_BUILD)
])
