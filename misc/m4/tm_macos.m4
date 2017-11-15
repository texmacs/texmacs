
#--------------------------------------------------------------------
#
# MODULE      : tm_macos.m4
# DESCRIPTION : MacOS specific settings
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([AC_QT_CXXFLAG_stdlib], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_LANG_PUSH([C++])
  OLDCXXFLAGS=$CXXFLAGS
  CXXFLAGS="$CXXFLAGS -stdlib=libc++"
  AC_COMPILE_CHECK([whether C++ compiler supports -stdlib],[],[], [
    AC_MSG_RESULT(yes)
    Stdlib="yes"
  ], [
    AC_MSG_RESULT(no)
    Stdlib="no"])
  CXXFLAGS=$OLDCXXFLAGS
  AC_LANG_POP([C++])
])

AC_DEFUN([TM_MACOS],[
  if test x"$CONFIG_OS" = xMACOS; then
    AC_ARG_ENABLE(macosx-extensions,
    AS_HELP_STRING([--disable-macosx-extensions],
    [do not use Mac specific services (spellchecker, image handling, ...)]),
	[], [enable_macosx_extensions="yes"])
    case "$enable_macosx_extensions" in
	yes)
	    AC_MSG_RESULT([enabling Mac OSX extensions])
	    AC_DEFINE(MACOSX_EXTENSIONS, 1, [Enabling Mac OSX extensions])
	    CONFIG_MACOS="MacOS"
    #        CONFIG_BFLAGS="$CONFIG_BFLAGS -framework IOKit"
	    ;;
	no)
	    AC_MSG_RESULT([disabling Mac OSX extensions])
	    ;;
	*)
	    AC_MSG_ERROR([bad option --enable-macosx-extensions=$enable_macosx_extensions])
	    ;;
    esac
  fi

  case "${host}" in
    *apple*darwin*)
      AC_QT_CXXFLAG_stdlib
      if test "$Stdlib" = "no"; then
	QT_CXXFLAGS=`echo $QT_CXXFLAGS | sed 's/ -stdlib=@<:@^@<:@:space:@:>@@:>@*//g'`
	QT_LDFLAGS=`echo $QT_LDFLAGS | sed 's/ -stdlib=@<:@^@<:@:space:@:>@@:>@*//g'`
      fi
   ;;
  esac
])
