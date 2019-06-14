
#--------------------------------------------------------------------
#
# MODULE      : tm_macos.m4
# DESCRIPTION : MacOS specific settings
# COPYRIGHT   : (C) 2000, 2019 Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

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

   AC_ARG_WITH(sdk,
       AS_HELP_STRING([--with-sdk@<:@=none@:>@],
       [Apple SDK level (i.e 10.6)]),[
         sdk=/Developer/SDKs/MacOSX$withval.sdk
         [[ -d $sdk ]] || AC_MSG_ERROR([Apple SDK not found in $sdk])
         LC_MERGE_FLAGS([-isysroot $sdk] ,[CFLAGS])
         LC_MERGE_FLAGS([-isysroot $sdk],[CXXFLAGS])
         LC_MERGE_FLAGS([-Wl,-syslibroot,$sdk],[LDFLAGS])
       ],[]
    )
    AC_ARG_WITH(osx,
       AS_HELP_STRING([--with-osx@<:@=none@:>@],
       [targeted configuration (i.e 10.6)]),[
         MACOSX_TARGET=-os$withval
         LC_MERGE_FLAGS([-mmacosx-version-min=$withval],[CFLAGS])
         LC_MERGE_FLAGS([-mmacosx-version-min=$withval],[CXXFLAGS])
         LC_MERGE_FLAGS([-Wl,-macosx_version_min,$withval],[LDFLAGS])
       ],[]
    )
    AC_SUBST(MACOSX_TARGET)
fi
])
