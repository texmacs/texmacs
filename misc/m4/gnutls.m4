
#--------------------------------------------------------------------
#
# MODULE      : gnutls.m4
# DESCRIPTION : TeXmacs configuration options for GnuTLS
# COPYRIGHT   : (C) 2021 Gregoire Lecerf
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LM_GNUTLS_3],[AC_LANG_PROGRAM([
@%:@include <stdio.h>
@%:@include <stdlib.h>
@%:@include <gnutls/gnutls.h>],[
    gnutls_global_init ();
    gnutls_global_deinit ();
@%:@if GNUTLS_VERSION_NUMBER >= 0x030000
    exit (0);
@%:@else
    __dummy_error;
@%:@endif
])])

AC_DEFUN([LC_GNUTLS],[
  AC_ARG_WITH(gnutls,
    AS_HELP_STRING([--with-gnutls@<:@=ARG@:>@],
      [with GnuTLS support [ARG=no]]),
    with_gnutls=$withval, with_gnutls="no")
  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_gnutls" = "no"; then
      AC_MSG_RESULT([disabling GnuTLS support])
  else
      CPPFLAGS=`pkg-config --cflags gnutls hogweed nettle`
      LIBS=`pkg-config --libs gnutls hogweed nettle`
      if test "$TMREPO" != "" -a "$TMREPO" != "no" -a "$CONFIG_OS" = "MINGW"; then
          AC_DEFINE(TEXMACS_FIX_1_GNUTLS, 1, [Special fix])
          LIBS="$LIBS -ldbghelp -lSecur32 -lshell32 -lole32 -ladvapi32 -lsecur32 -lpthread"
      fi
      if test "$TMREPO" != "" -a "$TMREPO" != "no" -a "$CONFIG_OS" = "MACOS"; then
          LIBS="$LIBS -framework CoreFoundation -framework Security -lz"
      fi
      if test "$TMREPO" != "" -a "$TMREPO" != "no" -a "$CONFIG_OS" = "GNU_LINUX"; then
          LIBS="$LIBS -lz"
      fi
      if test "$TMREPO" != "" -a "$TMREPO" != "no" -a "$CONFIG_OS" = "ANDROID"; then
          LIBS="$LIBS -lz"
      fi
      AC_MSG_CHECKING(for GnuTLS version >= 3 (https://www.gnutls.org))
      AC_LINK_IFELSE([LM_GNUTLS_3],[
        AC_MSG_RESULT(yes)
	AC_MSG_RESULT([enabling GnuTLS support])
        AC_DEFINE(USE_GNUTLS, 1, [Use GnuTLS library version >= 3])
        GNUTLS_CPPFLAGS="$CPPFLAGS"
        GNUTLS_LDFLAGS="$LIBS"
       ],[
        AC_MSG_RESULT(no)
	AC_MSG_ERROR([Cannot find a GnuTLS library version >=3 (https://www.gnutls.org)])
     ])
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST([GNUTLS])
])
