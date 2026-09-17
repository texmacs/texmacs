
#--------------------------------------------------------------------
#
# MODULE      : resvg.m4
# DESCRIPTION : TeXmacs configuration options for resvg
# COPYRIGHT   : (C) 2026 Liza Belos
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LM_RESVG],[AC_LANG_PROGRAM([
@%:@include <resvg.h>
],[
  resvg_options *opt = resvg_options_create ();
  resvg_options_destroy (opt);
])])

AC_DEFUN([LC_RESVG],[
  AC_ARG_WITH([resvg],
    AS_HELP_STRING([--with-resvg@<:@=DIR@:>@],
      [with resvg support [ARG=auto]]),
    [with_resvg=$withval], [with_resvg="auto"])

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"

  RESVG_CFLAGS=""
  RESVG_LDFLAGS=""
  RESVG_LIBS=""
  HAS_RESVG="no"

  if test "$with_resvg" = "no"; then
    AC_MSG_CHECKING([for resvg])
    AC_MSG_RESULT([no (disabled)])
  else
    AC_MSG_CHECKING([for resvg])

    PKG_CONFIG=`which pkg-config 2>/dev/null`
    TRY_CFLAGS=""
    TRY_LDFLAGS=""
    TRY_LIBS=""

    if test -n "$PKG_CONFIG" && $PKG_CONFIG --exists resvg 2>/dev/null; then
      TRY_CFLAGS=`$PKG_CONFIG --cflags resvg`
      TRY_LIBS=`$PKG_CONFIG --libs resvg`
    else
      RESVG_DIR=""
      if test "$with_resvg" != "yes" && test "$with_resvg" != "auto" && test -d "$with_resvg"; then
        RESVG_DIR="$with_resvg"
      elif test -n "$TMREPO" && test -f "$TMREPO/include/resvg.h"; then
        RESVG_DIR="$TMREPO"
      elif test -n "$prefix" && test -f "$prefix/include/resvg.h"; then
        RESVG_DIR="$prefix"
      elif test -n "$WORKING_DIR_WIN" && test -f "$WORKING_DIR_WIN/local/include/resvg.h"; then
        RESVG_DIR="$WORKING_DIR_WIN/local"
      fi
      if test -n "$RESVG_DIR"; then
        case "$CONFIG_OS" in
          MINGW)
            if which cygpath >/dev/null 2>&1; then
              RESVG_DIR=`cygpath -m "$RESVG_DIR"`
            fi
            ;;
        esac
        TRY_CFLAGS="-I$RESVG_DIR/include"
        TRY_LDFLAGS="-L$RESVG_DIR/lib"
      fi
      RESVG_EXTRA_LIBS=""
      case "$CONFIG_OS" in
        MINGW)
          RESVG_EXTRA_LIBS="-lws2_32 -luserenv -lbcrypt -lntdll"
          ;;
        MACOS)
          RESVG_EXTRA_LIBS="-framework Security -framework CoreFoundation"
          ;;
        *)
          RESVG_EXTRA_LIBS="-lpthread -ldl -lm"
          ;;
      esac
      TRY_LIBS="-lresvg $RESVG_EXTRA_LIBS"
    fi

    CPPFLAGS="$SAVE_CPPFLAGS $TRY_CFLAGS"
    LDFLAGS="$SAVE_LDFLAGS $TRY_LDFLAGS"
    LIBS="$SAVE_LIBS $TRY_LIBS"

    AC_LINK_IFELSE([LM_RESVG],[
      HAS_RESVG="yes"
      RESVG_CFLAGS="$TRY_CFLAGS"
      RESVG_LDFLAGS="$TRY_LDFLAGS"
      RESVG_LIBS="$TRY_LIBS"
      AC_MSG_RESULT([yes])
      AC_DEFINE(USE_RESVG, 1, [Use resvg library for SVG rendering])
    ],[
      AC_MSG_RESULT([no])
      if test "$with_resvg" = "yes" || (test "$with_resvg" != "auto" && test "$with_resvg" != "no"); then
        AC_MSG_ERROR([Cannot find resvg library])
      fi
    ])
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  AC_SUBST(RESVG_CFLAGS)
  AC_SUBST(RESVG_LDFLAGS)
  AC_SUBST(RESVG_LIBS)
  AC_SUBST(HAS_RESVG)
])

