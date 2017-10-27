
#--------------------------------------------------------------------
#
# MODULE      : tm_windows.m4
# DESCRIPTION : Windows specific settings
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_WINDOWS],[
  if test "$CONFIG_OS" = MINGW; then
    AC_ARG_ENABLE(console,
      AS_HELP_STRING([--enable-console], [enable windows console]))
    if test "$enableval" = yes
    then  AC_MSG_RESULT([enabling Windows console])
          LC_SCATTER_FLAGS([-mconsole])
    else  AC_MSG_RESULT([disabling Windows console])
          LC_SCATTER_FLAGS([-mwindows])
    fi
    if test -n "$TMREPO"; then
      AC_SUBST([XTRA_CMD],[$TMREPO/windows])
    fi
  fi
])
