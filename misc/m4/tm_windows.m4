
#--------------------------------------------------------------------
#
# MODULE      : tm_windows.m4
# DESCRIPTION : Windows specific settings
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_WINDOWS],[
  if test x"$CONFIG_OS" = xMINGW; then
    AC_ARG_ENABLE(console,
    [  --enable-console        enable windows console], [], [])
    case "$enable_console" in
	yes)
	    AC_MSG_RESULT([enabling Windows console])
	    ;;
	*)
	    AC_MSG_RESULT([disabling Windows console])
	    CONFIG_LDRT="$CONFIG_LDRT -mwindows"
	    ;;
    esac
  fi
])
