
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
    AS_IF([test -n "$TMREPO"],[
      AC_SUBST([XTRA_CMD],[$TMREPO/windows/bin])
      AS_IF([test -d $TMREPO/windows/aspell],[
        AC_SUBST([ASPELL_CMD],[$TMREPO/windows/aspell])
        AC_DEFINE([ASPELL],["plugins/aspell"],[embedded aspell location])])
    ])

    AC_ARG_ENABLE(cv2pdb,
      AS_HELP_STRING([--enable-windows-pbd], [enable windows debug symbols]))
    if test "$enableval" = yes
    then  AC_MSG_RESULT([enabling cv2pdb])
          AC_CHECK_PROG([CV2PDB], [cv2pdb], [yes], [no])
          if test "$CV2PDB" = no
          then  AC_MSG_ERROR([cv2pdb not found])
          fi
    else  AC_MSG_RESULT([disabling cv2pdb])
    fi
    
  fi

])
