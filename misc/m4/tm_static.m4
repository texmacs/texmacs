
#--------------------------------------------------------------------
#
# MODULE      : tm_static.m4
# DESCRIPTION : Build static libraries
# COPYRIGHT   : (C) 2017  Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_STATIC],[
  AC_ARG_ENABLE(static,
  AS_HELP_STRING([--enable-static@<:@=no@:>@], [statically link libraries]),
    [AC_MSG_NOTICE([Link with static librairies enabled])],[
    if test -n "$TMREPO" -a $CONFIFG_OS == MAC_OS
      then enableval="yes"
      else enableval="no"
    fi
    ])

  unset LNSTATIC SEMISTATIC SEMIDYNAMIC MAC
  if [[[ "$enableval" != no ]]]
  then case $CONFIG_OS in
    LINUX|MINGW)
      LNSTATIC="-static"
      LC_APPEND_FLAG([-static],[CFLAGS])
      LC_APPEND_FLAG([-static],[CXXFLAGS])
      AC_MSG_WARN([statically link main libraries for linux])
      ;;
    MACOS)
      LC_APPEND_FLAG([-Wl,-search_paths_first],[LDFLAGS])
      AC_MSG_NOTICE([statically link main libraries for MacOS X])
      ;;
    *) AC_MSG_ERROR(static link not supported);;
    esac
  fi
])
