
#--------------------------------------------------------------------
#
# MODULE      : aspell.m4
# DESCRIPTION : TeXmacs configuration options for Aspell
# COPYRIGHT   : (C) 2016 Liza Belos
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_ASPELL],[
  AC_ARG_WITH(aspell,
  AS_HELP_STRING([--with-aspell@<:@DIR=@:>@],
  [where to find aspell library []]), [], [unset withval])

  if [[[ "$withval" != no ]]]
  then
    unset USE_ASPELL
    
    AC_CHECK_HEADER(aspell.h, [
      LC_CHECK_LIB([aspell],[new_aspell_config],[
          LC_DEFINE(USE_ASPELL, 1, [Use aspell library])
          ASPELL_LIBS=" -laspell ${[$0]_extralibs}"
          LC_COMBINE_FLAGS([ASPELL])
      ],[:],[-lintl],[$0_extralibs])
    ])

    if [[[ $USE_ASPELL ]]]
    then
        AC_MSG_NOTICE([Aspell support enabled])
    else
        AC_MSG_WARN([Aspell library not found])
    fi
  else
    AC_MSG_WARN([Disabling aspell support])
  fi
  LC_SUBST([ASPELL])
])