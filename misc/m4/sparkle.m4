
#--------------------------------------------------------------------
#
# MODULE      : sparkle.m4
# DESCRIPTION : Add Sparkle library if present (for Texmacs auto update)
# COPYRIGHT   : (C) 2000, 2021 Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_SPARKLE],[
  AC_ARG_WITH(sparkle,
  AS_HELP_STRING([--with-sparkle@<:@=ARG@:>@],
  [with Sparkle autoupdater [ARG=no or path for winSparkle or yes for mac
  (framework should be in the standard user frameworks location)]]))

  if test "$with_sparkle" = "no" -o "$with_sparkle" = "" ; then
      AC_MSG_RESULT([disabling Sparkle usage])
  else
    unset sparkle_ok
    AC_SUBST([CONFIG_SPARKLE])
    AX_SAVE_FLAGS
    case "${CONFIG_OS}" in
    (MINGW)  
      AC_MSG_CHECKING([whether we can use the Sparkle library])
      AC_LANG_PUSH([C])
      LC_SCATTER_FLAGS([-I $with_sparkle/include -L $with_sparkle/Release -lwinsparkle],[SPARKLE])
      LC_SET_FLAGS([SPARKLE])
      AC_TRY_LINK(
  [
  #include <winsparkle.h>
  ],[
  win_sparkle_init();
  ],[ sparkle_ok=1
      WINSPARKLE_PATH="$with_sparkle"
      WINSPARKLE_DLL="WinSparkle*.dll"
      AC_SUBST(WINSPARKLE_DLL)
      AC_SUBST(WINSPARKLE_PATH)
  ],[AC_MSG_ERROR([Can't use WinSparkle])])
      AC_LANG_POP([C])
      ;;
    (MACOS)
      with_sparkle=$HOME/Library/Frameworks
      AC_MSG_CHECKING([whether we can use the Sparkle framework])
      AC_LANG_PUSH([Objective C])
      LC_SCATTER_FLAGS([-I$with_sparkle/Sparkle.framework/Headers -F$with_sparkle -framework Sparkle -Wl,-rpath,$with_sparkle],[SPARKLE])
      LC_SET_FLAGS([SPARKLE])
      AC_TRY_LINK(
  [
  #include <Cocoa/Cocoa.h>
  #include "Sparkle.h"
  ],[
  SUUpdater* updater;
  ],[ sparkle_ok=1
      AC_SUBST([CONFIG_SPARKLE],[Updater])
  ],[AC_MSG_ERROR([Can't use WinSparkle])])
      AC_LANG_POP([Objective C])
      ;;
      esac
    
    if [[ $sparkle_ok ]]
    then AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SPARKLE, 1, [Use Sparkle framework])
      AX_RESTORE_FLAGS
      LC_SUBST(SPARKLE)
    else
      AC_MSG_RESULT(no)
      AX_RESTORE_FLAGS
    fi
  fi

  AC_ARG_WITH(appcast,
    AS_HELP_STRING([--with-appcast@<:@=ARG@:>@],
    [path for the appcast updater file]),[
      APPCAST=$withval
      AC_SUBST(APPCAST)
  ])

])
