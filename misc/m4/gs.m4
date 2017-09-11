
#-------------------------------------------------------------------
# 
# MODULE      : gs.m4
# DESCRIPTION : Ghostscript file location
# COPYRIGHT   : (C) 2000, 2016  Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#-------------------------------------------------------------------

AC_DEFUN([LC_WITH_GS],[ 
AC_PATH_PROGS([GS_EXE], gs gswin32c.exe, [], [${$1:-$PATH}])
if [[[ "$GS_EXE" == *.exe ]]]
then AC_PATH_PROG([GS_DLL], gsdll32.dll, [], [${$1:-$PATH}])
  [[ "$GS_DLL" ]] || unset GS_EXE
fi

if [[ "$GS_EXE" ]]
then while read -r l sep p # default path, separator, extra path
    do
      case $l in
      (/*/ghostscript*/lib) GS_LIB=$l; GS_ELIB=$p;;
      (/*/ghostscript*/fonts) GS_FONTS=$l; GS_EFONTS=$p;;
      esac
    done <<< "$($GS_EXE -h)"
fi
])

AC_DEFUN([LC_GS],[ 
  AC_ARG_WITH(gs,
  AS_HELP_STRING([--with-gs@<:@=DIR@:>@],
  [with ghostscript support []]), [], [unset withval])

  if [[[ $withval != no ]]]
  then
    LC_WITH_GS([withval])
  else unset GS_EXE
  fi    

  if [[ "$GS_EXE" ]]
  then 
    AC_DEFINE([USE_GS], [1], [Use ghostscript])
    CONFIG_GS="Ghostscript"
    # need to adjust path to the relocated tm SDK
    # add also relative path for BUNDLE
    if [[[ "$GS_EXE" =~ $TMREPO ]]]; then # it is comming from tm SDK
      # try to change path to match the bundle if any
      TMREPObase=$(basename $TMREPO)
      AC_DEFINE_UNQUOTED([GS_EXE],["../../bin/$(basename $GS_EXE)"],[gs path relative to TEXMACS_PATH])
      AC_DEFINE_UNQUOTED([GS_LIB],
        ["../share/ghostscript${GS_LIB##*ghostscript}:${GS_ELIB:+${GS_ELIB}:}${TMREPO}${GS_LIB##*$TMREPObase}"],[gs lib])
      AC_DEFINE_UNQUOTED([GS_FONTS],
      ["../share/ghostscript${GS_FONTS##*ghostscript}:${TMREPO}${GS_FONTS##*$TMREPObase}"],[gs fonts])
    fi  
  else AC_MSG_RESULT([disabling ghostscript support])
  fi
  AC_SUBST([CONFIG_GS])
  AC_SUBST([GS_EXE])
  AC_SUBST([GS_DLL])
])
