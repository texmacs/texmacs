
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

# if we are using TMREPO with a embedded gs then it will be packed in TeXmacs

AC_DEFUN([LC_GS],[ 
  AC_ARG_ENABLE(gs,
  AS_HELP_STRING([--disable-gs@<:@=DIR@:>@],[disable ghostscript support]),
    [AC_MSG_WARN([Compilation may fail])], [
      AC_DEFINE([USE_GS], [1], [Use ghostscript])   
      AC_SUBST([CONFIG_GS],["Ghostscript"])
      if @<:@@<:@ -n $TMREPO @:>@@:>@ 
      then # it is comming from tm SDK
        AC_PATH_PROGS([GS_EXE],gs gs.exe, [], [$TMREPO/bin])
        if @<:@@<:@ -x "$GS_EXE" @:>@@:>@ 
        then
          #get needed gs fonts and libs paths
          while read -r l sep p # default path, separator, extra path
          do
            case $l in
            (/*/ghostscript*/lib) GS_LIB=$l;;
            (/*/ghostscript*/fonts) GS_FONTS=$l;;
            esac
          done <<< "$($GS_EXE -h)"    # try to change path to match the bundle if any
        
          AC_DEFINE_UNQUOTED([GS_EXE],["bin/$(basename $GS_EXE)"],[gs path relative to TEXMACS_PATH])
          AC_DEFINE_UNQUOTED([GS_LIB],["share/ghostscript/lib"],[gs lib path relative to TEXMACS_PATH])
          AC_DEFINE_UNQUOTED([GS_FONTS],["share/ghostscript/fonts"],[gs fonts relative to TEXMACS_PATH])
          AC_SUBST([GS_EXE])
          AC_SUBST([GS_LIB])
          AC_SUBST([GS_FONTS])
          AC_MSG_NOTICE([Ghostscript found  in TMREPO, it will be embedded in Package for Macos or Windows])
        else AC_MSG_WARN([Ghostscript not detected in TMREPO, won't be embedded in Package])
        fi
      fi
    ]
  )
])
