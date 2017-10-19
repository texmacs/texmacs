
#--------------------------------------------------------------------
#
# MODULE      : poppler.m4
# DESCRIPTION : TeXmacs configuration options for poppler library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

m4_define([PopplerLibs],[-lpoppler-qt4 -lpoppler])
AC_DEFUN([LM_POPPLER],[
  AC_LANG_PROGRAM([[#include "poppler/qt4/poppler-qt4.h"]],[[
    Poppler::Document::load("Dummy");
  ]])
])

AC_DEFUN([LC_POPPLER],[
 if @<:@@<:@ $CONFIG_GUI != QT || $CONFIG_OS != MINGW @:>@@:>@ 
  then LC_MSG_RESULT([disabled poppler: needs Qt and MinGW])
  else
    AX_SAVE_FLAGS
    LC_MERGE_FLAGS([PopplerLibs],[QT_LIBS])
    LC_MERGE_FLAGS($CPPFLAGS,[QT_CPPFLAGS])
    LC_SET_FLAGS([QT])
    AC_CHECK_HEADER(poppler/qt4/poppler-qt4.h, [
      AC_LINK_IFELSE([
        LM_POPPLER
      ],[
        AX_RESTORE_FLAGS
        LC_SCATTER_FLAGS([PopplerLibs])
      ],[
        AC_MSG_ERROR(Cannot find poppler library)
      ])
    ],[
      AC_MSG_ERROR([Cannot find poppler header])
    ])
  fi
])
