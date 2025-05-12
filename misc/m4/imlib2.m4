
#--------------------------------------------------------------------
#
# MODULE      : imlib2.m4
# DESCRIPTION : TeXmacs configuration options for imlib2 library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_IMLIB2],[
  AC_ARG_WITH(imlib2,
  AS_HELP_STRING([--with-imlib2@<:@=ARG@:>@],
  [with imlib2 support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_imlib2" = "no" -o "$with_imlib2" = "" ; then
      AC_MSG_RESULT([disabling imlib2 support])
  else
      CPPFLAGS=`pkg-config --cflags imlib2`
      LIBS=`pkg-config --libs imlib2`
      AC_CHECK_HEADER(Imlib2.h,
      AC_MSG_CHECKING(for imlib2)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <X11/Xlib.h>
  #include <Imlib2.h>
  #include <stdio.h>
  #include <string.h>
  ]], [[
      Imlib_Image image= imlib_load_image("");
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_IMLIB2, 1, [Use imlib2 library])
      IMLIB2_CFLAGS="$CPPFLAGS"
      if test "$with_imlib2" = "linked" ; then
        IMLIB2_LDFLAGS="$LIBS"
        AC_DEFINE(LINKED_IMLIB2, 1, [Link imlib2 library with TeXmacs])
      fi
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(IMLIB2)
])
