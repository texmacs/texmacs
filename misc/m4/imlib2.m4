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
      CPPFLAGS=`imlib2-config --cflags`
      LIBS=`imlib2-config --libs`
      AC_CHECK_HEADER(Imlib2.h,
      AC_MSG_CHECKING(for imlib2)
      AC_TRY_LINK(
  [
  #include <X11/Xlib.h>
  #include <Imlib2.h>
  #include <stdio.h>
  #include <string.h>
  ],[
      Imlib_Image image= imlib_load_image("");
  ],[
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

  AC_SUBST(IMLIB2_CFLAGS)
  AC_SUBST(IMLIB2_LDFLAGS)
])