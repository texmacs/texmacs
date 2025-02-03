AC_DEFUN([LC_CAIRO],[
  AC_ARG_WITH(cairo,
  AS_HELP_STRING([--with-cairo@<:@=ARG@:>@],
  [with cairo support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_cairo" = "no" -o "$with_cairo" = "" ; then
      AC_MSG_RESULT([disabling cairo support])
  else
      CPPFLAGS=`pkg-config --cflags cairo`
      LIBS=`pkg-config --libs cairo`
      AC_CHECK_HEADER(cairo.h,
      AC_MSG_CHECKING(for cairo)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <cairo.h>
  ]], [[
      cairo_surface_t *surface;
      surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 120, 120);
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_CAIRO, 1, [Use cairo library])
      CAIRO_CFLAGS="$CPPFLAGS"
      if test "$with_cairo" = "linked" ; then
        CAIRO_LDFLAGS="$LIBS"
        AC_DEFINE(LINKED_CAIRO, 1, [Link cairo library with TeXmacs])
      fi
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(CAIRO)
])