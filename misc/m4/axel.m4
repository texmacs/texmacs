AC_DEFUN([LC_AXEL],[
  AC_ARG_WITH(axel,
  AS_HELP_STRING([--with-axel@<:@=ARG@:>@],
  [with axel support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_axel" = "no" -o "$with_axel" = "" ; then
      AC_MSG_RESULT([disabling axel support])
  else
      CPPFLAGS=`axel-config --cflags`
      LIBS=`axel-config --libs`
      AC_CHECK_HEADER(QGui/Viewer.h,
      AC_MSG_CHECKING(for axel)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <QGui/Viewer.h>
  ]], [[
      Viewer viewer(0);
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_AXEL, 1, [Use axel library])
      AXEL_CFLAGS="$CPPFLAGS"
      if test "$with_axel" = "linked" ; then
        AXEL_LDFLAGS="$LIBS"
        AC_DEFINE(LINKED_AXEL, 1, [Link axel library with TeXmacs])
      fi
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(AXEL)
])