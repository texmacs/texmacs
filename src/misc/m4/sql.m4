AC_DEFUN([LC_SQL],[
  AC_ARG_WITH(sqlite3,
  AS_HELP_STRING([--with-sqlite3@<:@=ARG@:>@],
  [with sqlite3 support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_sqlite3" = "no" -o "$with_sqlite3" = "" ; then
      AC_MSG_RESULT([disabling sqlite3 support])
  else
      CPPFLAGS=`pkg-config --cflags sqlite3`
      LIBS=`pkg-config --libs sqlite3`
      AC_CHECK_HEADER(sqlite3.h,
      AC_MSG_CHECKING(for sqlite3)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <sqlite3.h>
  ]], [[
      int res= sqlite3_threadsafe ();
  ]])],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_SQLITE3, 1, [Use sqlite3 library])
      SQLITE3_CFLAGS="$CPPFLAGS"
      if test "$with_sqlite3" = "linked" ; then
        SQLITE3_LDFLAGS="$LIBS"
        AC_DEFINE(LINKED_SQLITE3, 1, [Link sqlite3 library with TeXmacs])
      fi
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(SQLITE3)
])