AC_DEFUN([LC_FREETYPE],[
  AC_ARG_ENABLE(freetype,
  [  --disable-freetype      enable compilation without freetype],
      [disable_freetype="yes"], [disable_freetype="no"])

  AC_ARG_WITH(freetype,
  AS_HELP_STRING([--with-freetype@<:@=ARG@:>@],
  [with freetype support [ARG=linked]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test -z "$FREETYPE_CFLAGS"; then
    FREETYPE_CFLAGS=`freetype-config --cflags`
  fi
  CPPFLAGS="$CPPFLAGS $FREETYPE_CFLAGS"
  if test -z "$FREETYPE_LDFLAGS"; then
    FREETYPE_LDFLAGS=`freetype-config --libs`
  fi
  LIBS="$LDFLAGS $FREETYPE_LDFLAGS"
  AC_CHECK_HEADER(ft2build.h,
  AC_MSG_CHECKING(for freetype)
  AC_TRY_LINK(
  [
  #include <ft2build.h>
  #include FT_FREETYPE_H 
  ],[
      FT_Library ft_library;
      (void) FT_Init_FreeType (&ft_library);
  ],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_FREETYPE, 1, [Use freetype library])
      FREETYPE_CFLAGS="$CPPFLAGS"
      if test "$with_freetype" = "linked" -o "$with_freetype" = "" ; then
        FREETYPE_LDFLAGS="$LIBS"
        AC_DEFINE(LINKED_FREETYPE, 1, [Link freetype library with TeXmacs])
      fi
  ],[
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([cannot link freetype library])
  ]),
  [
      if test "$disable_freetype" != "yes"; then
        AC_MSG_ERROR([Cannot find freetype library.
  TeXmacs almost need freetype library to work properly.
  If you know what you are doing, you can configure with --disable-freetype])
      fi
  ]
  )

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  AC_SUBST(FREETYPE_CFLAGS)
  AC_SUBST(FREETYPE_LDFLAGS)
])