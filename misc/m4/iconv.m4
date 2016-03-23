AC_DEFUN([LC_ICONV],[
  AC_ARG_WITH(iconv,
  AS_HELP_STRING([--with-iconv@<:@=DIR@:>@], [where to find iconv [system]]))

  # Check for iconv
  # Modified from GNOME's libxml2 configure.in
  AC_LANG_SAVE
  AC_LANG_C  # compile C to avoid the 'const char**' problem

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_iconv" = "no" ; then
      AC_MSG_RESULT([disabling iconv support])
  else
      if test -n "$ICONV_CFLAGS" -a -n "$ICONV_LDFLAGS"; then
         CPPFLAGS="$ICONV_CFLAGS" # for AC_TRY_LINK
         LDFLAGS="$ICONV_LDFLAGS"
      fi
      if test "$with_iconv" != "yes" -a "$with_iconv" != "" ; then
         CPPFLAGS="-I$with_iconv/include" # for AC_TRY_LINK
         LDFLAGS="-L$with_iconv/lib"
         ICONV_CFLAGS="-I$with_iconv/include"
         ICONV_LDFLAGS="-L$with_iconv/lib"
      fi

      AC_CHECK_HEADER(iconv.h,
      AC_MSG_CHECKING(for iconv)
      AC_TRY_LINK(
  [
  #include <stdlib.h>
  #include <iconv.h>
  ],[
      iconv_t cd = iconv_open ("","");
      iconv (cd, NULL, NULL, NULL, NULL);
  ],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_ICONV, 1, [Use iconv library])
  ],[
      AC_MSG_RESULT(no)
      AC_MSG_CHECKING(for iconv in -liconv)
  #    LDFLAGS="${ICONV_LDFLAGS}"    # for AC_TRY_LINK
      LIBS="-liconv"                # for AC_TRY_LINK
      AC_TRY_LINK(
  [
  #include <stdlib.h>
  #include <iconv.h>
  ],[
        iconv_t cd = iconv_open ("","");
        iconv (cd, NULL, NULL, NULL, NULL);
  ],[
        AC_MSG_RESULT(yes)
        AC_DEFINE(USE_ICONV, 1, [Use iconv library])
        ICONV_LDFLAGS="${ICONV_LDFLAGS/-liconv} -liconv"
  ],[
        AC_MSG_RESULT(no)
        AC_MSG_WARN([absence of iconv may crash HTML import])
    ])]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"
  AC_LANG_RESTORE  # restore C++ language
  AC_SUBST(ICONV_CFLAGS)
  AC_SUBST(ICONV_LDFLAGS)
])