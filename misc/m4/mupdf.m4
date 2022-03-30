AC_DEFUN([LC_MUPDF],[
  AC_ARG_WITH(mupdf,
  AS_HELP_STRING([--with-mupdf@<:@=ARG@:>@],
  [with MuPDF support [ARG=no]]))

  SAVE_CPPFLAGS="$CPPFLAGS"
  SAVE_LDFLAGS="$LDFLAGS"
  SAVE_LIBS="$LIBS"
  if test "$with_mupdf" = "no" -o "$with_mupdf" = "" ; then
      AC_MSG_RESULT([disabling MuPDF support])
  else
      CPPFLAGS="-I/usr/local/include"
      LIBS="-lmupdf -lmupdf-third"
      AC_CHECK_HEADER(mupdf/pdf.h,
      AC_MSG_CHECKING(for MuPDF)
      AC_TRY_LINK(
  [
  #include <mupdf/pdf.h>
  ],[
      fz_context *ctx = fz_new_context (NULL, NULL, FZ_STORE_UNLIMITED);
  ],[
      AC_MSG_RESULT(yes)
      AC_DEFINE(USE_MUPDF, 1, [Use MuPDF library])
      MUPDF_CFLAGS="$CPPFLAGS"
      if test "$with_mupdf" = "linked" ; then
        MUPDF_LIBS="$LIBS"
        AC_DEFINE(LINKED_MUPDF, 1, [Link MuPDF library with TeXmacs])
      fi
      AC_DEFINE(MUPDF_RENDERER, 1, [Enabling native MuPDF backend])
      CONFIG_MUPDF="MuPdf"
      AC_SUBST(CONFIG_MUPDF)
  ],[
      AC_MSG_RESULT(no)]))
  fi

  CPPFLAGS="$SAVE_CPPFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
  LIBS="$SAVE_LIBS"

  LC_SUBST(MUPDF)
])