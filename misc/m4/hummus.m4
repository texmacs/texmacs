AC_DEFUN([LC_HUMMUS],[
  AC_ARG_ENABLE(pdf-renderer,
               [  --enable-pdf-renderer   use native PDF renderer for pdf export],
               [], [enable_pdf_renderer="yes"])
  case "$enable_pdf_renderer" in
      yes)
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
          AC_MSG_CHECKING(whether freetype defines T1_EncodingType)
          AC_TRY_LINK(
  [
  #include <ft2build.h>
  #include FT_FREETYPE_H
  #include FT_TYPE1_TABLES_H
  ],[
  FT_Library ft_library;
  T1_EncodingType t;
  (void) FT_Init_FreeType (&ft_library);
  ],[
          AC_MSG_RESULT(yes)
      AC_MSG_RESULT([enabling native PDF renderer])
          AC_DEFINE(PDF_RENDERER, 1, [Enabling native PDF renderer])
  #        CONFIG_PDF="Pdf Pdf/dvipdfmx"
  #        CONFIG_PDF="Pdf Pdf/PDFWriter"
          CONFIG_PDF="Pdf"
          PDF_CFLAGS="-DPDFHUMMUS_NO_TIFF -DPDFHUMMUS_NO_DCT"
  ],[
          AC_MSG_RESULT(no)
          AC_MSG_ERROR([cannot find FreeType or your version is < 2.4.8.
  If you have several versions installed please use the proper freetype-config script to set
  the environment variables FREETYPE_CFLAGS and FREETYPE_LDFLAGS. 
                       ])
          CONFIG_PDF=""
  ]))
          CPPFLAGS="$SAVE_CPPFLAGS"
          LDFLAGS="$SAVE_LDFLAGS"
          LIBS="$SAVE_LIBS"
          ;;
      no)
        AC_MSG_RESULT([disabling native PDF renderer])
          CONFIG_PDF=""
    ;;
      *)
    AC_MSG_ERROR([bad option --enable-pdf-renderer=$enable_pdf_renderer])
    ;;
  esac
  AC_SUBST(CONFIG_PDF)
])