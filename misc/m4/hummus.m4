
#--------------------------------------------------------------------
#
# MODULE      : hummus.m4
# DESCRIPTION : TeXmacs configuration options for hummus library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_HUMMUS],[
AC_ARG_ENABLE(pdf-renderer,
  AS_HELP_STRING([--disable-pdf-renderer],
  [use hummus support for native pdf exports]), [], [unset enableval])

  LC_MSG_CHECKING([hummus support for native pdf exports])
  if @<:@@<:@ "$enableval" != no @:>@@:>@
  then if @<:@@<:@ $CONFIG_GUI != QT @:>@@:>@ 
    then LC_MSG_RESULT([disabled: needs Qt])
    else
      AC_CHECK_HEADER(zlib.h, [
        AC_CHECK_LIB([z],[deflate],[
          AC_CHECK_HEADER(png.h, [
            AC_CHECK_LIB([png],[png_set_read_fn],[
              if @<:@@<:@ $USE_FREETYPE -eq 3 @:>@@:>@
              then LC_MERGE_FLAGS([-lz -lpng],[PDF_LIBS])
                AC_DEFINE(PDF_RENDERER, 1, [Enabling native PDF renderer])
                AC_DEFINE(PDFHUMMUS_NO_TIFF, 1, [Disable Tiff Format])
                AC_DEFINE(PDFHUMMUS_NO_DCT, 1, [Disable DCT])
                CONFIG_PDF="Pdf"
                AC_SUBST(CONFIG_PDF)
                LC_COMBINE_FLAGS(PDF)
                LC_MSG_RESULT([enabled])
              else
                LC_MSG_RESULT([disabled: needs freetype >= 2.4.8.])
              fi
            ],[
              LC_MSG_RESULT([disabled: needs libpng])
            ])
          ],[
            LC_MSG_RESULT([disabled: needs png.h])
          ])
        ],[
          LC_MSG_RESULT([disabled: needs libz])
        ])
      ],[
        LC_MSG_RESULT([disabled: needs zlib.h])
      ])
     fi
  else LC_MSG_RESULT([disabled])
  fi
  LC_SUBST(PDF)
])
