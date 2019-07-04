
#--------------------------------------------------------------------
#
# MODULE      : freetype.m4
# DESCRIPTION : TeXmacs configuration options for Freetype
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_WITH_FREETYPE],[
  AC_PATH_PROG(FREETYPE_tmp1, freetype-config, [], [${$1:-$PATH}])
  if test -n "$FREETYPE_tmp1"
  then
    LC_SCATTER_FLAGS([$($FREETYPE_tmp1 --libs) $($FREETYPE_tmp1 --cflags)],[FREETYPE])
  else
    LC_SCATTER_FLAGS([$(pkg-config --libs --cflags freetype2)],[FREETYPE])
  fi
  unset FREETYPE_tmp1
])

AC_DEFUN([LC_FREETYPE],[
  AC_ARG_WITH(freetype,
  AS_HELP_STRING([--with-freetype@<:@DIR=@:>@],
  [where to find freetype-config []]), [], [unset withval])

  if [[[ "$withval" != no ]]]
  then
    unset USE_FREETYPE
    LC_WITH_FREETYPE([withval]) #init FLAGS
    AX_SAVE_FLAGS
    LC_SET_FLAGS([FREETYPE])
    
    AC_CHECK_HEADER(ft2build.h, [
      LC_CHECK_LIB([freetype],[FT_Init_FreeType],[
        AC_CHECK_LIB([freetype],[FT_Get_PS_Font_Value],[
          LC_DEFINE(USE_FREETYPE, 3, [Use freetype library])
        ],[
          LC_DEFINE(USE_FREETYPE, 2, [Use freetype library])
        ],[$$0_extralibs])
      ],[:],[-lpng,-lpng16,-lz],[$0_extralibs])
    ],[
      AC_CHECK_HEADER(freetype.h, [
        LC_CHECK_LIB([freetype],[ft_init_freetype],[
          LC_DEFINE(USE_FREETYPE, 1, [Use freetype library])
        ],[:],[-lpng,-lpng16,-lz],[$0_extralibs])
      ])
    ])
    AX_RESTORE_FLAGS
    if [[[ $USE_FREETYPE ]]]
    then
      FREETYPE_LIBS+=" ${[$0]_extralibs}"
      LC_COMBINE_FLAGS([FREETYPE])
      AC_DEFINE(LINKED_FREETYPE, 1, [Freetype library available])
    else
      AC_MSG_ERROR([Cannot find a working freetype library.])
    fi
  else
    AC_MSG_WARN(disabling freetype support)
  fi
  LC_SUBST([FREETYPE])
])
