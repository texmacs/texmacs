#-------------------------------------------------------------------
# Support for Freetype
#-------------------------------------------------------------------

AC_DEFUN([LC_WITH_FREETYPE],[
  AC_PATH_PROG(FREETYPE_tmp1, freetype-config, [no], [${$1:-$PATH}])
  if [[ $FREETYPE_tmp1 != no ]]
  then
    LC_SCATTER_FLAGS([$($FREETYPE_tmp1 --libs) $($FREETYPE_tmp1 --cflags)],[FREETYPE])
  else
    AC_MSG_WARN([freetype-config not found])
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
    LC_WITH_FREETYPE([withval])	#init FLAGS
    AX_SAVE_FLAGS
    LC_SET_FLAGS([FREETYPE])

    AC_CHECK_HEADER(ft2build.h, [
      AC_CHECK_LIB([freetype],[FT_Init_FreeType],[
        AC_CHECK_LIB([freetype],[FT_Get_PS_Font_Value],[
          LC_DEFINE(USE_FREETYPE, 3, [Use freetype library])
        ],[
          LC_DEFINE(USE_FREETYPE, 2, [Use freetype library])
        ])])
    ],[
      AC_CHECK_HEADER(freetype.h, [
        AC_CHECK_LIB([freetype],[ft_init_freetype],[
          LC_DEFINE(USE_FREETYPE, 1, [Use freetype library])
        ])
      ])
    ])
    AX_RESTORE_FLAGS
    if [[[ $USE_FREETYPE ]]]
    then
      LC_COMBINE_FLAGS([FREETYPE],[freetype])
      AC_DEFINE(LINKED_FREETYPE, 1, [Freetype library available])
    else
      AC_MSG_ERROR([Cannot find a working freetype library.])
    fi
  else
    AC_MSG_WARN(disabling freetype support)
  fi
  LC_SUBST([FREETYPE])
])
