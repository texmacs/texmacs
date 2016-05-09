#--------------------------------------------------------------------
# Checks for iconv library
#--------------------------------------------------------------------

AC_DEFUN([LC_ICONV],[
AC_ARG_WITH(iconv,
AS_HELP_STRING([--with-iconv@<:@=DIR@:>@], [where to find iconv []]),
	[], [unset withval;])

if [[[ "$withval" != no ]]]
then
  i_failure=1
  
  [$0]_LIBPATHS="$LDFLAGS $withval -L/opt/local/lib -L/sw/lib"
  
  LC_GET_ARG_VALUE([$0]_LIBPATHS, [-L], [$0]_LIBPATH)
  STRIP_ARG([[$0]_LIBPATHS], -L$[$0]_LIBPATH)
  
  while test -n "$[$0]_LIBPATH"
  do
    LC_CLEAR_FLAGS([ICONV]) # no external iconv definition allowed
    LC_SET_TRIVIAL_FLAGS([ICONV],[${$0_LIBPATH]%/lib})
    AX_SAVE_FLAGS	
    LC_SET_FLAGS([ICONV])
    AC_CHECK_HEADER(iconv.h, [
      LC_LINK_IFELSE([iconv], [AC_LANG_PROGRAM([[@%:@include <iconv.h>]],
        [[iconv_open("",""); ]])
      ], [
        AX_RESTORE_FLAGS
        LC_COMBINE_FLAGS([ICONV],[iconv])
        AC_DEFINE(USE_ICONV, 1, [Use iconv library])
        unset i_failure
        unset [$0]_LIBPATH
      ], [
        AC_MSG_ERROR([you may have several versions of iconv installed,
          use with-iconv=iconv_base_path (i.e /usr/local) to specify one location])
    ])],[
      LC_GET_ARG_VALUE([$0]_LIBPATHS, [-L], [$0]_LIBPATH)
      STRIP_ARG([[$0]_LIBPATHS], -L$[$0]_LIBPATH)
    ])
  done
  if [[ $i_failure ]]
  then AC_MSG_ERROR([absence of iconv may crash HTML import or prevent the build
        but it is possible to run configure --with-iconv=no])
  fi
fi
LC_SUBST([ICONV])
])
