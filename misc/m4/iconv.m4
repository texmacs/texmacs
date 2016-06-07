#--------------------------------------------------------------------
# Checks for iconv library
#--------------------------------------------------------------------

AC_DEFUN([LC_ICONV],[
AC_ARG_WITH(iconv,
AS_HELP_STRING([--with-iconv@<:@=DIR@:>@], [where to find iconv []]),
	[], [unset withval;])

if [[[ "$withval" != no ]]]
then
  [$0]_TEMP=$(mktemp -t texmacs)

  [$0]_PATH=$(echo ${LDFLAGS//\/lib/\/include})
  AX_SAVE_FLAGS
  CPPFLAGS="${[$0]_PATH//-L/-I} -I$withval/include -I/opt/local/include -I/sw/include -MM -MF $[$0]_TEMP"

  LC_CLEAR_FLAGS([ICONV]) # no previous iconv definition allowed
  AC_CHECK_HEADER(iconv.h, [
    [$0]_DEPEND=$(<$[$0]_TEMP)
    [$0]_ICONV=${[$0]_DEPEND%/include/iconv.h*}
    if  @<:@@<:@ "$[$0]_DEPEND" == "$[$0]_ICONV" @:>@@:>@
    then  AX_RESTORE_FLAGS
      AC_MSG_NOTICE([iconv found in default compiler location])
    else [$0]_ICONV=${[$0]_ICONV##* }
      AC_MSG_NOTICE([iconv found in $$0_ICONV])
      LC_SET_TRIVIAL_FLAGS([ICONV],[$$0_ICONV])
      LC_SET_FLAGS([ICONV])
    fi
    rm $[$0]_TEMP
    AC_CHECK_HEADER(iconv.h, [
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[@%:@include <iconv.h>]],
        [[iconv_open("",""); ]])],[iconv],[
        AX_RESTORE_FLAGS
        LC_COMBINE_FLAGS([ICONV])
        AC_DEFINE(USE_ICONV, 1, [Use iconv library])
        unset i_failure
        unset [$0]_LIBPATH
      ],[AC_MSG_ERROR([Cannot use iconv.h.])])
    ],[
      AC_MSG_ERROR([libiconv does not match header.])])
  ],[
    rm $[$0]_TEMP
    AC_MSG_ERROR([use with-iconv=iconv_base_path (i.e /usr/local) to specify your icon location.
Use with-iconv=no to ignore iconv.])
  ],[-])
else AC_MSG_WARN([absence of iconv may crash HTML import or prevent the build])
fi
LC_SUBST([ICONV])
])
