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
  # try to find a header somewhere in this path and ask for its location
  CPPFLAGS="${[$0]_PATH//-L/-I} -I$withval/include -I/opt/local/include -I/sw/include -MM -MF $[$0]_TEMP"

  LC_CLEAR_FLAGS([ICONV]) # no previous iconv definition allowed
  AC_CHECK_HEADER(iconv.h, [
    [$0]_DEPEND=$(<$[$0]_TEMP)
    [$0]_ICONV=${[$0]_DEPEND%/include/iconv.h*}
    if  @<:@@<:@ "$[$0]_DEPEND" == "$[$0]_ICONV" @:>@@:>@
    then
      # don't add the default path to the LDFLAGS
      AC_MSG_NOTICE([iconv found in default compiler location])
      LC_MERGE_FLAGS([-liconv],[ICONV_LIBS])
    else
      [$0]_ICONV=${[$0]_ICONV@%:@@%:@* }
      AC_MSG_NOTICE([iconv found in $$0_ICONV])
      LC_SET_TRIVIAL_FLAGS([ICONV],[$$0_ICONV])
    fi
    LC_SET_FLAGS([ICONV])
    rm $[$0]_TEMP
    AC_CHECK_HEADER(iconv.h, [
      LC_LINK_IFELSE([iconv],[AC_LANG_PROGRAM([[@%:@include <iconv.h>]],
        [[iconv_open("",""); ]])],[
        AX_RESTORE_FLAGS
        LC_COMBINE_FLAGS([ICONV])
        AC_DEFINE(USE_ICONV, 1, [Use iconv library])
      ],[AC_MSG_ERROR([Cannot use iconv.h.])])
    ],[
      AC_MSG_ERROR([libiconv does not match header.])])
  ],[
    rm $[$0]_TEMP
    AC_MSG_ERROR([Use with-iconv=iconv_base_path (i.e /usr/local) to specify your icon location.  Use with-iconv=no to ignore iconv.])
  ],[-])
else AC_MSG_WARN([absence of iconv may crash HTML import or prevent the build])
fi
LC_SUBST([ICONV])
unset ${![$0]_*}
])
