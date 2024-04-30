
#--------------------------------------------------------------------
#
# MODULE      : iconv.m4
# DESCRIPTION : TeXmacs configuration options for iconv library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

m4_define([PROG_ICONV], [AC_LANG_PROGRAM([[@%:@include <iconv.h>]],
        [[iconv_open("",""); ]])])


AC_DEFUN([LC_ICONV],[
AC_ARG_WITH(iconv,
AS_HELP_STRING([--with-iconv@<:@=DIR@:>@], [where to find iconv []]),
  [], [unset withval;])

if [[[ "$withval" != no ]]]
then _LC_ICONV([MM])
else AC_MSG_WARN([absence of iconv may crash HTML import or prevent the build])
fi
LC_SUBST([ICONV])
])

AC_DEFUN([_LC_ICONV],[
  [$0]_TEMP=$(mktemp -t texmacsXXXXXX)

  [$0]_PATH=$(echo ${LDFLAGS//\/lib/\/include})
  AX_SAVE_FLAGS
  # try to find a header somewhere in this path and ask for its location
  CPPFLAGS="${[$0]_PATH//-L/-I} -I$withval/include -I/opt/local/include -I/sw/include -[$1] -MF $[$0]_TEMP"

  LC_CLEAR_FLAGS([ICONV]) # no previous iconv definition allowed
  AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[#include<iconv.h>]])], [
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
    AX_RESTORE_FLAGS
    LC_SET_FLAGS([ICONV])
    rm $[$0]_TEMP
    AC_CHECK_HEADER(iconv.h, [
      LC_LINK_IFELSE([iconv],[PROG_ICONV],[
        AX_RESTORE_FLAGS
        LC_COMBINE_FLAGS([ICONV])
        AC_DEFINE(USE_ICONV, 1, [Use iconv library])
      ],[
        # it could be a bug this default locations management, try to force them
        m4_if([$1], [MM], [
          AX_RESTORE_FLAGS
          unset ac_cv_header_iconv_h  # clear the cache....
          AC_MSG_WARN([Try to force default location])
          _LC_ICONV([M])
        ],[
          # iconv must be include in LIBC 
          unset LIBS ICONV_LIBS
          LC_LINK_IFELSE([iconv in default libs],[PROG_ICONV],[
            AX_RESTORE_FLAGS
            LC_COMBINE_FLAGS([ICONV])
            AC_DEFINE(USE_ICONV, 1, [Use iconv library])
          ],[
            #no iconv
            LC_CLEAR_FLAGS([ICONV])
            AC_MSG_WARN([Cannot use iconv.h.])
          ])
        ])
      ])
    ],[
      AC_MSG_ERROR([libiconv does not match header.])])
  ],[
    rm $[$0]_TEMP
    AC_MSG_WARN([Use --with-iconv=iconv_base_path (i.e /usr/local) to specify your icon location])
  ])
  unset ${![$0]_*}
])
