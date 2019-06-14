
#--------------------------------------------------------------------
#
# MODULE      : sign.m4
# DESCRIPTION : Code signing
# COPYRIGHT   : (C) 2016, 2017  Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_SIGN],[
  AC_ARG_ENABLE(sign,
  AS_HELP_STRING([--enable-sign@<:@=ARG@:>@],
  [Code signing ID]))

  if test "$enable_sign" = "no" -o "$enable_sign" = "" ; then
      AC_MSG_RESULT([no code signing])
  else
    case "${CONFIG_OS}" in
    (MACOS)
      if type codesign >/dev/null
      then  AC_MSG_RESULT([signing enabled])
            AC_SUBST(SIGNID,[$enable_sign])
      else AC_MSG_RESULT([signing disabled])
      fi;;
    (*) AC_MSG_RESULT([no code signing available yet]);;
    esac
  fi
])
