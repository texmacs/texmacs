
#--------------------------------------------------------------------
#
# MODULE      : scheme.m4
# DESCRIPTION : Selection of the Scheme interpreter (S7 or Guile)
# COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_SCHEME],[
  AC_ARG_WITH(scheme,
    AS_HELP_STRING([--with-scheme@<:@=s7|guile@:>@],
                   [Scheme interpreter to use @<:@default=s7@:>@]),
    [], [with_scheme=s7])
  case "$with_scheme" in
    (s7 | S7)
      AC_MSG_NOTICE([using the S7 Scheme interpreter (vendored in src/Scheme/S7)])
      AC_DEFINE([USE_S7], [1], [Use the S7 Scheme interpreter])
      SCHEME_DIR=S7
      ;;
    (guile | Guile)
      AC_MSG_NOTICE([using the Guile Scheme interpreter])
      AC_DEFINE([USE_GUILE], [1], [Use the Guile Scheme interpreter])
      SCHEME_DIR=Guile
      LC_GUILE
      ;;
    (*)
      AC_MSG_ERROR([unknown Scheme interpreter '$with_scheme', use s7 or guile])
      ;;
  esac
  AC_SUBST(SCHEME_DIR)
])
