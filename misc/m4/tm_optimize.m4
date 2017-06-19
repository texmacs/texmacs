
#--------------------------------------------------------------------
#
# MODULE      : tm_optimize.m4
# DESCRIPTION : Optimization options
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_OPTIMIZE],[
  if test "$optimize_default" = "yes"; then
    case "$GXX_VERSION" in
      3.3.3 | 3.3.4 | 4.*)
	;;
      2.96 | 3.0 | 3.0.* | 3.1 | 3.1.* | 3.2 | 3.2.* | 3.3 | 3.3.*)
	case "${host}" in
	  i*86-*-linux-gnu* | i*86-*-freebsd*)
	    AC_MSG_WARN([using g++ 3.*, optimize without inline by default])
	    optimize_default="no-inline"
	  ;;
	  *)
	    AC_MSG_WARN([using g++ 3.*, optimize without inline by default])
	    optimize_default="no-inline"
	  ;;
	esac
      ;;
    esac
  fi

  AC_ARG_ENABLE(optimize,
  AS_HELP_STRING([--enable-optimize@<:@=ARG@:>@],
  [compile with optimizations [guessed]]),
  [], [enable_optimize="$optimize_default"])

  case "$enable_optimize" in
      yes)
	  # keep optimization options
	  AC_MSG_RESULT([enabling optimizations, $CONFIG_CXXOPTIMIZE]) ;;
      no-inline)
	  optimize_no_inline="-fno-default-inline -fno-inline"
	  CONFIG_CXXOPTIMIZE="$CONFIG_CXXOPTIMIZE $optimize_no_inline"
	  AC_MSG_RESULT([enabling optimizations, $CONFIG_CXXOPTIMIZE]) ;;
      no)
	  CONFIG_CXXOPTIMIZE=""
	  AC_MSG_RESULT([disabling optimizations]) ;;
      *)
	  CONFIG_CXXOPTIMIZE="$enable_optimize"
	  AC_MSG_RESULT([customizing optimizations, $enable_optimize]) ;;
  esac
])
