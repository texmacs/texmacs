
#--------------------------------------------------------------------
#
# MODULE      : tm_debug.m4
# DESCRIPTION : Debugging options
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_DEBUG],[
  #STD_DEBUG_FLAGS="-fno-rtti -fno-exceptions"
  #if test "$enable_cocoa" = "yes"; then
  #    STD_DEBUG_FLAGS="-fno-exceptions"
  #fi

  STD_DEBUG_FLAGS="-fno-rtti"
  if test "$enable_cocoa" = "yes"; then
      STD_DEBUG_FLAGS=""
  fi

  AC_MSG_CHECKING(if compiler supports $STD_DEBUG_FLAGS)
  CXXFLAGS="$STD_DEBUG_FLAGS"
  AC_TRY_COMPILE([
    int some_variable = 0;
  ],[
  ],[
      CONFIG_CXXDIALECT="$STD_DEBUG_FLAGS"
      AC_MSG_RESULT(yes)
  ],[
      CONFIG_CXXDIALECT=""
      AC_MSG_RESULT(no)
  ])
  CXXFLAGS=""
  AC_SUBST(CONFIG_CXXDIALECT)

  #--- Profiling

  AC_ARG_ENABLE(profile,
  [  --enable-profile        enable gcc compilation flag for profiling with gprof],
      [], [enable_profile="no"])

  if test "$enable_profile" = "yes"; then
      if test "$GXX" = "yes"; then
	  enable_profile="yes"
      else
	  enable_profile="none"
      fi
  fi

  case "$enable_profile" in
      yes)
	  AC_MSG_RESULT([enabling gcc profiling compilation flag -pg])
	  CONFIG_CXXPROFILE="-pg"
	  ;;
      none)
	  AC_MSG_RESULT([disabling gcc profiling compilation flag])
	  CONFIG_CXXPROFILE=""
	  ;;
      *)
	  CONFIG_CXXPROFILE=""
	  ;;
  esac

  #--- Debugging

  AC_ARG_ENABLE(debug,
  AS_HELP_STRING([--enable-debug@<:@=ARG@:>@],
  [install a debugging enable executable [-ggdb]]),
  [], [enable_debug="no"])

  if test "$enable_debug" = "yes"; then
      if test "$GXX" = "yes"; then
	  enable_debug="-ggdb"
      else
	  enable_debug="-g3"
      fi
  fi

  if test "$enable_debug" = "no"; then
      AC_MSG_RESULT([disabling debugging])
      CONFIG_STRIP="strip"
      CONFIG_CXXDEBUG=""
      optimize_default="yes"
  else
      AC_MSG_RESULT([enabling debugging, $enable_debug])
      CONFIG_STRIP="true"
      CONFIG_CXXDEBUG="$enable_debug"
      optimize_default="no"
  fi

  #--- Assert

  AC_ARG_ENABLE(assert,
  AS_HELP_STRING([--enable-assert],
  [exit on failed assertions in code]),
  [], [enable_assert="yes"])

  if test "$enable_assert" = "no"; then
      AC_MSG_RESULT([disabling assertion checking])
  else
      AC_MSG_RESULT([enabling assertion checking])
      AC_DEFINE(DEBUG_ASSERT, 1, [check assertions in code])
  fi

  #--- Warnings

  case "$GXX_VERSION" in
    none)
      CONFIG_CXXWARNING="";
    ;;
    *)
      CONFIG_CXXWARNING="-Wall -Wno-return-type"
    ;;
  esac

  AC_ARG_ENABLE(warnings,
  [  --enable-warnings       print warning messages during compilation],
      [], [enable_warnings="yes"])
  case "$enable_warnings" in
      yes)
	  AC_MSG_RESULT([enabling warning flags $CONFIG_CXXWARNING]) ;;
      no)
	  CONFIG_CXXWARNING=""
	  AC_MSG_RESULT([disabling warning flags]) ;;
      *)
	  CONFIG_CXXWARNING="$enable_warnings"
	  AC_MSG_RESULT([enabling warning flags $CONFIG_CXXWARNING]) ;;
  esac

  #--- Experimental mode

  AC_ARG_ENABLE(experimental,
  [  --enable-experimental   support new style rewriting code],
      [], [enable_experimental="no"])
  case "$enable_experimental" in
      yes)
	  AC_MSG_RESULT([enabling experimental style rewriting code])
	  AC_DEFINE(EXPERIMENTAL, 1, [Enable experimental style rewriting code])
	  CONFIG_EXPERIMENTAL="Memorizer Environment Evaluate"
	  ;;
      no)
	  AC_MSG_RESULT([disabling experimental style rewriting code])
	  CONFIG_EXPERIMENTAL=""
	  ;;
      *)
	  AC_MSG_ERROR([bad option --enable-experimental=$enable_experimental])
	  ;;
  esac
  AC_SUBST(CONFIG_EXPERIMENTAL)
])
