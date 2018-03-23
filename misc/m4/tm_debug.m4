
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

AC_DEFUN([AC_CPLUSPLUS_STACK],[
  AC_MSG_CHECKING(for C++ stack backtrace support)
  AC_RUN_IFELSE([AC_LANG_PROGRAM([
#include <stdio.h>
#include <stdlib.h>
#include <execinfo.h>
#include <cxxabi.h>
], [
    FILE* out= stderr;
    unsigned int max_frames= 63;
    fprintf(out, "stack trace:\n");
    void* addrlist[[max_frames+1]];
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void*));
    if (addrlen == 0) return 1;
    char** symbollist = backtrace_symbols(addrlist, addrlen);
    size_t funcnamesize = 256;
    char* funcname = (char*)malloc(funcnamesize);
    for (int i = 1; i < addrlen; i++) {
        char *begin_name = 0, *begin_offset = 0, *end_offset = 0;
        for (char *p = symbollist[[i]]; *p; ++p) {
            if (*p == '(')
                begin_name = p;
            else if (*p == '+')
                begin_offset = p;
            else if (*p == ')' && begin_offset) {
                end_offset = p;
                break;
            }
        }
        if (begin_name && begin_offset && end_offset
            && begin_name < begin_offset)
        {
            *begin_name++ = '\0';
            *begin_offset++ = '\0';
            *end_offset = '\0';
            int status;
            char* ret = abi::__cxa_demangle(begin_name,
                                            funcname, &funcnamesize, &status);
            if (status == 0) {
                funcname = ret;
                fprintf(out, "  %s : %s+%s\n",
                        symbollist[[i]], funcname, begin_offset);
            }
            else {
                fprintf(out, "  %s : %s()+%s\n",
                        symbollist[[i]], begin_name, begin_offset);
            }
        }
        else fprintf(out, "  %s\n", symbollist[[i]]);
    }
    free(funcname);
    free(symbollist);
    return 0;
  ])],[
    AC_MSG_RESULT(yes)
    AC_DEFINE(USE_STACK_TRACE, 1, [Use C++ stack backtraces])
  ],[
    AC_MSG_RESULT(no)
  ],[
    AC_MSG_RESULT(no)
  ])
])

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
  AX_SAVE_FLAGS
  CXXFLAGS=$STD_DEBUG_FLAGS
  AC_TRY_COMPILE([
    int some_variable = 0;
  ],[
  ],[
      CONFIG_CXXDIALECT="$STD_DEBUG_FLAGS"
      AC_MSG_RESULT(yes)
  ],[
      unset STD_DEBUG_FLAGS
      CONFIG_CXXDIALECT=""
      AC_MSG_RESULT(no)
  ])
  CXXFLAGS=""
  AX_RESTORE_FLAGS
  CXXFLAGS+=${STD_DEBUG_FLAGS:+ $STD_DEBUG_FLAGS}
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
	  enable_debug="-O0 -ggdb -pg"
      else
	  enable_debug="-O0 -g3 -pg"
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
      AC_DEFINE([DEBUG_ON], 1, [debugging built])
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
