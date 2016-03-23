AC_DEFUN([LC_GS],[
  AC_ARG_WITH(gs,
  AS_HELP_STRING([--with-gs@<:@=ARG@:>@],
  [with ghostscript support [ARG=yes]]))

  if test "$with_gs" = "no" ; then
      AC_MSG_RESULT([disabling ghostscript support])
  else
  case "${host}" in
    *mingw*)
      GS_EXE="gswin32c.exe"
      GS_DLL="gsdll32.dll"
      AC_CHECK_PROG([HAS_GS_EXE], [${GS_EXE}], [yes], [no])
      AC_CHECK_PROG([HAS_GS_LIB], [${GS_DLL}], [yes], [no])
    ;;
    *)
      AC_CHECK_PROG([HAS_GS_EXE], [gs], [yes], [no])
      HAS_GS_LIB="yes"
    ;;
  esac
  if test "x${HAS_GS_EXE}" = "xyes" -a "x${HAS_GS_LIB}" = "xyes"; then
    AC_DEFINE([USE_GS], [1], [Use ghostscript])
    CONFIG_GS="Ghostscript"
    AC_SUBST([CONFIG_GS])
    AC_SUBST([GS_EXE])
    AC_SUBST([GS_DLL])
  else
    AC_MSG_RESULT([disabling ghostscript support])
  fi
  fi
])