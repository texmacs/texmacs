AC_DEFUN([LC_DLOPEN],[
  AC_CHECK_FUNC(dlopen)
  if test "$ac_cv_func_dl" = "yes"; then
    AC_DEFINE(DYNAMIC_LINKING, 1, [Dynamic linking works])
  else
    AC_CHECK_LIB(dl,dlopen)
    if test "$ac_cv_lib_dl_dlopen" = "yes"; then
      AC_CHECK_FUNCS(dlopen)
      CONFIG_BDL="-ldl"
      AC_DEFINE(DYNAMIC_LINKING, 1, [Dynamic linking works])
    else
      AC_CHECK_LIB(dld,dld_link)
      if test "$ac_cv_lib_dld_dld_link" = "yes"; then
        CONFIG_BDL="-ldl"
        AC_DEFINE(DYNAMIC_LINKING, 1, [Dynamic linking works])
      else
        AC_CHECK_FUNCS(shl_load)
        if test "$ac_cv_func_shl_load" = "yes"; then
          CONFIG_BDL="-ldl"
          AC_DEFINE(DYNAMIC_LINKING, 1, [Dynamic linking works])
        else
          AC_CHECK_FUNCS(dlopen)
          if test "$ac_cv_func_dlopen" = "yes"; then
            AC_DEFINE(DYNAMIC_LINKING, 1, [Dynamic linking works])
          fi
        fi
      fi
    fi
  fi
  AC_SUBST(CONFIG_BDL)
])