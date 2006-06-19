
#-------------------------------------------------------------------
# Modified version of GUILE_FLAGS in guile.m4
# from the official guile distribution
#-------------------------------------------------------------------

AC_DEFUN([GUILE_FLAGS],[
## The GUILE_FLAGS macro.
  ## First, let's just see if we can find Guile at all.
  AC_MSG_CHECKING(for Guile)
  guile-config link > /dev/null || {
    echo "configure: cannot find guile-config; is Guile installed?" 1>&2
    exit 1
  }
  GUILE_ORIGINAL_CFLAGS="`guile-config compile`"
  GUILE_CFLAGS="$GUILE_ORIGINAL_CFLAGS"
  GUILE_VARIANT_CFLAGS="$GUILE_ORIGINAL_CFLAGS $GUILE_ORIGINAL_CFLAGS/guile $GUILE_ORIGINAL_CFLAGS/libguile"
  GUILE_LDFLAGS="`guile-config link`"
  GUILE_VARIANT_LDFLAGS="-L`guile-config info libdir` -lguile -lreadline -ltermcap"
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
  AC_MSG_RESULT(yes)
])
