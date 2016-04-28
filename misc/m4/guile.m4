#--------------------------------------------------------------------
# Various test programs definition
#--------------------------------------------------------------------
AC_DEFUN([LM_FUNC_CHECK],[AC_LANG_PROGRAM([
 /* Override any GCC internal prototype to avoid an error.
    Use char because int might match the return type of a GCC
    builtin and then its argument prototype would still apply.  */
 #ifdef __cplusplus
 extern "C"
 #endif
 char [$1] ();
 ],[
 return [$1] ();
])])

AC_DEFUN([LM_GUILE_DOTS],[AC_LANG_PROGRAM([
@%:@include <gh.h>
@%:@include <GUILE_LIB_NAME.h>

typedef SCM (*FN)(...);

static SCM identity (SCM x) { return x; }
void declare () { gh_new_procedure ("identity", (FN) identity, 1, 0, 0); }
])])


AC_DEFUN([LM_GUILE_SIZE],[AC_LANG_PROGRAM([
@%:@include <gh.h>
@%:@include <GUILE_LIB_NAME.h>

void print_string (SCM s) {
  int len_r;
  char* r= gh_scm2newstr (s, &len_r); 
}
])])


#-------------------------------------------------------------------
# Modified version of GUILE_FLAGS in guile.m4
# from the official guile distribution
#-------------------------------------------------------------------

AC_DEFUN([LC_WITH_GUILE],[
	[guile_config=$1]
  AC_CHECK_PROGS(guile_config, guile18-config guile-config guile20-config, [])
  
  LC_WITH_GUILE_tmp1="$($guile_config link) $($guile_config compile)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 $($guile_config compile)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 -I$($guile_config info pkgincludedir)" && dnl
  GUILE_VERSION=$($guile_config info guileversion) && dnl
  GUILE_VERSIONT=${GUILE_VERSION#*.*.} &&
  GUILE_VERSION=${GUILE_VERSION%.$GUILE_VERSIONT} && dnl
  GUILE_DATA_PATH=$($guile_config info pkgdatadir)/${GUILE_VERSION} || dnl
  AC_MSG_ERROR([cannot find guile-config; is Guile installed?])
  LC_SCATTER_FLAGS([$LC_WITH_GUILE_tmp1], [GUILE_TMP])

  LC_SET_EMPTY_FLAGS([GUILE_TMP],[GUILE])
  LC_CLEAR_FLAGS([GUILE_TMP])

  # complete include path according the library name
  LC_GET_ARG_VALUE(GUILE_CPPFLAGS, [-I], [LC_WITH_GUILE_tmp2])
  LC_GET_ARG_VALUE(GUILE_LIBS, [-l], [GUILE_LIB])
  LC_APPEND_FLAG([-I$LC_WITH_GUILE_tmp2/$GUILE_LIB], [GUILE_CPPFLAGS])
  # Get the lib name
  GUILE_NUM=${GUILE_LIB@%:@guile}
  GUILE_NUM=${GUILE_NUM:-0}

  AC_DEFUN([GUILE_LIB_NAME], [lib$GUILE_LIB])
  unset LC_WITH_GUILE_tmp1 LC_WITH_GUILE_tmp2

  LC_COMBINE_FLAGS([GUILE],[guile])
  LC_SUBST(GUILE)
  GUILE_BIN="$GUILE_LIB"
  GUILE_EFFECTIVE_VERSION=`$GUILE_BIN -c '(display (version))'`
  GUILE_CONFIG="$guile_config"
  GUILE_CFLAGS="$GUILE_CPPFLAGS"
  GUILE_LDFLAGS=""
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
#  if [[ "$GUILE_VERSION" = "$GUILE_EFFECTIVE_VERSION" ]]
#  then echo ok
#  else
#    echo not ok
#    echo $GUILE_VERSION
#    echo $GUILE_EFFECTIVE_VERSION
#  fi
])

#-------------------------------------------------------------------
# For autodetection of flags required to link statically with Guile
#-------------------------------------------------------------------

AC_DEFUN(TEXMACS_LINK_GUILE,
[AC_TRY_LINK([
#include <guile/gh.h>
$CONFIG_DOTS
],[
  struct dummy {
    static void my_main (int argc, char** argv) {}
    static void install_guile () {
#ifdef DOTS_OK
      gh_enter (0, NULL, (void (*)(...)) ((void*) my_main));
#else
      gh_enter (0, NULL, my_main);
#endif
    }
  };
  dummy::install_guile ();
], $1, $2)
])

AC_DEFUN([LC_GUILE],[
  AC_ARG_WITH(guile,
  AS_HELP_STRING([--with-guile@<:@=DIR@:>@],
  [where to find guile-config [system]]), [], [unset withval])
  if [[[ "$withval" != no ]]]
  then
  AC_ARG_ENABLE(guile2,
  [  --enable-guile2         enable compilation with Guile 2, for development purposes],
      [], [enable_guile2="no"])

  if test -z "$GUILE_CFLAGS" -a -z "$GUILE_LDFLAGS"; then
    LC_WITH_GUILE
  fi

  AC_MSG_CHECKING(version of guile)
  if test -z "$GUILE_EFFECTIVE_VERSION" ; then
    GUILE_EFFECTIVE_VERSION=`$GUILE_BIN -c '(display (version))'`
  fi
  AC_MSG_RESULT($GUILE_EFFECTIVE_VERSION)

    case "$GUILE_VERSION" in
      1.0* | 1.1* | 1.2* | 1.3* | 1.4* | 1.5*) AC_DEFINE([GUILE_A],[1],[Guile version]) ;;
      1.6* | 1.7*) AC_DEFINE(GUILE_B,[1],[Guile version]) ;;
      2.*) AC_DEFINE(GUILE_D,[1],[Guile version])
        if test "$enableval" != "no"; then
          AC_MSG_ERROR([TeXmacs is incompatible with Guile 2.
    If you know what you are doing, run configure with --enable-guile2])
        fi 
      ;;
      *) AC_DEFINE(GUILE_C,[1],[Guile version]) ;;
    esac

  AC_MSG_CHECKING(guile data path)
  if test -z "$GUILE_DATA_PATH" ; then
    GUILE_DATA_PATH=`$GUILE_CONFIG info pkgdatadir`
  fi
  AC_MSG_RESULT($GUILE_DATA_PATH)

  AC_SUBST(GUILE_BIN)
  AC_SUBST(GUILE_DATA_PATH)
  else
    AC_MSG_ERROR([ cannot work without Guile])
  fi

  AC_MSG_CHECKING(whether ... arguments behave correctly)
  if test -z "$GUILE_CFLAGS"; then
    CXXFLAGS="`$GUILE_CONFIG compile`"
  else
    CXXFLAGS="$GUILE_CFLAGS"
  fi
        LC_RUN_IFELSE([Guile DOTS], [LM_GUILE_DOTS],[
          AC_DEFINE(DOTS_OK, 1, [Defined if ...-style argument passing works])
        ],[
  #       LC_MERGE_FLAGS([-fpermissive],[CXXFLAGS])
          LC_RUN_IFELSE([Guile DOTS with -fpermissive], [LM_GUILE_DOTS],[
          LC_MERGE_FLAGS([-fpermissive],[GUILE_CXXFLAGS])
            AC_DEFINE(DOTS_OK, 1, [Defined if ...-style argument passing works])])
        ])
  AC_MSG_CHECKING(the size_t of guile strings)
  if test -z "$GUILE_CFLAGS"; then
    CXXFLAGS="`$GUILE_CONFIG compile`"
  else
    CXXFLAGS="$GUILE_CFLAGS"
  fi
        AC_RUN_IFELSE([LM_GUILE_SIZE], [
          AC_DEFINE(guile_str_size_t, int, [Guile string size type])
          AC_MSG_RESULT(int)
        ],[
          AC_DEFINE(guile_str_size_t, size_t, [Guile string size type])
          AC_MSG_RESULT(size_t)
    ])
  CXXFLAGS=""

  CONFIG_GUILE_SERIAL="X"
  AC_SUBST(CONFIG_GUILE_SERIAL)
])
