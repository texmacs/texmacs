
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
 # build the prefered guile version search line
  AC_CHECK_PROGS(guile_config, guile18-config guile17-config guile16-config guile-config guile20-config, [])

  LC_WITH_GUILE_tmp1="$($guile_config link)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 $($guile_config compile)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 -I$($guile_config info pkgincludedir)" && dnl
  # get th version with guile-config or guile. keep the same naming with guile version 
  # ie: guile18-config -> guile18
  { GUILE_VERSION=$($guile_config info guileversion) || dnl
   GUILE_VERSION=$(${guile_config%%-*} -c '(display (version))'); } && dnl
  GUILE_VERSION=${GUILE_VERSION:-0} && dnl
  GUILE_VERSION_TAIL=${GUILE_VERSION@%:@*.*.} && dnl
  GUILE_VERSION=${GUILE_VERSION%.$GUILE_VERSION_TAIL} && dnl
  GUILE_DATA_PATH=$($guile_config info pkgdatadir)/${GUILE_VERSION} || dnl
  AC_MSG_ERROR([cannot find guile-config; is Guile installed?])
  LC_SCATTER_FLAGS([$LC_WITH_GUILE_tmp1], [GUILE_TMP])

  LC_SET_EMPTY_FLAGS([GUILE_TMP],[GUILE])
  LC_CLEAR_FLAGS([GUILE_TMP])

  # complete include path according the library name
  LC_GET_ARG_VALUE(GUILE_CPPFLAGS, [-I], [LC_WITH_GUILE_tmp2])
  LC_GET_ARG_VALUE(GUILE_LIBS, [-l], [GUILE_LIB])
  LC_APPEND_FLAG([-I$LC_WITH_GUILE_tmp2/$GUILE_LIB], [GUILE_CPPFLAGS])

  AC_DEFUN([GUILE_LIB_NAME], [lib$GUILE_LIB])
  unset LC_WITH_GUILE_tmp1 LC_WITH_GUILE_tmp2
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
    LC_WITH_GUILE($withval)

    AC_ARG_ENABLE(guile2,
      AS_HELP_STRING([--disable-guile2=[yes]],[enable compilation with Guile 2, for development purposes]),
      [], [unset enableval])

    AC_MSG_NOTICE(Guile version $GUILE_VERSION)

    case "$GUILE_VERSION" in
      1.0 | 1.1 | 1.2 | 1.3 | 1.4 | 1.5) AC_DEFINE([GUILE_A],[1],[Guile version]) ;;
      1.6 | 1.7) AC_DEFINE(GUILE_B,[1],[Guile version]) ;;
      1.8 | 1.9) AC_DEFINE(GUILE_C,[1],[Guile version]) ;;
      2.*) AC_DEFINE(GUILE_D,[1],[Guile version])
        if test "$enableval" != "no"; then
          AC_MSG_ERROR([TeXmacs is incompatible with Guile 2.
    If you know what you are doing, run configure with --enable-guile2])
        fi 
        ;;
      0) AC_MSG_ERROR([Cannot determine Guile version.]) ;;
      *) AC_MSG_ERROR([Guile version unmanaged.]) ;;
    esac

    AC_MSG_NOTICE([Guile data path: $GUILE_DATA_PATH])

    AC_DEFINE_UNQUOTED([GUILE_VERSION], [$GUILE_VERSION], [Guile version])
    AC_SUBST(GUILE_DATA_PATH)
  else
    AC_MSG_ERROR([ cannot work without Guile])
  fi

  AX_SAVE_FLAGS	
  LC_SET_FLAGS([GUILE])

  unset g_success
  AC_CHECK_HEADER(gh.h, [
    AC_CHECK_HEADER(GUILE_LIB_NAME.h, [
      case GUILE_LIB_NAME in
        libguile) ;;
        libguile18) AC_DEFINE(GUILE_HEADER_18, 1, [Guile 1.8 header]) ;;
        *) AC_MSG_WARN([Strange guile header name GUILE_LIB_NAME.h]) ;;
      esac
      LC_LINK_IFELSE([Guile],[LM_FUNC_CHECK([gh_scm2newstr])], [
        g_success=1
        LC_RUN_IFELSE([Guile DOTS], [LM_GUILE_DOTS],[
          AC_DEFINE(DOTS_OK, 1, [Defined if ...-style argument passing works])
        ],[
#         LC_MERGE_FLAGS([-fpermissive],[CXXFLAGS])
#         LC_RUN_IFELSE([Guile DOTS with -fpermissive], [LM_GUILE_DOTS],[
#         LC_MERGE_FLAGS([-fpermissive],[GUILE_CXXFLAGS])
#           AC_DEFINE(DOTS_OK, 1, [Defined if ...-style argument passing works])])
        ])
        AC_RUN_IFELSE([LM_GUILE_SIZE], [
          AC_DEFINE(guile_str_size_t, int, [Guile string size type])
          AC_MSG_RESULT(checking for Guile size type... int)
        ],[
          AC_DEFINE(guile_str_size_t, size_t, [Guile string size type])
          AC_MSG_RESULT(checking for Guile size type... size_t)
    ])])])])
    if [[ ! $g_success ]];then 
      AC_MSG_ERROR([It seems that guile-config does not provide the right parameters.
      Consult the config.log for error details and check your guile installation])
    unset g_success
  fi

  CONFIG_GUILE_SERIAL="X"
  AC_SUBST(CONFIG_GUILE_SERIAL)

  AX_RESTORE_FLAGS
  LC_SUBST([GUILE])
  LC_COMBINE_FLAGS([GUILE],[])
])
