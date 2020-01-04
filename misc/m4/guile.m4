
#--------------------------------------------------------------------
#
# MODULE      : guile.m4
# DESCRIPTION : TeXmacs configuration options for Guile
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Compiling Guile if shipped with TeXmacs
#--------------------------------------------------------------------

AC_DEFUN([LC_BUILD_GUILE],[
  TEXMACS_SRC="$PWD"
  GUILE_SRC="$PWD/guile"
  if [[ -d "$GUILE_SRC" ]];
  then
    if [[ ! -d "$GUILE_SRC/build" ]];
    then
      mkdir $GUILE_SRC/build
      #cd $GUILE_SRC; ./configure --disable-shared --enable-static --prefix=$GUILE_SRC/build
      #cd $GUILE_SRC; ./configure --disable-shared --enable-static --disable-networking --disable-error-on-warning --disable-nls --prefix=$GUILE_SRC/build
      cd $GUILE_SRC; ./configure --prefix=$GUILE_SRC/build
      cd $GUILE_SRC; make
      cd $GUILE_SRC; make install
      cd $TEXMACS_SRC
    fi
    PATH="$GUILE_SRC/build/bin:$PATH"
  fi
])

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
  if test -z $1
  then
    # build the prefered guile version search line
    m4_define(configlist, m4_split(m4_combine([ ],[guile],[],[18],[1.8],[16],[1.6],[1],[],[20],[2.0],[2])))
    AC_CHECK_PROGS(GUILE_CONFIG, m4_combine([ ],[configlist],[-],[config]))
  else GUILE_CONFIG=$1
  fi
  GUILE_CONFIG=$(type -p $GUILE_CONFIG)
  GUILE_EXE=${GUILE_CONFIG%-config}
  AC_SUBST(GUILE_CONFIG)
  AC_SUBST(GUILE_EXE)
  LC_WITH_GUILE_tmp1="$($GUILE_CONFIG link)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 $($GUILE_CONFIG compile)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 -I$($GUILE_CONFIG info includedir)" && dnl
  LC_WITH_GUILE_tmp1="$LC_WITH_GUILE_tmp1 -I$($GUILE_CONFIG info pkgincludedir)" && dnl
  # get th version with guile-config or guile. keep the same naming with guile version 
  # ie: guile18-config -> guile18
  { GUILE_VERSION=$($GUILE_CONFIG info guileversion) || dnl
   GUILE_VERSION=$(${GUILE_CONFIG%%-*} -c '(display (version))'); } && dnl
  GUILE_VERSION=${GUILE_VERSION:-0} && dnl
  GUILE_VERSION_TAIL=${GUILE_VERSION@%:@*.*.} && dnl
  GUILE_VERSION=${GUILE_VERSION%.$GUILE_VERSION_TAIL} && dnl
  GUILE_DATA_PATH=$($GUILE_CONFIG info pkgdatadir)/${GUILE_VERSION} || dnl
  AC_MSG_ERROR([cannot find guile-config; is Guile installed?])
  LC_CLEAR_FLAGS([GUILE])
  LC_SCATTER_FLAGS([$LC_WITH_GUILE_tmp1], [GUILE])

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
        libguile16) AC_DEFINE(GUILE_HEADER_16, 1, [Guile 1.6 header]) ;;
        libguile18) AC_DEFINE(GUILE_HEADER_18, 1, [Guile 1.8 header]) ;;
        *) AC_MSG_WARN([Strange guile header name GUILE_LIB_NAME.h]) ;;
      esac
      LC_CHECK_LIB([${GUILE_LIB}],[gh_scm2newstr],[
        g_success=1
        LC_RUN_IFELSE([Guile DOTS], [LM_GUILE_DOTS],[
          AC_DEFINE(DOTS_OK, 1, [Defined if ...-style argument passing works])
        ])
        AC_MSG_CHECKING(Guile size type)
        AC_RUN_IFELSE([LM_GUILE_SIZE], [
          AC_DEFINE(guile_str_size_t, int, [Guile string size type])
          AC_MSG_RESULT(int)
        ],[
          AC_DEFINE(guile_str_size_t, size_t, [Guile string size type])
          AC_MSG_RESULT(size_t)
        ])
      ],[AC_MSG_WARN([Cannot use guile])],[-lintl,-liconv,-ltre],[$0_extralibs])
    ])
  ])
  # AC_CHECK_LIB might have completed LIBS we need to complete GUILE_LIBS
  GUILE_LIBS+=${$0_extralibs:+ ${$0_extralibs}}
  if [[ ! $g_success ]];then 
    AC_MSG_ERROR([It seems that guile-config does not provide the right parameters.
    Consult the config.log for error details and check your guile installation])
    unset g_success
  fi

  CONFIG_GUILE_SERIAL="X"
  AC_SUBST(CONFIG_GUILE_SERIAL)

  AX_RESTORE_FLAGS
  LC_COMBINE_FLAGS([GUILE])
  LC_SUBST([GUILE])
  unset ${![$0]_*}
])
