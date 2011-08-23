
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

#-------------------------------------------------------------------
# Modify the X include files to make them C++-compatible, if needed
#-------------------------------------------------------------------

AC_DEFUN(CPP_X_HEADERS,[
AC_MSG_CHECKING(for C++-compatible X header files)
ac_save_cppflags="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
AC_TRY_COMPILE([#include <X11/Xlib.h>
#include <X11/Xutil.h>],,
echo yes,
[rm -rf X11
mkdir X11
for ac_dir in                \
  /usr/X11/include           \
  /usr/X11R6/include         \
  /usr/X11R5/include         \
  /usr/X11R4/include         \
                             \
  /usr/include/X11           \
  /usr/include/X11R6         \
  /usr/include/X11R5         \
  /usr/include/X11R4         \
                             \
  /usr/local/X11/include     \
  /usr/local/X11R6/include   \
  /usr/local/X11R5/include   \
  /usr/local/X11R4/include   \
                             \
  /usr/local/include/X11     \
  /usr/local/include/X11R6   \
  /usr/local/include/X11R5   \
  /usr/local/include/X11R4   \
                             \
  /usr/X386/include          \
  /usr/x386/include          \
  /usr/XFree86/include/X11   \
                             \
  /usr/include               \
  /usr/local/include         \
  /usr/unsupported/include   \
  /usr/athena/include        \
  /usr/local/x11r5/include   \
  /usr/lpp/Xamples/include   \
                             \
  /usr/openwin/include       \
  /usr/openwin/share/include \
  ; \
do
  if test -r "$ac_dir/X11/Xlib.h"; then
    tm_x_includes=$ac_dir
    break
  fi
done
sed 's/^extern \(X[[a-zA-Z0-9]]*(\)/extern int \1/' \
  < "$tm_x_includes/X11/Xlib.h" > X11/Xlib.h
sed 's/^extern \(X[[a-zA-Z0-9]]*(\)/extern int \1/' \
  < "$tm_x_includes/X11/Xutil.h" > X11/Xutil.h
X_CFLAGS="-I.. $X_CFLAGS"
echo "no; fixing"])
CPPFLAGS="$ac_save_cppflags"
])

#-------------------------------------------------------------------
# Support for Qt
#-------------------------------------------------------------------

m4_include([misc/autotroll/autotroll.m4])

AC_DEFUN([HACKED_AT_WITH_QT],[
  if test -r "/c/Qt"; then
    MOC="`which moc`"
    qt_bin="`dirname $MOC`"
    qt_home="`dirname $qt_bin`"
    qt_version="`basename $qt_home`"
    qt_parent="`dirname $qt_home`"
    QT_CPPFLAGS="-I$qt_home/mkspecs/macx-g++ -I$qt_home/include/QtCore -I$qt_home/include/QtGui -I$qt_home/include -I/$qt_home/include/ActiveQt -I."
    QT_CXXFLAGS="-pipe -g -Wall -W -DQT_DLL -DQT_GUI_LIB -DQT_CORE_LIB -DQT_THREAD_SUPPORT"
    QT_LDFLAGS="-enable-stdcall-fixup -Wl,-enable-auto-import -Wl,-enable-runtime-pseudo-reloc -mthreads -Wl -Wl,-subsystem,windows"
    QT_LIBS="-L'c:/Qt/$qt_version/lib' -lmingw32 -lqtmaind -lQtGuid4 -lQtCored4"
    if test "$qt_parent" != "/c/Qt"; then
      at_cv_qt_build="ko"
    fi
  else
    AT_WITH_QT
  fi
])

#-------------------------------------------------------------------
# Support for stack traces
#-------------------------------------------------------------------

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
