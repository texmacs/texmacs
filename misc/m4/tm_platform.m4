
#--------------------------------------------------------------------
#
# MODULE      : tm_platform.m4
# DESCRIPTION : Determine various platform dependent settings
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_PLATFORM],[
  CONFIG_OS_SUFFIX="${host}"
  CONFIG_OS_COMPAT="Unix"
  CONFIG_MACOS=""
  CONFIG_CXXFLAGS=""
  CONFIG_CXXTEMPLATE=""
  CONFIG_STD_SETENV="#define STD_SETENV"
  CONFIG_SO="so"
  CONFIG_LIB_PATH="LD_LIBRARY_PATH"
  CONFIG_CHMOD="chmod -f"
  CONFIG_CXXOPTIMIZE="-O2"
  CONFIG_BSTATIC="-lXdmcp -lXau -lXrender"
  CONFIG_BSHARED="-Wl,-Bdynamic"
  CONFIG_BFLAGS=""
  CONFIG_BPATH="-Wl,-rpath,"
  CONFIG_HOST_OS="$host_os"
  CONFIG_HOST_VENDOR="$host_vendor"
  CONFIG_HOST_CPU="$host_cpu"
  CONFIG_USER="$USER"
  CONFIG_DATE="`date`"
  CONFIG_QTPIPES="no"
  type rsync && CONFIG_CP="rsync -a --exclude='.*'" || CONFIG_CP="cp -f -R -p"
  # tweak for XCode project
  CONFIG_ARCHS='$(NATIVE_ARCH)'

  X11_CFLAGS="$X_CFLAGS"
  X11_LDFLAGS="$X_LIBS -lXext -lX11"

  AX_SAVE_FLAGS
  LC_CLEAR_FLAGS
  AC_CHECK_SIZEOF(void *)
  AC_CHECK_ALIGNOF(void *)
  if [[[ $ac_cv_sizeof_void_p -eq 0 || $ac_cv_alignof_void_p -eq 0 ]]]
  then AC_MSG_ERROR([Cannot determine the machine size])
  fi
  AC_DEFINE_UNQUOTED([WORD_LENGTH],[$ac_cv_sizeof_void_p],[Pointer  size])
  AC_DEFINE_UNQUOTED([WORD_LENGTH_INC],[$(($ac_cv_sizeof_void_p - 1))],[Pointer increment])

  AC_MSG_NOTICE([Sizeof integer: $ac_cv_sizeof_void_p])
  AC_DEFINE_UNQUOTED([WORD_MASK],[$( printf 0x%x $(( (~($ac_cv_alignof_void_p - 1)) & ((256 ** ac_cv_sizeof_void_p) -1) )))],[Word Mask])
  AC_DEFINE([MAX_FAST],[264] ,[Max fast alloc // WORD_LENGTH more than power of 2])
  AX_RESTORE_FLAGS

  AC_DEFUN([LINUX_COMMON],[
      AC_MSG_RESULT(an Intel or AMD GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      AC_SUBST([CONFIG_PACKAGE],[GENERIC_PACKAGE])
      CONFIG_OS="GNU_LINUX"
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
      CONFIG_QTPIPES="yes"
      AC_DEFINE([STACK_SIZE], 0x1000000, [If not set during link])
      AC_CHECK_LIB(expat,XML_ParserCreate,[CONFIG_BSTATIC="-lexpat $CONFIG_BSTATIC";CONFIG_STYPE=A])
      AC_CHECK_LIB(xcb,xcb_disconnect,[CONFIG_BSTATIC="-lxcb $CONFIG_BSTATIC";CONFIG_STYPE=B])
  ])

  AC_MSG_CHECKING(final adjustments for)
  case "${host}" in
    x86_64-*-linux*)
      CONFIG_OS_SUFFIX="x86_64-pc-linux-gnu"
      LINUX_COMMON
    ;;
    i*86-*-linux*)
      CONFIG_OS_SUFFIX="i386-pc-linux-gnu"
      LINUX_COMMON
    ;;
    i*86-*-freebsd* | x86_64-*-freebsd*)
      AC_MSG_RESULT(an Intel or AMD GNU/BSD host)
      AC_DEFINE([OS_FREEBSD],[1],[OS type])
      CONFIG_OS="FREEBSD"
      CONFIG_QTPIPES="yes"
      CPPFLAGS="$CPPFLAGS -I/usr/local/include -I."
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
    ;;
    i*86-*-solaris*)
      AC_MSG_RESULT(an Intel or AMS Solaris host)
      AC_DEFINE([OS_SOLARIS],[1],[OS type])
      CONFIG_OS="SOLARIS"
      CONFIG_CXXOPTIMIZE="-O3"
      CONFIG_BPATH="-Wl,-R,"
      X11_LDFLAGS="$X_LIBS -lXext -lX11 -lsocket"
    ;;
    *mingw*)
      AC_MSG_RESULT([for mingw host])
      AC_DEFINE([OS_MINGW],[(defined (__MINGW__) || defined (__MINGW32__))],[OS type])
      AC_SUBST([CONFIG_BUNDLE],[WINDOWS_BUNDLE])
      AC_SUBST([CONFIG_PACKAGE],[WINDOWS_PACKAGE])
      CONFIG_OS=MINGW
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
      CONFIG_QTPIPES="yes"
      CONFIG_OS_COMPAT="Windows"
      CPPFLAGS="$CPPFLAGS -I/usr/local/include -IPlugins/Windows -I."
      GUILE_LDFLAGS="-lmingwex $GUILE_LDFLAGS -lintl" #added mingwex to mask the internal guile readdir function
      LC_APPEND_FLAG([-Wl,--stack=16777216],[LDFLAGS])
    ;;
    *-*-cygwin)
      AC_MSG_RESULT(cygwin host)
      AC_DEFINE([OS_CYGWIN],[1],[OS type])
      CONFIG_OS="CYGWIN"
      CONFIG_QTPIPES="yes"
      CONFIG_BFLAGS="-Wl,-stack,8388608"
    ;;
    *apple*darwin*)
      echo "$ac_t""for a MacOS host" 1>&6
      AC_DEFINE([OS_MACOS],[1],[OS type])
      AC_SUBST([CONFIG_BUNDLE],[MACOS_BUNDLE])
      AC_SUBST([CONFIG_PACKAGE],[MACOS_PACKAGE])
      CONFIG_OS="MACOS"
      CONFIG_QTPIPES="yes"
      CONFIG_CXXFLAGS="-I${prefix}/include"
      CONFIG_CXXOPTIMIZE="-O2 -mfpmath=sse -msse2"
      CONFIG_BSHARED=""
      CONFIG_BFLAGS="-framework Cocoa -framework IOKit"
      CONFIG_BPATH=""
      CONFIG_SO="dylib"
      CONFIG_LIB_PATH="DYLD_LIBRARY_PATH"
      LC_APPEND_FLAG([-Wl,-stack_size,0x1000000,-headerpad_max_install_names],[LDFLAGS])
      test -z "$with_tmrepo" -a -d /sw/include -a -d /sw/lib &&
        LC_SCATTER_FLAGS([-I/sw/include -L/sw/lib])
      test -z "$with_tmrepo" -a -d /opt/local/include -a -d /opt/local/lib &&
        LC_SCATTER_FLAGS([-I/opt/local/include -L/opt/local/lib])
    ;;
    *darwin*)
      echo "$ac_t""for a generic Darwin host" 1>&6
      AC_DEFINE([OS_DARWIN],[1],[OS type])
      CONFIG_OS="DARWIN"
      CONFIG_CXXFLAGS="-I${prefix}/include"
      CONFIG_BSHARED=""
      CONFIG_BPATH=""
      CONFIG_SO="dylib"
      CONFIG_LIB_PATH="DYLD_LIBRARY_PATH"
    ;;
    *haiku*)
      AC_MSG_RESULT(a generic Haiku host)
      AC_DEFINE([OS_HAIKU],[1],[OS type])
      CONFIG_OS="HAIKU"
      CONFIG_QTPIPES="yes"
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
    ;;
    powerpc-*-linux*)
      AC_MSG_RESULT(a PowerPC/GNU-linux host)
      AC_DEFINE([OS_POWERPC_GNU_LINUX],[1],[OS type])
      CONFIG_OS="POWERPC_GNU_LINUX"
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
    ;;
    *sun*)
      AC_MSG_RESULT(a SUN/solaris host)
      AC_DEFINE([OS_SUN],[1],[OS type])
      CONFIG_OS="SUN"
      CONFIG_BSTATIC=""
      CONFIG_BSHARED=""
      CONFIG_BPATH="-Wl,-R,"
      X11_LDFLAGS="$X_LIBS -lXext -lX11 -lsocket"
      AC_DEFINE_UNQUOTED([WORD_LENGTH],[8],[Pointer  size])
      AC_DEFINE_UNQUOTED([WORD_LENGTH_INC],[7],[Pointer increment])
      AC_DEFINE_UNQUOTED([WORD_MASK],[$( printf 0x%x 0xfffffff8)],[Word Mask])
      CONFIG_MAX_FAST="264 // WORD_LENGTH more than power of 2"
      CONFIG_STD_SETENV=""
    ;;
    sparc*-*-linux*)
      AC_MSG_RESULT(a Sparc host running GNU/Linux)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_CXXOPTIMIZE="-O3 -fexpensive-optimizations"
      AC_DEFINE_UNQUOTED([WORD_LENGTH],[8],[Pointer  size])
      AC_DEFINE_UNQUOTED([WORD_LENGTH_INC],[7],[Pointer increment])
      AC_DEFINE_UNQUOTED([WORD_MASK],[$( printf 0x%x 0xfffffff8)],[Word Mask])
      CONFIG_MAX_FAST="264 // WORD_LENGTH more than power of 2"
      CONFIG_STD_SETENV=""
    ;;
    *dec*)
      AC_MSG_RESULT(a DEC/alpha host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_BSTATIC=""
      CONFIG_BSHARED="-shared"
      CONFIG_STD_SETENV=""
    ;;
    *alpha*-*-linux*)
      AC_MSG_RESULT(an Alpha GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_BSTATIC=""
      CONFIG_BSHARED="-shared"
    ;;
    s390-*-linux*)
      AC_MSG_RESULT(an IBM S/390 GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_STD_SETENV=""
    ;;
    ia64-*-linux*)
      AC_MSG_RESULT(an Itanium GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_BSTATIC=""
      CONFIG_BSHARED="-shared"
    ;;
    hppa*-*-linux*)
      AC_MSG_RESULT(an HP PA_RISC GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
      CONFIG_CXXFLAGS="$CONFIG_CXXFLAGS -fPIC"
      CONFIG_BSTATIC=""
      CONFIG_BSHARED="-shared"
    ;;
    *sgi-irix*)
      echo "$ac_t""for a SGI/Irix host" 1>&6
      AC_DEFINE([OS_IRIX],[1],[OS type])
      CONFIG_OS="IRIX"
      CONFIG_CXXFLAGS=""
      X_LIBS=-L/usr/lib32
      CONFIG_BSTATIC=""
      CONFIG_BSHARED=""
      CONFIG_BPATH=""
      X11_LDFLAGS="$X_LIBS -lX11"
      AC_DEFINE_UNQUOTED([WORD_MASK],[$( printf 0x%x 0xfffffff8)],[Word Mask])
      CONFIG_STD_SETENV=""
      CONFIG_CHMOD="chmod"
      CONFIG_LIB_PATH="LD_LIBRARYN32_PATH"
    ;;
    m68k-*-linux* | mips-*-linux* | mipsel-*-linux* | arm*-*-linux*)
      AC_MSG_RESULT(a supported GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
    ;;
    *-linux*)
      AC_MSG_RESULT(a generic GNU/Linux host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
    ;;
    *)
      AC_MSG_RESULT(a generic host)
      AC_DEFINE([OS_GNU_LINUX],[1],[OS type])
      CONFIG_OS="GNU_LINUX"
    ;;
  esac

  case "${host}" in
    *apple*darwin10*)
      echo "$ac_t""Xcode 3.2 project tweak (only MacOSX 10.6)" 1>&6
      CONFIG_ARCHS='$(NATIVE_ARCH_ACTUAL)'
    ;;
    arm*apple*darwin*)
      echo "$ac_t""Adjust optimization flag for Apple M1" 1>&6
      CONFIG_CXXOPTIMIZE="-O2"
    ;;
  esac

  case "$GXX_VERSION" in
    3.* | 4.*)
#     CONFIG_BSTATIC="-static"
      CONFIG_BSHARED="-dynamic"
    ;;
  esac


  AC_SUBST(CONFIG_OS)
  AC_SUBST(CONFIG_OS_SUFFIX)
  AC_SUBST(CONFIG_OS_COMPAT)
  AC_SUBST(CONFIG_MACOS)
  AC_SUBST(CONFIG_CXXWARNING)
  AC_SUBST(CONFIG_CXXTEMPLATE)
  AC_SUBST(CONFIG_CXXOPTIMIZE)
  AC_SUBST(CONFIG_CXXDEBUG)
  AC_SUBST(CONFIG_CXXPROFILE)
  AC_SUBST(CONFIG_BFLAGS)
  AC_SUBST(CONFIG_BSTATIC)
  AC_SUBST(CONFIG_STYPE)
  AC_SUBST(CONFIG_BSHARED)
  AC_SUBST(CONFIG_BPATH)
  AC_SUBST(CONFIG_CXXFLAGS)
  AC_SUBST(CONFIG_STD_SETENV)
  AC_SUBST(CONFIG_SO)
  AC_SUBST(CONFIG_LIB_PATH)
  AC_SUBST(CONFIG_STRIP)
  AC_SUBST(CONFIG_CHMOD)
  AC_SUBST(CONFIG_LDRT)
  AC_SUBST(CONFIG_HOST_OS)
  AC_SUBST(CONFIG_HOST_VENDOR)
  AC_SUBST(CONFIG_USER)
  AC_SUBST(CONFIG_DATE)
  AC_SUBST(CONFIG_ARCHS)
  AC_SUBST(CONFIG_CP)
])
