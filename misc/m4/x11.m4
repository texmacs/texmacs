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

