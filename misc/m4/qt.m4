
#--------------------------------------------------------------------
#
# MODULE      : qt.m4
# DESCRIPTION : TeXmacs configuration options for Qt library
# COPYRIGHT   : (C) 2016 Joris van der Hoeven, Denis RAUX
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

m4_include([misc/autotroll/autotroll.m4])
m4_include([misc/m4/qt5.m4])

AC_DEFUN([LC_WITH_QT],[
#QT has the install dir hard coded in library so we need to fix it manually for relocatable environment
QT5_AVAILABLE="no"
SAVE_LIBS="$LIBS"
LIBS=""
#     AX_HAVE_QT
LIBS="$SAVE_LIBS"
if test x"$have_qt" = xyes; then 
  AC_MSG_RESULT([Qt5 found])
  at_cv_qt_build=ok
  QMAKE="qmake"
  MOC="moc"
  QT5_AVAILABLE="yes"
else
LC_DUMP_FLAGS
# AC_MSG_RESULT([Qt5 seems not present, trying Qt4...])
  AT_WITH_QT([],[+exceptions],[LIBS += $LDFLAGS])
fi
# clean and dispatch the collected flags
LC_COPY_FLAGS([QT],[TMP])
LC_CLEAR_FLAGS([QT])
LC_SCATTER_FLAGS([$TMP_CPPFLAGS $TMP_CXXFLAGS $TMP_LDFLAGS $TMP_LIBS $QT_DEFINES],[QT])
QT_FRAMEWORKS_PATH=`$QMAKE -query QT_INSTALL_LIBS`
QT_PLUGINS_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
LC_GET_ARG_VALUE([QT_CXXFLAGS], [-mmacosx-version-min], [MACOSX_VERSION_MIN])
if [[[ -n $MACOSX_VERSION_MIN ]]]
then m4_foreach(_tmp1,[[QT_CXXFLAGS], [QT_CPPFLAGS], [QT_LDFLAGS]], [STRIP_ARG(_tmp1,[-mmacosx-version-min])])
fi
LC_COMBINE_FLAGS([QT])
])
