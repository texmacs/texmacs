
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

AC_DEFUN([LM_QT_JPG],[AC_LANG_PROGRAM([
@%:@include <QtCore>
@%:@include <QtPlugin>
@%:@define qt_static_plugin_qjpeg qt_static_plugin_QJpegPlugin

Q_IMPORT_PLUGIN(qjpeg)
])])

AC_DEFUN([LM_QT_GIF],[AC_LANG_PROGRAM([
@%:@include <QtCore>
@%:@include <QtPlugin>
@%:@define qt_static_plugin_qgif qt_static_plugin_QGifPlugin

Q_IMPORT_PLUGIN(qgif)
])])

AC_DEFUN([LM_QT_ICO],[AC_LANG_PROGRAM([
@%:@include <QtCore>
@%:@include <QtPlugin>
@%:@define qt_static_plugin_qico qt_static_plugin_QICOPlugin

Q_IMPORT_PLUGIN(qico)
])])

AC_DEFUN([LM_QT_SVG],[AC_LANG_PROGRAM([
@%:@include <QtCore>
@%:@include <QtPlugin>
@%:@define qt_static_plugin_qsvg qt_static_plugin_QSvgPlugin

Q_IMPORT_PLUGIN(qsvg)
])])

AC_DEFUN([LC_WITH_QT],[
  typeset xtralibs
  case $CONFIG_OS in
    MINGW) xtralibs="+xml";;
  esac
  case $(qmake -query QT_VERSION 2>/dev/null) in
  5.*)
    AT_WITH_QT([$xtralibs +printsupport +svg],[+exceptions],[
      LIBS += $LDFLAGS
      QTPLUGIN = qjpeg qgif qico qsvg
    ],AC_MSG_ERROR([Cannot find a working Qt library]))
    ;;
  4.*)
    AT_WITH_QT([$xtralibs +printsupport +svg],[+exceptions],[LIBS += $LDFLAGS],AC_MSG_ERROR([Cannot find a working Qt library]))
    ;;
  *) AC_MSG_ERROR([Qt not found or Qt version not supported ]);;
  esac
  # clean and dispatch the collected flags
  LC_COPY_FLAGS([QT],[TMP])
  LC_CLEAR_FLAGS([QT])
  LC_SCATTER_FLAGS([$TMP_CPPFLAGS $TMP_CXXFLAGS $TMP_LDFLAGS $TMP_LIBS $QT_DEFINES],[QT])
  QT_FRAMEWORKS_PATH=`$QMAKE -query QT_INSTALL_LIBS`
  QT_PLUGINS_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
  QT_INSTALL_LIBS=`$QMAKE -query QT_INSTALL_LIBS`
  LC_APPEND_FLAG([-Wl,-rpath,$QT_INSTALL_LIBS],[LDFLAGS])

  QT_VERSION=`$QMAKE -query QT_VERSION`
  QT_MAJOR=${QT_VERSION%%.*}
  test $QT_MAJOR -eq 5 && LC_APPEND_FLAG([-std=c++11],[QT_CXXFLAGS])

  LC_GET_ARG_VALUE([CXXFLAGS], [-mmacosx-version-min], [CXXMACOSX_VERSION_MIN])
  AS_IF([test -n $CXXMACOSX_VERSION_MIN],[
    LC_GET_ARG_VALUE([QT_CXXFLAGS], [-mmacosx-version-min], [QTMACOSX_VERSION_MIN])
    AS_IF([test -n $QTMACOSX_VERSION_MIN],[
      AS_IF([cmp_dot_number $CXXMACOSX_VERSION_MIN $QTMACOSX_VERSION_MIN],[
        m4_foreach(_tmp1,[[QT_CXXFLAGS],[QT_CPPFLAGS],[QT_LDFLAGS]],[
          STRIP_ARG(_tmp1,[-mmacosx-version-min=$QTMACOSX_VERSION_MIN])
        ])
      ],[
        m4_foreach(_tmp1,[[CXXFLAGS], [CFLAGS]], [
          STRIP_ARG(_tmp1,[-mmacosx-version-min=$CXXMACOSX_VERSION_MIN])
          LC_MERGE_FLAGS([-mmacosx-version-min=$QTMACOSX_VERSION_MIN],[CFLAGS])
          LC_MERGE_FLAGS([-mmacosx-version-min=$QTMACOSX_VERSION_MIN],[CXXFLAGS])
        ])
        AC_MSG_WARN([mmacosx-version-min adjusted to $QTMACOSX_VERSION_MIN])
      ])
    ])
  ])
  LC_COMBINE_FLAGS([QT])
  AX_SAVE_FLAGS 
  LC_SET_FLAGS([QT])
  AC_RUN_IFELSE([LM_QT_JPG], [AC_DEFINE([qt_static_plugin_qsvg],[qt_static_plugin_QJpegPlugin],[If there if static plugin lib])],
  	[AC_MSG_WARN([No static jpg plugin])])
  AC_RUN_IFELSE([LM_QT_GIF], [AC_DEFINE([qt_static_plugin_qgif],[qt_static_plugin_QGifPlugin],[If there if static plugin lib])],
  	[AC_MSG_WARN([No static gif plugin])])
  AC_RUN_IFELSE([LM_QT_ICO], [AC_DEFINE([qt_static_plugin_qico],[qt_static_plugin_QICOPlugin],[If there if static plugin lib])],
  	[AC_MSG_WARN([No static icon plugin])])
  AC_RUN_IFELSE([LM_QT_SVG], [AC_DEFINE([qt_static_plugin_qsvg],[qt_static_plugin_QSvgPlugin],[If there if static plugin lib])],
  	[AC_MSG_WARN([No static svg plugin])])
  AX_RESTORE_FLAGS
])
