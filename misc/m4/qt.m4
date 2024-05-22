
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

AC_DEFUN([LM_QT_COCOA],[AC_LANG_PROGRAM([
@%:@include <QtCore>
@%:@include <QtPlugin>

Q_IMPORT_PLUGIN(QCocoaIntegrationPlugin)
])])

AC_DEFUN([LC_WITH_QT],[
  typeset xtralibs
  case $CONFIG_OS in
    MINGW) xtralibs="+xml";;
  esac

  AC_ARG_WITH([qt-find-method],
    [AS_HELP_STRING([--with-qt-find-method=METHOD],
                    [Method to find Qt (autotroll, autotrollstatic, pkgconfig)])],
    [qt_find_method=$withval],
    [qt_find_method=autotroll])
  
  case $qt_find_method in
    autotroll)
      AC_MSG_NOTICE([Searching for Qt using autotroll])
      ;;
    pkgconfig)
      AC_MSG_NOTICE([Searching for Qt using pkg-config])
      ;;
    autotrollstatic)
      AC_MSG_NOTICE([Searching for Qt using autotroll static])
      ;;
    *)
      AC_MSG_ERROR([Unknown method to find Qt: $qt_find_method])
      ;;
  esac

  AC_PATH_PROGS([QMAKE], [qmake qmake-qt4 qmake-qt5 qmake-qt6 qmake6], [missing])
  if test "x$QMAKE" = "xmissing"; then
    AC_MSG_ERROR([Cannot find qmake, qmake-qt4, qmake-qt5, qmake-qt6, or qmake6, for using a Qt library])
  fi
  case $qt_find_method.$($QMAKE -query QT_VERSION 2>/dev/null) in
  autotroll.4.* | autotrollstatic.4.*)
    AC_MSG_NOTICE([Qt4 found])
    AT_WITH_QT([$xtralibs +printsupport +svg],[+exceptions],[LIBS += $LDFLAGS],AC_MSG_ERROR([Cannot find a working Qt library]))
    ;;
  autotroll.5.*)
    AC_MSG_NOTICE([Qt5 found])
    AS_IF([test "x$CONFIG_OS" = "xMACOS"],[xtraPlug=+macextras],[unset xtraPlug])
    AT_WITH_QT([$xtralibs +printsupport +svg $xtraPlug],[+exceptions],[
      LIBS += $LDFLAGS
      QTPLUGIN = qjpeg qgif qico qsvg
    ],AC_MSG_ERROR([Cannot find a working Qt library]))
    ;;
  autotrollstatic.5.*)
    AC_MSG_NOTICE([Qt5 found])
    AS_IF([test "x$CONFIG_OS" = "xMACOS"],[xtraPlug=+macextras],[unset xtraPlug])
    AT_WITH_QT([$xtralibs +core +gui +printsupport +svg $xtraPlug],[+exceptions],[
      LIBS += $LDFLAGS
      QTPLUGIN += qjpeg qgif qico qsvg qxcb
      QTPLUGIN.platforms += qminimal qxcb
      CONFIG += import_plugins
      CONFIG += static
    ],AC_MSG_ERROR([Cannot find a working Qt library]))
    AC_DEFINE([qt_static_plugin_xcb],[1],[If Qt is statically linked])
    AC_DEFINE([qt_no_fontconfig],[1],[Qt without fontconfig])
    ;;
  autotroll.6.* | autotrollstatic.6.*)
    AC_MSG_NOTICE([Qt6 found])
    AS_IF([test $CONFIG_OS == MACOS],[],[unset xtraPlug])
    AT_WITH_QT([$xtralibs +printsupport +svg $xtraPlug],[+exceptions],[
      LIBS += $LDFLAGS
      QTPLUGIN = qjpeg qgif qico qsvg
    ],AC_MSG_ERROR([Cannot find a working Qt library]))
    ;;
  pkgconfig.*) 
    QT_VERSION=`$QMAKE -query QT_VERSION`
    QT_MAJOR=${QT_VERSION%%.*}
    
    # Find Qt.
    AC_ARG_VAR([QT_PATH], [Path to the Qt binaries])
    if test -d /usr/local/Trolltech; then
      # Try to find the latest version.
      tmp_qt_paths=`echo /usr/local/Trolltech/*/bin | tr ' ' '\n' | sort -nr \
                                                | xargs | sed 's/  */:/g'`
    fi

    # Find qmake.
    AC_ARG_VAR([QMAKE], [Qt Makefile generator command])
    AX_PATH_TOOLS([QMAKE], [qmake qmake-qt$QT_MAJOR qmake$QT_MAJOR], [missing],
                  [$QT_DIR:$QT_PATH:$PATH:$tmp_qt_paths])
    if test "x$QMAKE" = xmissing; then
      AX_INSTEAD_IF([$4], [Cannot find qmake. Try --with-qt=PATH.])
      break
    fi

    # Find moc (Meta Object Compiler).
    AC_ARG_VAR([MOC], [Qt Meta Object Compiler command])
    AX_PATH_TOOLS([MOC], [moc moc-qt$QT_MAJOR moc$QT_MAJOR], [missing],
                  [$QT_PATH:$PATH:$tmp_qt_paths])
    if test "x$MOC" = "xmissing"; then
      AX_INSTEAD_IF([$4],
    [Cannot find moc (Meta Object Compiler). Try --with-qt=PATH.])
      break
    fi

    # Find uic (User Interface Compiler).
    AC_ARG_VAR([UIC], [Qt User Interface Compiler command])
    AX_PATH_TOOLS([UIC], [uic uic-qt$QT_MAJOR uic$QT_MAJOR], [missing],
                  [$QT_PATH:$PATH:$tmp_qt_paths])
    if test "x$UIC" = "xmissing"; then
      AX_INSTEAD_IF([$4],
  [Cannot find uic (User Interface Compiler). Try --with-qt=PATH.])
      break
    fi

    # Find rcc (Qt Resource Compiler).
    AC_ARG_VAR([RCC], [Qt Resource Compiler command])
    AX_PATH_TOOLS([RCC], [rcc rcc-qt$QT_MAJOR rcc$QT_MAJOR], [missing],
                  [$QT_PATH:$PATH:$tmp_qt_paths])
    if test "x$RCC" = "xmissing"; then
      AC_MSG_WARN([Cannot find rcc (Qt Resource Compiler). Try --with-qt=PATH.])
    fi

    AC_MSG_CHECKING([whether host operating system is Darwin])
    at_darwin=no
    at_qmake_args=
    case $host_os in
      darwin*)
        at_darwin=yes
        ;;
    esac
    AC_MSG_RESULT([$at_darwin])

    # If we don't know the path to Qt, guess it from the path to qmake.
    if test "x$QT_PATH" = "x"; then
      QT_PATH=`dirname "$QMAKE"`
    fi
    if test "x$QT_PATH" = "x"; then
      AX_INSTEAD_IF([$4],
                    [Cannot find your Qt installation. Try --with-qt=PATH.])
      break
    fi
    AC_SUBST([QT_PATH])


    PKG_CONFIG=`which pkg-config`
    AS_IF([test -z "$PKG_CONFIG"],[AC_MSG_ERROR([Cannot find pkg-config])])

    QT_PKGCONFIG_SUFFIX=''
    # if ABI or ANDROID_ABI is set, use it as suffix
    AS_IF([test -n "$ABI"],[QT_PKGCONFIG_SUFFIX="_${ABI}"])
    AS_IF([test -n "$ANDROID_ABI"],[QT_PKGCONFIG_SUFFIX="_${ANDROID_ABI}"])

    QT_PACKAGES="Qt${QT_MAJOR}Core$QT_PKGCONFIG_SUFFIX Qt${QT_MAJOR}Gui$QT_PKGCONFIG_SUFFIX Qt${QT_MAJOR}Widgets$QT_PKGCONFIG_SUFFIX Qt${QT_MAJOR}Svg$QT_PKGCONFIG_SUFFIX Qt${QT_MAJOR}PrintSupport$QT_PKGCONFIG_SUFFIX"

    AS_IF([test "x$CONFIG_OS" = "xMACOS"],[QT_PACKAGES="$QT_PACKAGES Qt${QT_MAJOR}MacExtras$QT_PKGCONFIG_SUFFIX"])

    # Set QT_DEFINES, QT_CXXFLAGS, QT_INCPATH, QT_LIBS, QT_LDFLAGS and QT_VERSION
    QT_DEFINES=`$PKG_CONFIG --cflags-only-I $QT_PACKAGES`
    AS_IF([test -z "$QT_DEFINES"],[AC_MSG_ERROR([Cannot find a working Qt library])])

    QT_CXXFLAGS=`$PKG_CONFIG --cflags-only-other $QT_PACKAGES`
    QT_INCPATH=`$PKG_CONFIG --variable=includedir $QT_PACKAGES`
    QT_LIBS=`$PKG_CONFIG --libs-only-l $QT_PACKAGES`
    QT_LDFLAGS=`$PKG_CONFIG --libs-only-L $QT_PACKAGES`

    QT_VERSION=`$PKG_CONFIG --modversion $QT_PACKAGES`
    AS_IF([test -z "$QT_VERSION"],[AC_MSG_ERROR([Cannot find a working Qt library])])
    ;;
  esac
  # clean and dispatch the collected flags

  case $qt_find_method in
    autotroll)
    LC_COPY_FLAGS([QT],[TMP])
    LC_CLEAR_FLAGS([QT])
    LC_SCATTER_FLAGS([$TMP_CPPFLAGS $TMP_CXXFLAGS $TMP_LDFLAGS $TMP_LIBS $QT_DEFINES],[QT])
      ;;
    pkgconfig)
    LC_COPY_FLAGS([QT],[TMP])
    LC_CLEAR_FLAGS([QT])
    LC_SCATTER_FLAGS([$TMP_CPPFLAGS $TMP_CXXFLAGS $TMP_LDFLAGS $TMP_LIBS $QT_DEFINES],[QT])
      ;;
    autotrollstatic)
    ;;
  esac

  QT_FRAMEWORKS_PATH=`$QMAKE -query QT_INSTALL_LIBS`
  QT_PLUGINS_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
  QT_INSTALL_LIBS=`$QMAKE -query QT_INSTALL_LIBS`
  LC_APPEND_FLAG([-Wl,-rpath,$QT_INSTALL_LIBS],[LDFLAGS])

  QT_VERSION=`$QMAKE -query QT_VERSION`
  QT_MAJOR=${QT_VERSION%%.*}
  test "0$QT_MAJOR" -eq 5 && LC_APPEND_FLAG([-std=c++11],[QT_CXXFLAGS])
  test "0$QT_MAJOR" -eq 6 && LC_APPEND_FLAG([-std=c++17],[QT_CXXFLAGS])

  LC_GET_ARG_VALUE([CXXFLAGS], [-mmacosx-version-min], [CXXMACOSX_VERSION_MIN])
  AS_IF([test -n "$CXXMACOSX_VERSION_MIN"],[
    LC_GET_ARG_VALUE([QT_CXXFLAGS], [-mmacosx-version-min], [QTMACOSX_VERSION_MIN])
    AS_IF([test -n "$QTMACOSX_VERSION_MIN"],[
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

  case $qt_find_method in
    autotroll)
    AC_RUN_IFELSE([LM_QT_JPG], [AC_DEFINE([qt_static_plugin_qjpeg],[qt_static_plugin_QJpegPlugin],[If there is a static plugin qjpeg])],
    [AC_MSG_WARN([No static qjpeg plugin])])
  AC_RUN_IFELSE([LM_QT_GIF], [AC_DEFINE([qt_static_plugin_qgif],[qt_static_plugin_QGifPlugin],[If there is a static plugin qgif])],
    [AC_MSG_WARN([No static qgif plugin])])
  AC_RUN_IFELSE([LM_QT_ICO], [AC_DEFINE([qt_static_plugin_qico],[qt_static_plugin_QICOPlugin],[If there is a static plugin qico])],
    [AC_MSG_WARN([No static qico plugin])])
  AC_RUN_IFELSE([LM_QT_SVG], [AC_DEFINE([qt_static_plugin_qsvg],[qt_static_plugin_QSvgPlugin],[If there is a static plugin qsvg])],
    [AC_MSG_WARN([No static qsvg plugin])])
  AC_RUN_IFELSE([LM_QT_COCOA], [AC_DEFINE([CocoaPlugin],[1],[If there is a static plugin Cocoa])],
    [AC_MSG_WARN([No static Cocoa plugin])])
      ;;
    pkgconfig)
      ;;
  esac
  AX_RESTORE_FLAGS
])
