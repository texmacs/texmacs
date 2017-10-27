
#--------------------------------------------------------------------
#
# MODULE      : tm_gui.m4
# DESCRIPTION : GUI selection
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_GUI],[

  CONFIG_X11=""
  CONFIG_COCOA=""
  CONFIG_GUI="X11"

  AC_ARG_ENABLE(qt,
  [  --disable-qt            replace Qt by X11 interface],
      [], [enable_qt="yes"])

  case "$enable_qt" in
      yes)
         LC_WITH_QT
         if test x"$at_cv_qt_build" = xko; then 
            AC_MSG_ERROR([cannot find Qt!])
         else
            AC_MSG_RESULT([enabling Qt port])
            CONFIG_GUI="QT"
            if test x"$CONFIG_OS" = xMACOS; then
               # on Mac we rely on some ObjC code contained in 
               # src/Plugins/MacOS    
               CONFIG_MACOS="MacOS"
            fi
         fi
         ;;
      no)
         CONFIG_QTPIPES="no"
         AC_MSG_RESULT([enabling X11 port])
         LC_X_HEADERS
         AC_PATH_X
         AC_PATH_XTRA
         ;;
      *)
         CONFIG_QTPIPES="no"
         AC_MSG_ERROR([bad option --enable-qt=$enable_qt])
         ;;
  esac

  # Qt Plugins list
  if test "$QT5_AVAILABLE" = yes; then
    QT_PLUGINS_LIST="imageformats"
  else
    QT_PLUGINS_LIST="accessible,imageformats"
  fi

  # Qt Pipes
  AC_ARG_ENABLE(qtpipes,
  [  --enable-qtpipes        replace UNIX pipes by Qt pipes],
      [], [enable_qtpipes=$CONFIG_QTPIPES])

  case "$enable_qtpipes" in
      yes)
         if test x"$CONFIG_GUI" = xQT; then
            AC_DEFINE(QTPIPES, 1, [Enabling Qt pipes])
            AC_MSG_RESULT([enabling Qt pipes])
         else
            AC_MSG_ERROR([QT not enabled!])
         fi
         ;;
      no)
         if test x"$CONFIG_GUI" = xQT; then
            AC_MSG_RESULT([disabling Qt pipes])
         fi
         ;;
      *)
         AC_MSG_ERROR([bad option --enable-qtpipes=$enable_qtpipes])
         ;;
  esac

  AC_ARG_ENABLE(cocoa,
  [  --enable-cocoa          replace X11 by Cocoa interface],
      [], [enable_cocoa="no"])
  case "$enable_cocoa" in
      yes)
         AC_MSG_RESULT([enabling experimental Cocoa port])
         COCOA_CFLAGS=""
         COCOA_LDFLAGS="-framework Cocoa"
         CONFIG_GUI="COCOA"
         ;;
      no)
         AC_MSG_RESULT([disabling experimental Cocoa port])
         ;;
      *)
         AC_MSG_ERROR([bad option --enable-cocoa=$enable_cocoa])
         ;;
  esac

  case "$CONFIG_GUI" in
      X11)
         CONFIG_X11="X11 Widkit"
         if test "x${CONFIG_GS}" != "xGhostscript"; then
           CONFIG_X11="$CONFIG_X11 Ghostscript"
         fi
         CONFIG_GUI_DEFINE="X11TEXMACS"
          AC_DEFINE(X11TEXMACS, 1, [Use standard X11 port])
         ;;
      COCOA)
         CONFIG_COCOA="Cocoa"
         CONFIG_GUI_DEFINE="AQUATEXMACS"
          AC_DEFINE(AQUATEXMACS, 1, [Enable experimental Cocoa port])
         ;;
      QT)
         CONFIG_QT="Qt"
         CONFIG_GUI_DEFINE="QTTEXMACS"
          AC_DEFINE(QTTEXMACS, 1, [Enable experimental Qt port])
         ;;
  esac

  AC_SUBST(COCOA_CFLAGS)
  AC_SUBST(COCOA_LDFLAGS)

  AC_SUBST(CONFIG_X11)
  AC_SUBST(CONFIG_COCOA)
  AC_SUBST(CONFIG_QT)
  AC_SUBST(CONFIG_GUI)
  AC_SUBST(CONFIG_GUI_DEFINE)

  AC_SUBST(QT_FRAMEWORKS_PATH)  
  AC_SUBST(QT_PLUGINS_PATH)
  AC_SUBST(QT_PLUGINS_LIST)
])
