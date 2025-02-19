
/******************************************************************************
 * MODULE     : QTMApplication.hpp
 * DESCRIPTION:
 * COPYRIGHT  :
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMAPPLICATION_HPP
#define QTMAPPLICATION_HPP

#include <QApplication>
#include <QIcon>
#include <QStyle>
#include <QStyleFactory>
#include "string.hpp"
#include "sys_utils.hpp"
#include "url.hpp"
#include "boot.hpp"
#include "gui.hpp"
#include "QTMKeyboard.hpp"
#include "QTMPixmapManager.hpp"
#include "QTMMainTabWindow.hpp"

void init_palette (QApplication* app);
void init_style_sheet (QApplication* app);
void set_standard_style_sheet (QWidget *w);

#if defined(Q_OS_MAC) && QT_VERSION < 0x060000 
#include "QTMMacPasteboardMimePDF.hpp"
#endif

class QTMApplication: public QApplication {
  Q_OBJECT

#if defined(Q_OS_MAC) && QT_VERSION < 0x060000 
  QMacPasteboardMimePDF mac_pasteboard_mime_pdf;
#endif
  
public:
  
  QTMApplication (int& argc, char** argv);
  
  void init_theme ();

  void set_window_icon (string icon_path);
  
  virtual bool notify (QObject* receiver, QEvent* event);

#if QT_VERSION >= 0x060000
  QTMPixmapManager& pixmap_manager() {
    static bool first_use= true;
    if (first_use) {
      pm.loadAll();
      first_use= false;
    }
    return pm;
  }
#endif

  inline QTMKeyboard &keyboard() {
    return mKeyboard;
  }

#ifdef TEXMACS_EXPERIMENTAL_TABWINDOW
  inline QTMMainTabWindow &mainTabWindow() {
    return *QTMMainTabWindow::topTabWindow();
  }
#endif

private:
#if QT_VERSION >= 0x060000
  bool mPixmapManagerInitialized;
  QTMPixmapManager pm;
#endif
  QTMKeyboard mKeyboard;
};

inline QTMApplication *tmapp() {
  return dynamic_cast<QTMApplication *>(qApp);
}

class QTMCoreApplication: public QCoreApplication {
  Q_OBJECT
  
public:
  QTMCoreApplication (int& argc, char** argv) :
    QCoreApplication (argc, argv) {}

  void set_window_icon (string icon_path) {
    (void) icon_path;
  }

  virtual bool notify (QObject* receiver, QEvent* event)
  {
    try {
      return QCoreApplication::notify (receiver, event);
    }
    catch (string s) {
      qt_error << "Thrown " << s << LF;
      the_exception= s;
    }
    return false;
  }
};

#endif   // QTMAPPLICATION_HPP
