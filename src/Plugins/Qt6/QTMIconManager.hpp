
/******************************************************************************
* MODULE     : QTMIconManager.hpp
* DESCRIPTION: A Qt6 utility class to manage icons
* COPYRIGHT  : (C) 2024 Liza Belos, 2025 Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_QTMICONMANAGER_HPP
#define TEXMACS_QTMICONMANAGER_HPP

#include <QApplication>

#if QT_VERSION >= 0x060000

#include <QIcon>
#include <QMap>
#include <QString>
#include <QPointer>
#include <QLabel>
#include <QPushButton>
#include <QAction>
#include <QMainWindow>
#include <QTabWidget>
#include <QToolButton>
#include "QTMResponsiveTabWidget.hpp"

#include "url.hpp"
#include "gui.hpp"

class QTMIconManager {

public:
  QTMIconManager () {};
  
  QIcon getIcon (url file_name);

  void setLabelPixmap(QPointer<QLabel> label, url file_name, int width = 0, int height = 0);

  void setPushButtonIcon(QPointer<QPushButton> button, url file_name);

  void setActionIcon(QPointer<QAction> action, url file_name);

  void setWindowIcon(QPointer<QMainWindow> window, url file_name);

  void setTabIcon(QPointer<QTabWidget> tabwidget, int index, url file_name);

  void setTabIcon(QPointer<QTMResponsiveTabWidget> tabbar, int index, url file_name);

  void setToolButtonIcon(QPointer<QToolButton> button, url file_name);

  static inline bool is_dark_mode () {
    return occurs ("dark", tm_style_sheet); }

private:
    QMap<QString, QIcon> engine_cache;

};

#endif // QT_VERSION >= 0x060000
#endif // TEXMACS_QTMICONMANAGER_HPP
