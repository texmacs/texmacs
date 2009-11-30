
/******************************************************************************
* MODULE     : QTMWindow.hpp
* DESCRIPTION: QT Texmacs window class
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMWINDOW_HPP
#define QTMWINDOW_HPP

#include "qt_other_widgets.hpp"
#include <QVariant>
#include <QMainWindow>
#include <QScrollArea>

class QTMWindow: public QMainWindow {
  Q_OBJECT

public:
  inline QTMWindow(qt_tm_widget_rep *_wid): QMainWindow () {
    setObjectName("A QTMWindow");
    setProperty ("texmacs_tm_widget", QVariant::fromValue ((void*) _wid));
  }

  inline qt_tm_widget_rep *
  tm_widget() {
    QVariant v= property("texmacs_tm_widget");
    return (qt_tm_widget_rep *)
      (v.canConvert<void*> ()? v.value<void*> (): NULL);
  }

protected:
  virtual void closeEvent (QCloseEvent *event);
};


#endif // QTMWINDOW_HPP
