
/******************************************************************************
* MODULE     : QTMWindow.cpp
* DESCRIPTION: QT Texmacs window class
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtGui>

#include "gui.hpp"
#include "QTMWindow.hpp"
#include "QTMWidget.hpp"
#include "message.hpp"

#include <QEvent>

void QTMWindow::closeEvent ( QCloseEvent *event )
{
  if (DEBUG_QT)   cout << "CLOSE WINDOW" << LF;
  qt_tm_widget_rep *wid = tm_widget ();
  if (wid) {
    wid -> quit ();
    needs_update ();
    event -> ignore ();
  }
 // QMainWindow::closeEvent (event);
}

void QTMPlainWindow::closeEvent ( QCloseEvent *event )
{
  if (DEBUG_QT)   cout << "CLOSE PLAIN WINDOW" << LF;
  (void) event;
  emit closed();
}
