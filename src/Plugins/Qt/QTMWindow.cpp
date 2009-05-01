
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

#include "QTMWindow.hpp"

#include <QEvent>

void QTMWindow::closeEvent ( QCloseEvent *event )
{
  if (DEBUG_EVENTS)   cout << "CLOSE WINDOW" << LF;
  qt_tm_widget_rep *wid = tm_widget ();
  if (wid) {
    wid -> quit ();
  }
  QMainWindow::closeEvent (event);
}

