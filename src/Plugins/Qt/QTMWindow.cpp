
/******************************************************************************
* MODULE     : QTMWindow.cpp
* DESCRIPTION: QT Texmacs window class
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMWindow.hpp"

#include <QCloseEvent>

void QTMPlainWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT) cout << "Close QTMPlainWindow" << LF;

  if (tmwid->ref_count != 0) {
    tmwid->send (SLOT_DESTROY, NULL);
      // Tell QT not to close the window, qt_window_widget_rep will.
    event->ignore ();
  }
  emit closed();
}

void QTMWindow::closeEvent (QCloseEvent* event)
{
  if (DEBUG_QT) cout << "Close QTMWindow" << LF;
	
 if (tmwid->ref_count != 0) {
    tmwid->send (SLOT_DESTROY, NULL);
    event->ignore ();
  }
  emit closed();
}
