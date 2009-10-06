
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
  if (DEBUG_EVENTS)   cout << "CLOSE WINDOW" << LF;
  qt_tm_widget_rep *wid = tm_widget ();
  if (wid) {
    wid -> quit ();
    needs_update ();
    event -> ignore ();
  }
 // QMainWindow::closeEvent (event);
}

void QTMScrollArea::resizeEvent( QResizeEvent* event )
{
  if (DEBUG_EVENTS) cout << "RESIZE SCROLLAREA" << LF;
  //FIXME: the following is an hack needed to trigger widget resize at the 
  //       texmacs level when the scrollarea is resized. needed because the 
  //       texmacs canvas minimal geometry in papyrus mode depends on the 
  //       scrollarea height. 

  if (tm_widget()->tm_canvas()) 
  {
    
    SI x1, y1, x2, y2;
    get_extents (tm_widget(), x1, y1, x2, y2);
    set_extents (tm_widget(), x1, y1, x2, y2);
  }

  QScrollArea::resizeEvent (event);
}
