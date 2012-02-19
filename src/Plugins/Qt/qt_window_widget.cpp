
/******************************************************************************
 * MODULE     : qt_window_widget.hpp
 * DESCRIPTION: QT window widget.
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_window_widget.hpp"
#include "qt_utilities.hpp"
#include "QTMWindow.hpp"

#include "message.hpp"
#include "analyze.hpp"

#include <QWidget>
#include <QVariant>

qt_window_widget_rep::qt_window_widget_rep (QWidget* _wid):
widget_rep(), wid(_wid)
{
  wid->setProperty ("texmacs_window_widget",
                    QVariant::fromValue ((void*) this));
  nr_windows++;
}

qt_window_widget_rep::~qt_window_widget_rep ()
{
  nr_windows--;
}

void
qt_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SIZE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      if (wid) {
        QSize size= to_qsize (p);
        wid->resize (size);
      }
    }
      break;
      
    case SLOT_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      if (wid) {
        QPoint pt = to_qpoint (p);
        pt.ry() += 40;
          // to avoid window under menu bar on MAC when moving at (0,0)
        if (DEBUG_QT) 
          cout << "Moving to (" << pt.x() << "," 
          << pt.y() << ")" << LF;
        wid->move (pt);
      }
    }
      break;
      
    case SLOT_VISIBILITY:
    {
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (wid) {
        if (flag) {
          wid->show();
          // wid->activateWindow();
          //WEIRD: in Ubuntu uncommenting the above line causes the main window 
          //to be opened in the background.
          wid->raise();
        }
        else wid->hide();
      }
    }
      break;
      
    case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      if (wid) wid->setWindowTitle (to_qstring (tm_var_encode(name)));
    }
      break;
      
    case SLOT_FULL_SCREEN:
    {
      check_type<bool> (val, "SLOT_FULL_SCREEN");
      QTMWindow *win = qobject_cast<QTMWindow*>(wid);
      if (win && win->tm_widget()) {
        win->tm_widget()->set_full_screen(open_box<bool> (val));
      }
        //win->set_full_screen (open_box<bool> (val));
    }
      break;
      
    case SLOT_UPDATE:
      NOT_IMPLEMENTED ;
        //send_update (THIS, val);
      break;
    case SLOT_REFRESH:
      NOT_IMPLEMENTED ;
        //send_refresh (THIS, val);
      break;
      
    default:
      FAILED ("cannot handle slot type");
  }
}

blackbox
qt_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
      TYPE_CHECK (type_id == type_helper<int>::id);
        // we need only to know if the widget is attached to some gui window
      return close_box<int> (wid? 1: 0);
        // return close_box<int> ((int)wid);
    case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= wid->pos();
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
    case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QSize s= wid->size();
      return close_box<coord2> (from_qsize (s));
    }
    default:
      FAILED ("cannot handle slot type");
      return blackbox ();
  }
}

/******************************************************************************
 * Notification of state changes
 ******************************************************************************/

void
qt_window_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::notify " << slot_name(s) << LF;
  widget_rep::notify (s, new_val);
}

widget
qt_window_widget_rep::read (slot s, blackbox index) {
  (void) index;
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
    default:
      FAILED ("cannot handle slot type");
      return widget();
  }
}

void
qt_window_widget_rep::write (slot s, blackbox index, widget w) {
  (void) w; (void) index;
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::write " << slot_name(s) << LF;
  
  switch (s) {
    default:
      FAILED ("cannot handle slot type");
  }
}

