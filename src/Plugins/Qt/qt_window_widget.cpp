
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
#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_gui.hpp"
#include "QTMWindow.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMMenuHelper.hpp"

#include "message.hpp"
#include "analyze.hpp"

#include <QWidget>
#include <QVariant>

qt_window_widget_rep::qt_window_widget_rep (QWidget* _wid, command _quit)
: qt_widget_rep(window_widget, _wid), quit(_quit)
{
  qwid->setProperty ("texmacs_window_widget",
                     QVariant::fromValue ((void*) this));
  
  if (qwid->metaObject() -> 
      indexOfSignal (QMetaObject::normalizedSignature ("closed()").constData ()) != -1) {
    QTMCommand* qtmcmd = new QTMCommand(qwid, quit);
    QObject::connect(qwid, SIGNAL (closed()), qtmcmd, SLOT (apply()));
  }

  nr_windows++;
}

/*!
 WARNING! This should be the only place were QWidgets are destroyed!
 */
qt_window_widget_rep::~qt_window_widget_rep ()
{
  nr_windows--;

  delete qwid;
}

widget
qt_window_widget_rep::popup_window_widget(string s)
{
  qwid->setWindowTitle(to_qstring(s));
  qwid->setWindowModality(Qt::NonModal);
  qwid->setWindowFlags(Qt::Popup);
  return this;
}

/*! Looks among the widget's parents for the containing texmacs window
 */
widget_rep* 
qt_window_widget_rep::widget_from_qwidget(QWidget* qwid)
{
  while (qwid != NULL) {
    QVariant v = qwid->property ("texmacs_window_widget");
    if (v.canConvert<void*> ())
      return static_cast<widget_rep*> (v.value<void*> ());
    else
      qwid = qwid->parentWidget();
  }
  FAILED ("attempt to retrieve the window of a QWidget without one");
}

void
qt_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 p= open_box<coord2> (val);
      if (qwid) {
        QSize size= to_qsize (p);
        qwid->resize (size);
      }
    }
      break;
      
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      coord2 p= open_box<coord2> (val);
      if (qwid) {
        QPoint pt = to_qpoint (p);
#ifdef OS_MACOS
        pt.ry() = (pt.y() <= 40) ? 40 : pt.y();
          // to avoid window under menu bar on MAC when moving at (0,0)
          // FIXME: use the real menu bar height.
#endif
        qwid->move (pt);
      }
    }
      break;
      
    case SLOT_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      if (qwid) {
        if (flag) {
          qwid->show();
          // wid->activateWindow();
          //WEIRD: in Ubuntu uncommenting the above line causes the main window 
          //to be opened in the background.
          qwid->raise();
        }
        else qwid->hide();
      }
    }
      break;

    case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab
      if (flag && qwid) {
        qwid->setWindowFlags(Qt::Window);  // ok?
        qwid->setWindowModality(Qt::WindowModal); //ok?
        qwid->show();
      }
    }   
      break;
    case SLOT_NAME:
    {   
      check_type<string> (val, s);
      string name = open_box<string> (val);
      if (qwid) qwid->setWindowTitle (to_qstring (tm_var_encode(name)));
    }
      break;
      
    case SLOT_FULL_SCREEN:
    {
      check_type<bool> (val, s);
      QTMWindow* qwin = qobject_cast<QTMWindow*>(qwid);
      if (qwin && qwin->tmwid->ref_count != 0) {
        qt_tm_widget_rep* wid = static_cast<qt_tm_widget_rep*>(qwin->tmwid.rep);
        if (wid)
          wid->set_full_screen(open_box<bool> (val));
      }
      else FAILED ("attempt to set full screen on a non qt_tm_widget");
    }
      break;
      
    case SLOT_REFRESH:
      the_gui->gui_helper->emitTmSlotRefresh();
      break;
      
    default:
      qt_widget_rep::send(s, val);
  }
}

blackbox
qt_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
      check_type_id<int> (type_id, s);
        // we need only know if the widget has some QT window attached
      return close_box<int> (qwid? 1: 0);
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      QPoint pt;
        // FIXME: dock widgets are embedded into qt_window_widget_reps as a temporary hack
        // because of this the underlying widget is not always a top level window
      if (qwid->isWindow()) pt = qwid->pos();
      else                  pt = qwid->window()->pos();
        //cout << "wpos: " << pt.x() << ", " << pt.y() << LF;
      return close_box<coord2> (from_qpoint (pt));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      QSize s= qwid->size();
      return close_box<coord2> (from_qsize (s));
    }
    default:
      return qt_widget_rep::query (s, type_id);
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


/******************************************************************************
 * popup widget
 ******************************************************************************/


qt_popup_widget_rep::qt_popup_widget_rep (widget wid, command _quit)
: qt_widget_rep(qt_widget_rep::popup_widget, 0), quit(_quit) {
  
  qwid = new QTMPopupWidget(concrete(wid)->as_qwidget());

  if (qwid->metaObject() ->
      indexOfSignal (QMetaObject::normalizedSignature ("closed()").constData ()) != -1) {
  QTMCommand* qtmcmd = new QTMCommand(qwid, quit);
  QObject::connect(qwid, SIGNAL (closed()), qtmcmd, SLOT (apply()));
  }
}

/*!
 WARNING! This should be the only place were QWidgets are destroyed!
 */
qt_popup_widget_rep::~qt_popup_widget_rep () {  
  delete qwid;
}

widget
qt_popup_widget_rep::popup_window_widget(string s) {
  qwid->setWindowTitle(to_qstring(s)); // useless for Qt::Popup

  return this;
}

void
qt_popup_widget_rep::send (slot s, blackbox val) {

  switch (s) {
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      qwid->resize (to_qsize (open_box<coord2> (val)));
    }
      break;
      
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      qwid->move (to_qpoint (open_box<coord2> (val)));
    }
      break;
      
    case SLOT_VISIBILITY:
    {
      check_type<bool> (val, s);
      qwid->setVisible(open_box<bool> (val));
    }
      break;
      
      //FIXME: what's this?
    case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab
      
      qwid->hide();
      if (flag) qwid->setWindowModality(Qt::WindowModal); //ok?
      else      qwid->setWindowModality(Qt::NonModal);    //ok?
      qwid->show();
    }   
      break;

    default:
      qt_widget_rep::send(s, val);
  }
  
  if (DEBUG_QT)
    cout << "qt_popup_widget_rep: caught " << slot_name (s) 
         << "\t\tsent to widget\t"         << type_as_string() << LF;
}

blackbox
qt_popup_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT)
    cout << "qt_popup_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qpoint (qwid->pos()));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qsize (qwid->size()));
    }
    default:
      return qt_widget_rep::query (s, type_id);
  }
}
