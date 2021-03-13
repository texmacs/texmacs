
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
#include "window.hpp"

#include <QWidget>
#include <QVariant>
#include <QDockWidget>

/*! Construct a qt_window_widget_rep around an already compiled widget.
 
 This means that the QWidget passed as an argument already represents a fully 
 parsed and compiled scheme widget. We use this fact to decide whether we must
 be resizable or not, for instance.
 
 The parameter "fake" means this qt_window_widget_rep is not part of the window
 list, so the nr_windows global variable must not be updated.
 */
qt_window_widget_rep::qt_window_widget_rep (QWidget* _wid, string name,
                                            command _quit, bool _fake)
: qt_widget_rep (window_widget, _wid), orig_name(name), quit(_quit), fake(_fake)
{
  qwid->setProperty ("texmacs_window_widget",
                     QVariant::fromValue ((void*) this));
  
    // Try to connect only if the QWidget has a closed() signal
    // We need this for the QDockWidgets we use in side tools (see qt_tm_widget_rep)
  if (qwid->metaObject() -> 
      indexOfSignal (QMetaObject::normalizedSignature ("closed()").constData ()) != -1) {
    QTMCommand* qtmcmd = new QTMCommand (qwid, quit);
    QObject::connect(qwid, SIGNAL (closed()), qtmcmd, SLOT (apply()));
  }

  if (!has_resizable_children (_wid))
    qwid->setFixedSize (qwid->sizeHint());
  
    // HACK: don't increment window count for side tools or any other fake windows
  if (!fake) win_id = ++nr_windows;
  
  if (DEBUG_QT)
    debug_qt << "Creating qt_window_widget " << id << "\n";

  QPalette pal;
  QColor winbg= pal.color (QPalette::Background);
  if (winbg.red() + winbg.green() + winbg.blue () < 255)
    pal.setColor (QPalette::Background, QColor (240, 240, 240));
  _wid->setPalette (pal);
}

/*!
 WARNING! This should be the only place were QWidgets are destroyed!
 */
qt_window_widget_rep::~qt_window_widget_rep ()
{
  if (!fake) nr_windows--;
  if (DEBUG_QT)
    debug_qt << "Deleting qt_window_widget " << id << "\n";
  if (qwid) qwid->deleteLater();
}

widget
qt_window_widget_rep::popup_window_widget (string s)
{
  qwid->setWindowTitle (to_qstring (s));
  qwid->setWindowModality (Qt::NonModal);
  qwid->setWindowFlags (Qt::Popup);
  return this;
}

/*! Looks among the widget's parents for the containing texmacs window
 */
widget_rep* 
qt_window_widget_rep::widget_from_qwidget (QWidget* q)
{
  while (q != NULL) {
    QVariant v = q->property ("texmacs_window_widget");
    if (v.canConvert<void*> ())
      return static_cast<widget_rep*> (v.value<void*> ());
    else
      q = q->parentWidget();
  }
  return NULL;
}

/*! Returns true if any of the child QWidgets has different minimum and maximum
 sizes, as set by qt_ui_element_rep::as_qwidget() for resize_widgets.
 */
bool
qt_window_widget_rep::has_resizable_children (QWidget* w, bool ret) {
    // Ignore any non QWidgets
  if (!w) return false;
  
    // Hack: these must always be resizable
  if (qobject_cast<QMainWindow*> (w) || qobject_cast<QDockWidget*> (w))
    return true;

  ret = (w->minimumSize() != QSize (0,0) &&
         w->maximumSize() != QSize (QWIDGETSIZE_MAX, QWIDGETSIZE_MAX) &&
         w->minimumSize() != w->maximumSize());
  
  QObjectList ch = w->children();
  for (int i=0; i<ch.size(); ++i)
    ret = ret || has_resizable_children (qobject_cast<QWidget*> (ch[i]), ret);
  
  return ret;
}

void
qt_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_window_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      if (qwid) {
        QSize size = to_qsize (p);
        qwid->resize (size);
      }
    }
      break; 
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
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
          //QWidget* master = QApplication::activeWindow ();
          qwid->show();
          //qwid->activateWindow();
          //WEIRD: in Ubuntu uncommenting the above line causes the main window 
          //to be opened in the background.
          qwid->raise();
          //QApplication::setActiveWindow (master);
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
        qwid->setWindowFlags (Qt::Window);  // ok?
        qwid->setWindowModality (Qt::WindowModal); //ok?
        qwid->show();
      }
    }   
      break;
    case SLOT_NAME:   // sets window *title* not the name
    {   
      check_type<string> (val, s);
      string name = open_box<string> (val);
        // The [*] is for QWidget::setWindowModified()
      if (qwid) qwid->setWindowTitle (to_qstring (name * "[*]"));
    }
      break;
    case SLOT_MODIFIED:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      if (qwid) qwid->setWindowModified (flag);
    }
      break;
    case SLOT_REFRESH:
    {
      check_type<string> (val, s);
      string kind = open_box<string> (val);
      the_gui->gui_helper->emitTmSlotRefresh (kind);
    }
      break;
    default:
      qt_widget_rep::send(s, val);
  }
}

blackbox
qt_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      check_type_id<int> (type_id, s);
      return close_box<int> (win_id);
    }

    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      QRect g;
        // FIXME: dock widgets are embedded into qt_window_widget_reps as a temporary hack
        // because of this the underlying widget is not always a top level window
      if (qwid->isWindow()) g = qwid->frameGeometry();
      else                  g = qwid->window()->frameGeometry();
        //cout << "wpos: " << pt.x() << ", " << pt.y() << LF;
      return close_box<coord2> (from_qpoint (QPoint (g.x(), g.y())));
    }

    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      QSize sz = qwid->frameSize();
      return close_box<coord2> (from_qsize (sz));
    }

    default:
      return qt_widget_rep::query (s, type_id);
  }
}

widget
qt_window_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_window_widget_rep::read " << slot_name(s)
                  << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_WINDOW:  // We use this in qt_gui_rep::show_help_balloon()
      check_type_void (index, s);
      return this;

    default:
      return qt_widget_rep::read (s, index);
  }
}

void
qt_window_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_window_widget_rep::notify " << slot_name(s) << LF;
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
  if (qwid) qwid->deleteLater();
}

widget
qt_popup_widget_rep::popup_window_widget(string s) {
  qwid->setWindowTitle (to_qstring (s)); // useless for Qt::Popup

  return this;
}

widget
qt_popup_widget_rep::tooltip_window_widget (string s) {
  qwid->setWindowTitle (to_qstring (s));
  qwid->setWindowModality (Qt::NonModal);
  qwid->setWindowFlags (Qt::ToolTip);
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
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_popup_widget_rep: sent " << slot_name (s) 
                  << "\t\tto widget\t"         << type_as_string() << LF;
}

blackbox
qt_popup_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_popup_widget_rep::query " << slot_name(s) << LF;
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
