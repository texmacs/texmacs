
/******************************************************************************
 * MODULE     : qt_view_widget.cpp
 * DESCRIPTION: QT View Widget class implementation
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_view_widget.hpp"
#include "qt_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "message.hpp"
#include "converter.hpp"

#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
#include "QTMStyle.hpp"


/******************************************************************************
 * Policy: qt_view_widget_rep owns the QWidget
 ******************************************************************************/

/**
 * Constructor. Sets the view to the specified QWidget, of which it takes
 * ownership.
 */
qt_view_widget_rep::qt_view_widget_rep (QWidget* v)
 : qt_widget_rep(), view(v), current_renderer(NULL)  {}

/**
 * FIXME: I'm (MG) not sure if we should delete manually all the QWidgets we 
 * create or exclusively the top level ones (the windows)
 *   - Qt specifies that widgets with a parent are deleted by the parent.
 *   - Our policy is that qt_view_widget_rep owns the QWidget (so it is 
 *     responsible to delete it)
 *  Are these two requirements compatible ?
 */
qt_view_widget_rep::~qt_view_widget_rep() {
  if (view) // this is redundant: delete does nothing if view==NULL
    delete view;
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::~qt_view_widget_rep()\n";
}


void
qt_view_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::send " << slot_name (s) << LF;

  switch (s) {
    case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      view->window() -> setWindowTitle (to_qstring (tm_var_encode(name)));
    }
      break;

    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (ren && canvas) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        canvas->invalidate_rect (x1,y2,x2,y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (canvas) canvas->invalidate_all ();
    }
      break;

    case SLOT_MOUSE_GRAB:
      NOT_IMPLEMENTED;
      //send_mouse_grab (THIS, val);
      break;
      
    case SLOT_MOUSE_POINTER:
      NOT_IMPLEMENTED;
      //send_mouse_pointer (THIS, val);
      break;
      
    case SLOT_KEYBOARD_FOCUS:
      //send_keyboard_focus (THIS, val);
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool> (val)) 
        the_keyboard_focus = this;
      if (DEBUG_QT)
        cout << "Ignored!\n";
      break;
    case SLOT_REFRESH:
      // This message is sent to refresh special widgets. Usually we just ignore it.
      break;
    default:
      FAILED ("unhandled slot type");
  }
}

/**
 * Querying
 */
blackbox
qt_view_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_view_widget_rep::query " << slot_name(s) << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
      TYPE_CHECK (type_id == type_helper<int>::id);
        // return close_box<int> ((int)view->window());
        // we need only to know if the widget is attached to some gui window
      return close_box<int> (view->window() ? 1 : 0);
#if 0
    case SLOT_RENDERER:
    {
      TYPE_CHECK (type_id == type_helper<renderer>::id);
      renderer r = get_current_renderer();
        //FIXME: sometimes the renderer is queried outside repaint events 
        //       (see e.g. edit_interface_rep::idle_time)
        //       TeXmacs current policy is that we should return NULL only 
        //       if the widget is not attached (in X11 sense)
      if (!r) 
        r = the_qt_renderer();
      
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (r && canvas) {
        SI ox = -canvas->backing_pos.x()*PIXEL;
        SI oy = canvas->backing_pos.y()*PIXEL;
        r->set_origin(ox,oy);
      }
      
      return close_box<renderer> (r);
    }      
#endif
    case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= view->pos();
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
    default:
      FAILED ("cannot handle slot type");
      return blackbox ();
  }
}

/**
 * Notification of state changes
 * Overriden to provide debugging support. Can be removed.
 */
void
qt_view_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::notify " << slot_name(s) << LF;
  qt_widget_rep::notify (s, new_val);
}

/**
 * Read access to subwidgets
 */
widget
qt_view_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::read " << slot_name(s) << LF;
  
  switch (s) {
    case SLOT_WINDOW:
    {
      check_type_void (index, "SLOT_WINDOW");
      QWidget* qwin = view->window();
      QVariant v= qwin->property ("texmacs_window_widget");
      if (v.canConvert<void*> ())
        return (widget_rep*) (v.value<void*> ());
      else FAILED ("QWidget property not set");
    }
      break; // not reached     
    default:
      FAILED ("cannot handle slot type");
      return widget();
  }
}

/**
 * Write access to subwidgets
 */
void
qt_view_widget_rep::write (slot s, blackbox index, widget w) {
  (void) index; (void) w;
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::write " << slot_name (s) << LF;
  switch (s) {
    default:
      FAILED ("cannot handle slot type");
  }
}


/*!
 * Sets the view's title to _title, then constructs a wrapper widget for the
 * view and returns it.
 */
widget
qt_view_widget_rep::plain_window_widget (string _title, command q) {
  view->setWindowTitle (to_qstring (_title));
  widget wid= tm_new<qt_window_widget_rep> (view, q);
    //FIXME: is this the right thing to do?
  return wid;
}


