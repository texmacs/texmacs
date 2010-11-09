
/******************************************************************************
 * MODULE     : qt_simple_widget.hpp
 * DESCRIPTION: QT simple widget class
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_simple_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "message.hpp"



/**
 *
 */
simple_widget_rep::simple_widget_rep ():
qt_view_widget_rep (new QTMWidget (this))
{
    // view->setProperty ("texmacs_widget", QVariant::fromValue ((void*) this));
}


/******************************************************************************
 * Empty handlers for redefinition later on
 ******************************************************************************/

void
simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t;
}

void
simple_widget_rep::handle_set_shrinking_factor (int sf) {
  (void) sf;
}

void
simple_widget_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT) cout << "qt_simple_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      if (ren) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        tm_canvas()->invalidate_rect (x1,y2,x2,y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      tm_canvas()->invalidate_all ();
    }
      break;
    case SLOT_CURSOR:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt = to_qpoint(p);
      tm_canvas() -> setCursorPos(pt);
    }
      break;
      
    default:
      if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
      qt_view_widget_rep::send (s, val);
        //      FAILED ("unhandled slot type");
  }
  
}


blackbox
simple_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_simple_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
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
      
#if 1
      QTMWidget *canvas = tm_canvas();
      if (r && canvas) {
        SI ox = -canvas->backing_pos.x()*PIXEL;
        SI oy = canvas->backing_pos.y()*PIXEL;
        r->set_origin(ox,oy);
      }
#endif 
      return close_box<renderer> (r);
    }
      
    default:
      return qt_view_widget_rep::query (s, type_id);      
  }
}

void
simple_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::notify (s, new_val);
}

/******************************************************************************
 * Read and write access of subwidgets
 ******************************************************************************/

widget
simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}

void
simple_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::write (s, index, w);
}
