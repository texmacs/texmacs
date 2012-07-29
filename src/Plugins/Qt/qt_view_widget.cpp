
/******************************************************************************
 * MODULE     : qt_view_widget.cpp
 * DESCRIPTION: A widget with a renderer.
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_widget.hpp"
#include "qt_view_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_simple_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "message.hpp"
#include "converter.hpp"

#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
#include "QTMStyle.hpp"


/*!
 Sets the view to the specified QTMWidget.
 */
qt_view_widget_rep::qt_view_widget_rep (QTMWidget* _view, types _type)
 : qt_widget_rep(_type, _view), current_renderer(NULL)  {}

void
qt_view_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      qwid->window()->setWindowTitle (to_qstring (tm_var_encode(name)));
    }
      break;

    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "   Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = static_cast<qt_renderer_rep*>(get_renderer(this));
      if (ren && canvas()) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        canvas()->invalidate_rect (x1, y2, x2, y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "   Invalidating all"<<  LF;
      if (canvas())
        canvas()->invalidate_all ();
    }
      break;

    case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect (p);
        //NOTE: rect.topLeft is ignored since it is always (0,0)
      canvas()->setExtents(rect);
    }
      break;
      
    case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt= to_qpoint (p);
      if (DEBUG_QT)
        cout << "   Position (" << pt.x() << "," << pt.y() << ")\n ";
      scrollarea()->setOrigin(pt);
    }
      break;
      
    case SLOT_SHRINKING_FACTOR:
    {  
      TYPE_CHECK (type_box (val) == type_helper<int>::id);
      if (canvas()) {
        int new_sf = open_box<int> (val);
        if (DEBUG_QT) cout << "   New shrinking factor :" << new_sf << LF;
        canvas()->tm_widget()->handle_set_shrinking_factor (new_sf);
      }
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
    {
      //send_keyboard_focus (THIS, val);
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool> (val)) 
        the_keyboard_focus = this;
      if (DEBUG_QT)
        cout << "   Ignored!\n";
    }
      break;
      
    case SLOT_CURSOR:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p = open_box<coord2> (val);
      canvas()->setCursorPos(to_qpoint(p));
    }
      break;

    default:
      qt_widget_rep::send(s, val);
      return;
  }

  if (DEBUG_QT)
    cout << "qt_view_widget_rep: caught " << slot_name (s) 
         << "\t\tsent to widget\t" << type_as_string() << LF;  
}


blackbox
qt_view_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_view_widget_rep::query() " << slot_name(s) << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      TYPE_CHECK (type_id == type_helper<int>::id);
        // return close_box<int> ((int)view->window());
        // we need only to know if the widget is attached to some gui window
      return close_box<int> (qwid->window() ? 1 : 0);
    }

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
      SI ox = - canvas()->backing_pos.x()*PIXEL;
      SI oy = canvas()->backing_pos.y()*PIXEL;
      r->set_origin(ox,oy);
      
      return close_box<renderer> (r);
    }      
      
    case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt = qwid->mapTo(qwid->window(), QPoint(0,0));
      if (DEBUG_QT)
        cout << "   Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }

    case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QSize s= qwid->size();
      return close_box<coord2> (from_qsize (s));
    }
    
    case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= canvas()->origin();
      if (DEBUG_QT)
        cout << "   Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
      
    case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect= canvas()->extents();
      coord4 c= from_qrect (rect);
        //if (DEBUG_QT) 
      cout << "   Canvas geometry " << rect << LF;
      return close_box<coord4> (c);
    }
      
    case SLOT_VISIBLE_PART:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QSize sz = canvas()->surface()->size();
        //sz.setWidth(sz.width()-2);
      QPoint pos = canvas()->backing_pos;
      coord4 c = from_qrect(QRect(pos,sz));
      if (DEBUG_QT) 
        cout << "   Visible Region " << c << LF;
      return close_box<coord4> (c);
    }

    default:
      return qt_widget_rep::query(s, type_id);
  }
}


widget
qt_view_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::read " << slot_name(s) << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, "SLOT_WINDOW");
      return qt_window_widget_rep::widget_from_qwidget(qwid);
    default:
      return qt_widget_rep::read (s, index);
  }
}
