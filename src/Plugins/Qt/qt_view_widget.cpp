
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
qt_view_widget_rep::qt_view_widget_rep (types _type)
 : qt_widget_rep(_type), current_renderer(NULL)  { }


/******************************************************************************
 * Handling of TeXmacs messages
 ******************************************************************************/

void
qt_view_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_NAME:
    {   
      check_type<string> (val, s);
      string name = open_box<string> (val);
      if (canvas() && canvas()->window())
        canvas()->window()->setWindowTitle (to_qstring (name));
    }
      break;

    case SLOT_INVALIDATE:
    {
      check_type<coord4>(val, s);
      coord4 p= open_box<coord4> (val);
      
      if (canvas()) {
        qt_renderer_rep* ren = the_qt_renderer();
         {
          coord2 pt_or = from_qpoint(canvas()->backing_pos);
          SI ox = -pt_or.x1;
          SI oy = -pt_or.x2;
          ren->set_origin(ox,oy);
        }

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
      check_type_void (val, s);
      canvas()->invalidate_all ();
    }
      break;

    case SLOT_EXTENTS:
    {
      check_type<coord4>(val, s);
      coord4 p = open_box<coord4> (val);
      scrollarea()->setExtents (to_qrect (p));
    }
      break;
    
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      canvas()->resize(to_qsize(p));   // FIXME?
    }
      break;

    case SLOT_SCROLL_POSITION:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      scrollarea()->setOrigin(to_qpoint (p));
    }
      break;
      
    case SLOT_ZOOM_FACTOR:
    {  
      check_type<double> (val, s);
      double new_zoom = open_box<double> (val);
      canvas()->tm_widget()->handle_set_zoom_factor (new_zoom);
    }
      break;  
      
    case SLOT_MOUSE_GRAB:
        // Sent after a left click to indicate the start of cursor dragging.
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
      check_type<bool>(val, s);
      if (open_box<bool> (val)) 
        the_keyboard_focus = this;
      if (DEBUG_QT)
        cout << "   Ignored!\n";
    }
      break;
      
    case SLOT_CURSOR:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      canvas()->setCursorPos(to_qpoint(p));
    }
      break;

    default:
      qt_widget_rep::send(s, val);
      return;
  }

  if (DEBUG_QT && s != SLOT_INVALIDATE)
    cout << "qt_view_widget_rep: sent " << slot_name (s) 
         << "\t\tto widget\t" << type_as_string() << LF;  
}

blackbox
qt_view_widget_rep::query (slot s, int type_id) {
    // Some slots are too noisy
  if ((DEBUG_QT) && (s != SLOT_IDENTIFIER))
    cout << "qt_view_widget_rep: queried " << slot_name(s)
         << "\t\tto widget\t" << type_as_string() << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
      if (qwid)
        return qt_window_widget_rep::widget_from_qwidget(qwid)->query(s, type_id);
      else
        return close_box<int>(0);

    case SLOT_INVALID:
    {
      return close_box<bool>(canvas() ? canvas()->is_invalid() : false);
    }

    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
        // HACK: mapTo() does not work as we expect on the Mac, so we manually
        // calculate the global screen cordinates and substract
      QPoint sg = scrollarea()->surface()->mapToGlobal (QPoint (0,0));
      QRect  wg = scrollarea()->window()->frameGeometry();
      sg.ry() -= wg.y();
      sg.rx() -= wg.x();
      return close_box<coord2> (from_qpoint (sg));
    }

    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qsize (canvas()->size()));
    }
    
    case SLOT_SCROLL_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qpoint (canvas()->origin()));
    }
      
    case SLOT_EXTENTS:
    {
      check_type_id<coord4> (type_id, s);
      return close_box<coord4> (from_qrect (canvas()->extents()));
    }
      
    case SLOT_VISIBLE_PART:
    {
      check_type_id<coord4> (type_id, s);
      if (canvas()) {
        QSize sz = canvas()->surface()->size();     // sz.setWidth(sz.width()-2);
        QPoint pos = canvas()->backing_pos;
        return close_box<coord4> (from_qrect(QRect(pos, sz)));
      } else {
        return close_box<coord4>(coord4(0,0,0,0));
      }
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
      check_type_void (index, s);
      return qt_window_widget_rep::widget_from_qwidget(qwid);
    default:
      return qt_widget_rep::read (s, index);
  }
}
