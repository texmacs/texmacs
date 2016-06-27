
/******************************************************************************
* MODULE     : qt_simple_widget.hpp
* DESCRIPTION: A widget containing a TeXmacs canvas.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "QTMWidget.hpp"
#include "QTMMenuHelper.hpp"
#include <QPixmap>
#include <QLayout>

qt_simple_widget_rep::qt_simple_widget_rep ()
 : qt_widget_rep (simple_widget), self (this), sequencer (0) { }

QWidget*
qt_simple_widget_rep::as_qwidget () {
  qwid = new QTMWidget (0, self);
  reapply_sent_slots();
  SI width, height;
  handle_get_size_hint (width, height);
  QSize sz = to_qsize (width, height);
  scrollarea()->editor_flag= is_editor_widget ();
  scrollarea()->setExtents (QRect (QPoint(0,0), sz));
  canvas()->resize (sz);
  return qwid;
}

/******************************************************************************
* Empty handlers for redefinition by our subclasses editor_rep, 
* box_widget_rep...
******************************************************************************/

bool
qt_simple_widget_rep::is_editor_widget () {
  return false;
}

void
qt_simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
qt_simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
qt_simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
qt_simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
qt_simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t;
}

void
qt_simple_widget_rep::handle_set_zoom_factor (double zoom) {
  (void) zoom;
}

void
qt_simple_widget_rep::handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
qt_simple_widget_rep::handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}


/******************************************************************************
* Handling of TeXmacs messages
******************************************************************************/

/*! Stores messages (SLOTS) sent to this widget for later replay.
 
 This is useful for recompilation of the QWidget inside as_qwidget() in some
 cases, where state information of the parsed widget (i.e. the qt_widget) is
 stored by us directly in the QWidget, and thus is lost if we delete it.

 Each SLOT is stored only once, repeated occurrences of the same one overwriting
 previous ones. Sequence information is also stored, allowing for correct replay.
 */
void
qt_simple_widget_rep::save_send_slot (slot s, blackbox val) {
  sent_slots[s].seq = sequencer;
  sent_slots[s].val = val;
  sent_slots[s].id  = s.sid;
  sequencer = (sequencer + 1) % slot_id__LAST;
}

void
qt_simple_widget_rep::reapply_sent_slots () {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << ">>>>>>>> reapply_sent_slots() for widget: " << type_as_string() << LF;
  
  t_slot_entry sorted_slots[slot_id__LAST];
  for (int i = 0; i < slot_id__LAST; ++i)
    sorted_slots[i] = sent_slots[i];
  qSort (&sorted_slots[0], &sorted_slots[slot_id__LAST]);
  
  for (int i = 0; i < slot_id__LAST; ++i)
    if (sorted_slots[i].seq >= 0)
      this->send(sorted_slots[i].id, sorted_slots[i].val);
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "<<<<<<<< reapply_sent_slots() for widget: " << type_as_string() << LF;
}

void
qt_simple_widget_rep::send (slot s, blackbox val) {
  save_send_slot (s, val);

  switch (s) {
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
      canvas()->resize (to_qsize(p));   // FIXME?
    }
      break;
      
    case SLOT_SCROLL_POSITION:
    {
      check_type<coord2>(val, s);
      coord2  p = open_box<coord2> (val);
      QPoint qp = to_qpoint (p);
      QSize  sz = canvas()->surface()->size();
      qp -= QPoint (sz.width() / 2, sz.height() / 2);
        // NOTE: adjust because child is centered
      scrollarea()->setOrigin (qp);
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
    {
      check_type<bool> (val, s);
      bool grab = open_box<bool>(val);
      if (grab && canvas() && !canvas()->hasFocus())
        canvas()->setFocus (Qt::MouseFocusReason);
    }
      break;

    case SLOT_MOUSE_POINTER:
    {
      typedef pair<string, string> T;
      check_type<T> (val, s);
      T contents = open_box<T> (val); // x1 = name, x2 = mask.
      /*
      if (contents.x2 == "")   // mask == ""
        ;                      // set default pointer.
      else                     // set new pointer
        ;
      */
      NOT_IMPLEMENTED("qt_simple_widget::SLOT_MOUSE_POINTER");
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
  
  if (DEBUG_QT_WIDGETS && s != SLOT_INVALIDATE)
    debug_widgets << "qt_simple_widget_rep: sent " << slot_name (s)
    << "\t\tto widget\t" << type_as_string() << LF;
}

blackbox
qt_simple_widget_rep::query (slot s, int type_id) {
    // Some slots are too noisy
  if (DEBUG_QT_WIDGETS && (s != SLOT_IDENTIFIER))
    debug_widgets << "qt_simple_widget_rep: queried " << slot_name(s)
                  << "\t\tto widget\t" << type_as_string() << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      if (qwid) {
        widget_rep* wid = qt_window_widget_rep::widget_from_qwidget(qwid);
        if (wid)
          return wid->query(s, type_id);
      }
      return close_box<int>(0);
    }
    case SLOT_INVALID:
    {
      return close_box<bool> (canvas() ? canvas()->is_invalid() : false);
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
qt_simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_simple_widget_rep::read " << slot_name(s)
    << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      return qt_window_widget_rep::widget_from_qwidget(qwid);
    default:
      return qt_widget_rep::read (s, index);
  }
}


/******************************************************************************
* Tanslation into QAction for insertion in menus (i.e. for buttons)
******************************************************************************/

// Prints the current contents of the canvas onto a QPixmap
QPixmap
impress (qt_simple_widget_rep* wid) {
  int width, height;
  wid->handle_get_size_hint (width, height);
  QSize s = to_qsize (width, height);
  QSize phys_s = s; phys_s *= retina_factor;
  QPixmap pxm (phys_s);
  if (DEBUG_QT)
    debug_qt << "impress (" << s.width() << "," << s.height() << ")\n";
  pxm.fill (Qt::transparent);
  {
    qt_renderer_rep *ren = the_qt_renderer();
    ren->begin (static_cast<QPaintDevice*>(&pxm));
    rectangle r = rectangle (0, 0,  phys_s.width(), phys_s.height());
    ren->set_origin (0, 0);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    {
        // we do not want to be interrupted here...
      the_gui->set_check_events (false);
      wid->handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
      the_gui->set_check_events (true);
    }
    ren->end();
  }
  return pxm;
}

QAction*
qt_simple_widget_rep::as_qaction () {
  QAction* a= new QTMAction (NULL);
  QPixmap pxm (impress (this));
  QIcon icon (pxm);
  a->setIcon (icon);
  return a;
}
