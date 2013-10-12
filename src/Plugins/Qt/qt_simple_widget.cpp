
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
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "QTMWidget.hpp"
#include "QTMMenuHelper.hpp"
#include <QPixmap>

qt_simple_widget_rep::qt_simple_widget_rep ()
 : qt_view_widget_rep (simple_widget), sequencer (0) { }

QWidget*
qt_simple_widget_rep::as_qwidget () {
  qwid = new QTMWidget (0, this);
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
qt_simple_widget_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
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
    cout << ">>>>>>>> reapply_sent_slots() for widget: " << type_as_string() << LF;
  
  t_slot_entry sorted_slots[slot_id__LAST];
  for (int i = 0; i < slot_id__LAST; ++i)
    sorted_slots[i] = sent_slots[i];
  qSort (&sorted_slots[0], &sorted_slots[slot_id__LAST]);
  
  for (int i = 0; i < slot_id__LAST; ++i)
    if (sorted_slots[i].seq >= 0)
      this->send(sorted_slots[i].id, sorted_slots[i].val);
  
  if (DEBUG_QT_WIDGETS)
    cout << "<<<<<<<< reapply_sent_slots() for widget: " << type_as_string() << LF;
}

void
qt_simple_widget_rep::send (slot s, blackbox val) {
  save_send_slot (s, val);
  qt_view_widget_rep::send (s, val);
}


/******************************************************************************
 * Tanslation into QAction for insertion in menus (i.e. for buttons)
 ******************************************************************************/

  // Prints the current contents of the canvas onto a QPixmap
QPixmap
impress (qt_simple_widget_rep* wid) {
  if (wid) {
    int width, height;
    wid->handle_get_size_hint (width, height);
    QSize s = to_qsize (width, height);
    QPixmap pxm (s);
    if (DEBUG_QT)
      cout << "impress (" << s.width() << "," << s.height() << ")\n";
    pxm.fill (Qt::transparent);
    {
      qt_renderer_rep *ren = the_qt_renderer();
      ren->begin (static_cast<QPaintDevice*>(&pxm));
      rectangle r = rectangle (0, 0, s.width(), s.height());
      ren->set_origin (0,0);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      {
          // we do not want to be interrupted here...
        extern bool disable_check_event;
        bool cache = disable_check_event;
        disable_check_event = true;
        wid->handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
        disable_check_event = cache;
      }
      ren->end();
    }
    return pxm;
  }
  else {
      // return arbitrary image...
    QPixmap pxm (10, 10);
    return pxm;
  }
}

QAction*
qt_simple_widget_rep::as_qaction () {
  QAction* a= new QTMAction (NULL);
  QPixmap pxm (impress (this));
  QIcon icon (pxm);
  a->setIcon (icon);
  return a;
}
