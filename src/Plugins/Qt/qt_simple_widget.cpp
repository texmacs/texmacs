
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
 : qt_view_widget_rep (simple_widget) { }

QWidget*
qt_simple_widget_rep::as_qwidget () {
  qwid = new QTMWidget(0, this);
  reapply_sent_slots();
  SI width, height;
  handle_get_size_hint (width, height);
  QSize sz = to_qsize(width, height);
  scrollarea()->setExtents (QRect (QPoint(0,0), sz));
  canvas()->resize(sz);
  return qwid;
}


/******************************************************************************
 * Empty handlers for redefinition by our subclasses editor_rep, 
 * box_widget_rep...
 ******************************************************************************/

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
qt_simple_widget_rep::handle_set_shrinking_factor (int sf) {
  (void) sf;
}

void
qt_simple_widget_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
qt_simple_widget_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}


/******************************************************************************
 * Handling of TeXmacs messages
 ******************************************************************************/

void
qt_simple_widget_rep::send (slot s, blackbox val) {
  save_send_slot(s, val);
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
    QSize s = to_qsize(width, height);
    QPixmap pxm(s);
    if (DEBUG_QT)
      cout << "impress (" << s.width() << "," << s.height() << ")\n";
    pxm.fill (Qt::transparent);
    {
      qt_renderer_rep *ren = the_qt_renderer();
      ren->begin (static_cast<QPaintDevice*>(&pxm));
      wid->set_current_renderer(the_qt_renderer());
      rectangle r = rectangle (0, 0, s.width(), s.height());
      ren->set_origin(0,0);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      {
          // we do not want to be interrupted here...
        extern bool disable_check_event;
        bool cache = disable_check_event;
        disable_check_event= true;
        wid->handle_repaint (r->x1, r->y2, r->x2, r->y1);
        disable_check_event= cache;
      }
      ren->end();
      wid->set_current_renderer(NULL);
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
