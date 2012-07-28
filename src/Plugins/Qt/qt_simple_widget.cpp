
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

#include "message.hpp"
#include "QTMMenuHelper.hpp"
#include <QPixmap>


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
  //if (DEBUG_QT) cout << "qt_qt_simple_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_CURSOR:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p = open_box<coord2> (val);
      canvas()->setCursorPos(to_qpoint(p));
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
    default:
      qt_view_widget_rep::send (s, val);
  }
}


blackbox
qt_simple_widget_rep::query (slot s, int type_id) {
  //if ((DEBUG_QT) && (s != SLOT_RENDERER))
  //cout << "qt_qt_simple_widget_rep::query " << slot_name(s) << LF;
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
      SI ox = - canvas()->backing_pos.x()*PIXEL;
      SI oy = canvas()->backing_pos.y()*PIXEL;
      r->set_origin(ox,oy);

      return close_box<renderer> (r);
    }
      
    default:
      return qt_view_widget_rep::query (s, type_id);      
  }
}

/*!
 HACK: sometimes the QTMWidget underlying this widget is deleted, for instance
 when destroying the QWidgets in a QTMRefreshWidget. Because of this we cannot
 simply return the qwid pointer. As a convention we could assume that if this
 method is being called on this object, then a new QTMWidget is to be built.
 However:
 FIXME: this breaks stuff elsewhere. qt_simple_widget works in a different way
 to qt_ui_element_rep and cia. There is no difference between parsing and
 compilation of the scheme trees: upon parsing the QTMWidget is built and its
 properties are set with TeXmacs messages. These properties are not remembered
 by the qt_simple_widget_rep: if the QTMWidget is deleted, they are lost and we
 cannot build a new copy.
 
 We could fix this, by duplicating state information in the qt_simple_widget_rep.
 A possibility is to add some stack of sent messages: we store the last of each
 type (at the end of send(), write(), etc.) provided that each message totally
 overwrites the previous, i.e. that its effect is independent of previous state.
 */
QWidget*
qt_simple_widget_rep::as_qwidget () {
    //qwid = new QTMWidget(0, this);
    //reapply_changes_to_qwid();
  return qwid;
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
    QSize s = QSize (width/PIXEL, height/PIXEL);
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
