
/******************************************************************************
* MODULE     : qt_simple_widget.hpp
* DESCRIPTION: A widget containing a TeXmacs canvas.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "iterator.hpp"

#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "QTMWidget.hpp"
#include "QTMMenuHelper.hpp"
#include <QPixmap>
#include <QLayout>


#ifdef USE_CAIRO
#include "Cairo/cairo_renderer.hpp"
#include "Cairo/tm_cairo.hpp"

#if defined (Q_OS_LINUX)
#include <QX11Info>
extern Drawable qt_x11Handle (const QPaintDevice *pd);
extern const QX11Info *qt_x11Info (const QPaintDevice *pd);
#undef KeyPress  // conflict between QEvent::KeyPress and X11 defintion
#endif // Q_WS_X11
#endif // USE_CAIRO



qt_simple_widget_rep::qt_simple_widget_rep ()
 : qt_widget_rep (simple_widget),  sequencer (0) { }

qt_simple_widget_rep::~qt_simple_widget_rep () {
  all_widgets->remove ((pointer) this);
}

QWidget*
qt_simple_widget_rep::as_qwidget () {
  qwid = new QTMWidget (0, this);
  reapply_sent_slots();
  SI width, height;
  handle_get_size_hint (width, height);
  QSize sz = to_qsize (width, height);
  scrollarea()->editor_flag= is_editor_widget ();
  scrollarea()->setExtents (QRect (QPoint (0,0), sz));
  canvas()->resize (sz);
  
  backingPixmap = QPixmap(sz);
  backingPixmap.fill (to_qcolor (tm_background));
  invalidate_all ();
  
  all_widgets->insert((pointer) this);
  backing_pos = canvas()->viewportToContents (QPoint(0,0));

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
      
      {
        qt_renderer_rep* ren = the_qt_renderer();
        {
          coord2 pt_or = from_qpoint(backing_pos);
          SI ox = -pt_or.x1;
          SI oy = -pt_or.x2;
          ren->set_origin(ox,oy);
        }
        
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        invalidate_rect (x1, y2, x2, y1);
      }
    }
      break;
      
    case SLOT_INVALIDATE_ALL:
    {
      check_type_void (val, s);
      invalidate_all ();
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
      return close_box<bool> (is_invalid());
    }
      
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
        // HACK: mapTo() does not work as we expect on the Mac, so we manually
        // calculate the global screen cordinates and substract
      QPoint sg = scrollarea()->surface()->mapToGlobal (QPoint (0,0));
      QRect  wg = scrollarea()->window()->frameGeometry();
      QPoint qp = sg - wg.topLeft() - canvas()->extents().topLeft();
      return close_box<coord2> (from_qpoint (qp));
    }
      
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (from_qsize (canvas()->size()));
    }
      
    case SLOT_SCROLL_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      QPoint qp = canvas()->origin();
      return close_box<coord2> (from_qpoint (qp));
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
        QPoint pos = backing_pos;
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
* Translation into QAction for insertion in menus (i.e. for buttons)
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

/******************************************************************************
 * Backing store management
 ******************************************************************************/


void
qt_simple_widget_rep::invalidate_rect (int x1, int y1, int x2, int y2) {
#ifdef Q_OS_MAC
  //HACK: for unknown reasons we need to enlarge the invalid rect to prevent
  //artifacts while moving the cursor (for example at the end of a formula like
  // $a+f$. These artifacts seems present only on 64 bit Macs.
  rectangle r = rectangle (x1-10, y1-10, x2+10, y2+10);
#else
  rectangle r = rectangle (x1, y1, x2, y2);
#endif
  // cout << "invalidating " << r << LF;
  invalid_regions = invalid_regions | rectangles (r);  
}

void
qt_simple_widget_rep::invalidate_all () {
  QSize sz = canvas()->surface()->size();
  // QPoint pt = QAbstractScrollArea::viewport()->pos();
  //cout << "invalidate all " << LF;
  invalid_regions = rectangles();
  invalidate_rect (0, 0, retina_factor * sz.width(),
                   retina_factor * sz.height());
}

bool
qt_simple_widget_rep::is_invalid () {
  return !is_nil (invalid_regions);
}




basic_renderer
qt_simple_widget_rep::get_renderer() {
#ifdef USE_CAIRO
  cairo_renderer_rep *ren = the_cairo_renderer ();
  cairo_surface_t *surf;
#ifdef Q_OS_LINUX
  //const QX11Info & info = x11Info();//qt_x11Info (this);
  //    Display *dpy = x11Info().display();
  //backingPixmap = QPixmap (width(),height());
  //cout << backingPixmap.width() << LF;
  Display *dpy = QX11Info::display();
  Drawable drawable = backingPixmap.handle();
  Visual *visual = (Visual*)(backingPixmap.x11Info().visual());
  surf = tm_cairo_xlib_surface_create (dpy, drawable, visual,
                                       backingPixmap.width (), backingPixmap.height ());
#elif defined (Q_OS_MAC)
  surf = tm_cairo_quartz_surface_create_for_cg_context (
                                                        (CGContextRef)(this->macCGHandle()), width(), height());
#endif
  cairo_t *ct = tm_cairo_create (surf);
  ren->begin (ct);
  tm_cairo_surface_destroy (surf);
  tm_cairo_destroy (ct);
#else
  qt_renderer_rep * ren = the_qt_renderer();
  ren->begin (&backingPixmap);
#endif
  return ren;
}

/*
 This function is called by the qt_gui::update method (via repaint_all) to keep
 the backing store in sync and propagate the changes to the surface on screen.
 First we check that the backing store geometry is right and then we
 request to the texmacs canvas widget to repaint the regions which were
 marked invalid. Subsequently, for each succesfully repainted region, we
 propagate its contents from the backing store to the onscreen surface.
 If repaint has been interrupted we do not propagate the changes and proceed
 to mark the region invalid again.
 */

void
qt_simple_widget_rep::repaint_invalid_regions () {
  
  QRegion qrgn;
 // QPoint origin = canvas()->origin() + canvas()->extents().topLeft();
  QPoint origin = canvas()->viewportToContents (QPoint(0,0));

  // qrgn is to keep track of the area on the screen which needs to be updated
  
  // update backing store origin wrt. TeXmacs document
  if (backing_pos != origin) {
    
    int dx =  retina_factor * (origin.x() - backing_pos.x());
    int dy =  retina_factor * (origin.y() - backing_pos.y());
    backing_pos = origin;
    
    QPixmap newBackingPixmap (backingPixmap.size());
    QPainter p (&newBackingPixmap);
    p.drawPixmap (-dx,-dy,backingPixmap);
    p.end();
    backingPixmap = newBackingPixmap;
    //cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
    
    
    rectangles invalid;
    while (!is_nil (invalid_regions)) {
      rectangle r = invalid_regions->item ;
      //      rectangle q = rectangle (r->x1+dx,r->y1-dy,r->x2+dx,r->y2-dy);
      rectangle q = rectangle (r->x1-dx,r->y1-dy,r->x2-dx,r->y2-dy);
      invalid = rectangles (q, invalid);
      //cout << r << " ---> " << q << LF;
      invalid_regions = invalid_regions->next;
    }
    
    QSize sz = backingPixmap.size();
    
    invalid_regions= invalid & rectangles (rectangle (0,0,
                                                      sz.width(),sz.height()));
    
    if (dy<0)
      invalidate_rect (0,0,sz.width(),min (sz.height(),-dy));
    else if (dy>0)
      invalidate_rect (0,max (0,sz.height()-dy),sz.width(),sz.height());
    
    if (dx<0)
      invalidate_rect (0,0,min (-dx,sz.width()),sz.height());
    else if (dx>0)
      invalidate_rect (max (0,sz.width()-dx),0,sz.width(),sz.height());
    
    // we call update now to allow repainting of invalid regions
    // this cannot be done directly since interpose_handler needs
    // to be run at least once in some situations
    // (for example when scrolling is initiated by TeXmacs itself)
    //the_gui->update();
    //  QAbstractScrollArea::viewport()->scroll (-dx,-dy);
    // QAbstractScrollArea::viewport()->update();
    qrgn += QRect (QPoint (0,0),sz);
  }
  
  //cout << "   repaint QPixmap of size " << backingPixmap.width() << " x "
  // << backingPixmap.height() << LF;
  // update backing store size
  {
    QSize _oldSize = backingPixmap.size();
    QSize _new_logical_Size = canvas()->surface()->size();
    QSize _newSize = _new_logical_Size;
    _newSize *= retina_factor;
    
    //cout << "      surface size of " << _newSize.width() << " x "
    // << _newSize.height() << LF;
    
    
    if (_newSize != _oldSize) {
      // cout << "RESIZING BITMAP"<< LF;
      QPixmap newBackingPixmap (_newSize);
      QPainter p (&newBackingPixmap);
      p.drawPixmap (0,0,backingPixmap);
      //p.fillRect (0, 0, _newSize.width(), _newSize.height(), Qt::red);
      if (_newSize.width() >= _oldSize.width()) {
        invalidate_rect (_oldSize.width(), 0, _newSize.width(), _newSize.height());
        p.fillRect (QRect (_oldSize.width(), 0, _newSize.width()-_oldSize.width(), _newSize.height()), Qt::gray);
      }
      if (_newSize.height() >= _oldSize.height()) {
        invalidate_rect (0,_oldSize.height(), _newSize.width(), _newSize.height());
        p.fillRect (QRect (0,_oldSize.height(), _newSize.width(), _newSize.height()-_oldSize.height()), Qt::gray);
      }
      p.end();
      backingPixmap = newBackingPixmap;
    }
  }
  
  // repaint invalid rectangles
  {
    rectangles new_regions;
    if (!is_nil (invalid_regions)) {
      rectangle lub= least_upper_bound (invalid_regions);
      if (area (lub) < 1.2 * area (invalid_regions))
        invalid_regions= rectangles (lub);
      
      basic_renderer_rep* ren = get_renderer();
      
      coord2 pt_or = from_qpoint(backing_pos);
      SI ox = -pt_or.x1;
      SI oy = -pt_or.x2;
      
      rectangles rects = invalid_regions;
      invalid_regions = rectangles();
      
      while (!is_nil (rects)) {
        rectangle r = copy (rects->item);
        rectangle r0 = rects->item;
        QRect qr = QRect (r0->x1 / retina_factor, r0->y1 / retina_factor,
                          (r0->x2 - r0->x1) / retina_factor,
                          (r0->y2 - r0->y1) / retina_factor);
        //cout << "repainting " << r0 << "\n";
        ren->set_origin (ox, oy);
        ren->encode (r->x1, r->y1);
        ren->encode (r->x2, r->y2);
        ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
        handle_repaint (ren, r->x1, r->y2, r->x2, r->y1);
        if (gui_interrupted ()) {
          //cout << "interrupted repainting of  " << r0 << "\n";
          //ren->set_pencil (green);
          //ren->line (r->x1, r->y1, r->x2, r->y2);
          //ren->line (r->x1, r->y2, r->x2, r->y1);
          invalidate_rect (r0->x1, r0->y1, r0->x2, r0->y2);
        }
        qrgn += qr;
        rects = rects->next;
      }
      
      ren->end();
    } // !is_nil (invalid_regions)
  }
  
  // propagate immediately the changes to the screen
  canvas()->surface()->repaint (qrgn);
}

hashset<pointer> qt_simple_widget_rep::all_widgets;

void
qt_simple_widget_rep::repaint_all () {
  iterator<pointer> i = iterate(qt_simple_widget_rep::all_widgets);
  while (i->busy()) {
    qt_simple_widget_rep *w = static_cast<qt_simple_widget_rep*>(i->next());
    if (w->canvas() && w->canvas()->isVisible()) w->repaint_invalid_regions();
  }
}
