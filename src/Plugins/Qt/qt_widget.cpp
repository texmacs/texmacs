
/******************************************************************************
* MODULE     : qt_widget.cpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_widget.hpp"
#include "qt_simple_widget.hpp"
#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"

#include "QTMMenuHelper.hpp"
#include "qt_renderer.hpp"

widget the_keyboard_focus (NULL);


/******************************************************************************
 * base widget
 ******************************************************************************/

widget
qt_widget_rep::plain_window_widget (string s) {
  (void) s;
  return widget ();
}

widget
qt_widget_rep::make_popup_widget () {
  return this;
}

widget
qt_widget_rep::popup_window_widget (string s) {
  (void) s;
  return widget ();
}


/******************************************************************************
 * glue widget
 ******************************************************************************/

class qt_glue_widget_rep: public qt_widget_rep {
public:
  
  tree col;
  bool hx, vx;
  SI w,h;
  
  
  qt_glue_widget_rep (tree _col, bool _hx, bool _vx, SI _w, SI _h)
  : col(_col), hx(_hx), vx(_vx), w(_w), h(_h) 
  {}
  
  qt_glue_widget_rep () {};
  
  QPixmap render ();
  
  virtual QAction *as_qaction();
  virtual QWidget *as_qwidget();
};


QPixmap 
qt_glue_widget_rep::render () {
  QSize s = QSize (w/PIXEL, h/PIXEL);
  QPixmap pxm(s);
  //cout << "glue (" << s.width() << "," << s.height() << ")\n";
  pxm.fill (Qt::transparent);
  QPaintDevice *pd = static_cast<QPaintDevice*>(&pxm);

  if (pd && !pxm.isNull()) {
    qt_renderer_rep *ren = the_qt_renderer();
    ren->begin (pd);
    rectangle r = rectangle (0, 0, s.width(), s.height());
    ren->set_origin(0,0);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    
    if (col == "") {
      // do nothing
    } else {
      if (is_atomic (col)) {
        color c= named_color (col->label);
        ren->set_background (c);
        ren->set_color (c);
        ren->fill (r->x1, r->y2, r->x2, r->y1);
      } else {
        ren->set_shrinking_factor (5);
        tree old_bg= ren->get_background_pattern ();
        ren->set_background_pattern (col);
        ren->clear_pattern (5*r->x1, 5*r->y2, 5*r->x2, 5*r->y1);
        ren->set_background_pattern (old_bg);
        ren->set_shrinking_factor (1);
      }
    }
    ren->end();
  }

  return pxm;  
}

QAction *
qt_glue_widget_rep::as_qaction() {
  QAction *a= new QTMAction();
  a->setText(to_qstring(as_string(col)));
  QIcon icon;
#if 0
  tree old_col = col;
  icon.addPixmap(render(), QIcon::Active, QIcon::On);
  col = "";
  icon.addPixmap(render(), QIcon::Normal, QIcon::On);
  col = old_col;
#else
  icon.addPixmap (render ());
#endif
  a->setIcon (icon);  
  a->setEnabled(false);
  return a;
}

QWidget *
qt_glue_widget_rep::as_qwidget() {
  QLabel *w= new QLabel();
  w->setText(to_qstring(as_string(col)));
  QIcon icon;
  w->setPixmap (render ());  
//  w->setEnabled(false);
  return w;
}


/******************************************************************************
* Global functions we export for the creation of widgets by TeXmacs
******************************************************************************/

// Window widgets creating functions
widget
plain_window_widget (widget w, string s) {
  // creates a decorated window with name s and contents w
  return concrete(w)->plain_window_widget (s);
}

widget
popup_window_widget (widget w, string s) {
  // creates an undecorated window with name s and contents w
  return concrete(w)->popup_window_widget (s);
}

void
destroy_window_widget (widget w) {
  // FIXME: Handle correcly
  // destroys a window as created by the above routines
  (void) w;

  // In the QT implementation explicitly destroying window widgets should not be necessary
  // since the widget itself destroy the Qt widget as soon as its destructor is called.
  // No memory leak should be caused by this trivial implementation.
}

/******************************************************************************
* Top-level widgets (??)
* See also message.hpp for specific messages for these widgets
******************************************************************************/

/*!
 * A factory for the main TeXmacs window.
 * @param mask Indicates whether the menu, icon bars, status bar, etc.
 *             are visible or not.
 * @param quit Command called on exit.
 */
widget
texmacs_widget (int mask, command quit) {
  (void) mask; (void) quit; // FIXME: handle correctly mask and quit
  widget w= tm_new<qt_tm_widget_rep> (mask, quit);
  return w;
}

/*!
 * A factory for a popup widget container whose contents are to be unmapped as 
 * soon as the mouse exits the widget. This is used in @link edit_mouse.cpp @endlink 
 * to implement a contextual menu in the canvas
 *  @param w The widget to be placed in the popup. It will be deleted after the
 *            mouse leaves the popup.
 *  @return 
 */
widget
popup_widget (widget w) {
  return concrete(w)->make_popup_widget();
}

/******************************************************************************
*  Widgets which are not strictly required by TeXmacs
*  their implementation is void
******************************************************************************/

widget
empty_widget () {
  // an empty widget of size zero
  NOT_IMPLEMENTED;
  return widget();
}

widget
glue_widget (bool hx, bool vx, SI w, SI h) {

  // an empty widget of minimal width w and height h and which is horizontally
  // resp. vertically extensible if hx resp. vx is true
  
  // glue_widget is used when detaching a canvas from the texmacs window
  // in view of attaching another one, e.g. when changing buffer.
  
  return glue_widget("", hx, vx, w, h);
}

widget
glue_widget (tree col, bool hx, bool vx, SI w, SI h) {
  return tm_new<qt_glue_widget_rep> (col, hx, vx, w, h);
}

widget
extend (widget w, array<widget> a) {
  (void) a;
  return w;
}

widget
wait_widget (SI width, SI height, string message) {
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
  (void) width; (void) height; (void) message;
  return widget();
}
