
/******************************************************************************
* MODULE     : qt_simple_widget.hpp
* DESCRIPTION: A widget containing a TeXmacs canvas.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_SIMPLE_WIDGET_HPP
#define QT_SIMPLE_WIDGET_HPP

#include "qt_view_widget.hpp"
#include "QTMWidget.hpp"

/*! A widget containing a TeXmacs canvas.
 
 This canvas can be used both for input or output of typesetted documents. 
 Editors (editor_rep), output-only widgets (box_widget_rep) and
 other classes are extensions to a "simple_widget", quite a misnomer...

 FIXME: check how box_widget_rep, which subclasses this to implement what in
 scheme code is a texmacs-output widget does resizing (is it wrong?). The
 problem with these widgets seems to be that set_extents is never called. ??
 */
class qt_simple_widget_rep: public qt_view_widget_rep {
  
public:
  qt_simple_widget_rep ()	
    : qt_view_widget_rep (new QTMWidget (0, this), simple_widget) { }

  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t);
  virtual void handle_set_shrinking_factor (int sf);
  virtual void handle_clear (SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (SI x1, SI y1, SI x2, SI y2);
  
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);

  virtual QAction* as_qaction();
  virtual QWidget* get_canvas() { return qwid; }
protected:
  QTMWidget* canvas () { return static_cast<QTMWidget*>(qwid); }
};

inline qt_simple_widget_rep *concrete_simple_widget (widget w) { 
  return static_cast<qt_simple_widget_rep*>(w.rep); 
}

// Export for TeXmacs' use
typedef qt_simple_widget_rep simple_widget_rep;

#endif // defined QT_SIMPLE_WIDGET_HPP
