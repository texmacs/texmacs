
/******************************************************************************
* MODULE     : ink_widget.cpp
* DESCRIPTION: Widget for inking
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Ink points
******************************************************************************/

class ink_point_rep: concrete_struct {
public:
  double x;
  double y;
  double p; // pressure
  inline ink_point_rep (double x2, double y2, double p2):
    x (x2), y (y2), p (p2) {}
  friend class ink_point;
};

class ink_point {
CONCRETE(ink_point);
  inline ink_point (double x= 0.0, double y= 0.0, double p= 1.0) {
    rep= tm_new<ink_point_rep> (x, y, p); }
  inline operator tree () {
    return tree (TUPLE,
                 as_string (rep->x), as_string (rep->y),
                 as_string (rep->p)); }
};
CONCRETE_CODE(ink_point);

tm_ostream&
operator << (tm_ostream& out, ink_point p) {
  out << "[ " << p->x << ", " << p->y << " ]";
  return out;
}

typedef array<ink_point> ink_shape;

/******************************************************************************
* Ink widget
******************************************************************************/

class ink_widget_rep: public attribute_widget_rep {
  array<ink_shape> shs;
  bool dragging;

public:
  ink_widget_rep ();
  operator tree ();
  void refresh_last ();
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

/******************************************************************************
* Routines for ink_widgets
******************************************************************************/

ink_widget_rep::ink_widget_rep ():
  attribute_widget_rep (), shs (), dragging (false) {}

ink_widget_rep::operator tree () {
  return tree (TUPLE, "ink");
}

void
ink_widget_rep::handle_get_size (get_size_event ev) {
  ev->w= 600 * PIXEL;
  ev->h= 400 * PIXEL;
  if (ev->mode == 1) {
    ev->w= 1280 * PIXEL;
    ev->h= 400 * PIXEL;
  }
  abs_round (ev->w, ev->h);
}

void
ink_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  layout_pastel (ren, ev->x1, ev->y1, ev->x2, ev->y2);
  ren->set_color (black);
  ren->set_line_style (2 * PIXEL);
  for (int i=0; i<N(shs); i++) {
    ink_shape sh= shs[i];
    int n= N(sh);
    if (n>1) {
      array<SI> x (n);
      array<SI> y (n);
      for (int j=0; j<n; j++) {
        x[j]= (SI) (sh[j]->x * PIXEL);
        y[j]= (SI) (sh[j]->y * PIXEL);
      }
      ren->lines (x, y);
    }
  }
}

void
ink_widget_rep::refresh_last () {
  if (N(shs) > 0) {
    ink_shape& sh= shs [N(shs)-1];
    ink_point& p = sh [N(sh)-2];
    ink_point& q = sh [N(sh)-1];
    SI x1= min (p->x, q->x) * PIXEL;
    SI y1= min (p->y, q->y) * PIXEL;
    SI x2= max (p->x, q->x) * PIXEL;
    SI y2= max (p->y, q->y) * PIXEL;
    this << emit_invalidate (x1 - PIXEL, y1 - PIXEL, x2 + PIXEL, y2 + PIXEL);
  }
}

void
ink_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  //cout << type << ", " << x/PIXEL << ", " << y/PIXEL << "\n";
  if (type == "press-left") {
    ink_shape sh (0);
    sh << ink_point (x/PIXEL, y/PIXEL);
    sh << ink_point (x/PIXEL, y/PIXEL);
    shs << sh;
    refresh_last ();
    dragging= true;
  }
  else if (dragging && (type == "move" || type == "release-left")) {
    ink_shape& sh= shs [N(shs)-1];
    ink_point& p = sh [N(sh)-1];
    if (p->x != (x/PIXEL) || p->y != (y/PIXEL)) {
      sh << ink_point (x/PIXEL, y/PIXEL);
      refresh_last ();
    }
    dragging= (type == "move");
  }
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
ink_wk_widget () {
  return tm_new<ink_widget_rep> ();
}
