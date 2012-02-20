
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
#include "Scheme/object.hpp"
#include "poly_line.hpp"

/******************************************************************************
* Ink widget
******************************************************************************/

class ink_widget_rep: public attribute_widget_rep {
  command cb;
  contours shs;
  bool dragging;

public:
  ink_widget_rep (command cb);
  operator tree ();
  void refresh_last ();
  void commit ();
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

/******************************************************************************
* Routines for ink_widgets
******************************************************************************/

ink_widget_rep::ink_widget_rep (command cb2):
  attribute_widget_rep (), cb (cb2), shs (), dragging (false) {}

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
    poly_line sh= shs[i];
    int n= N(sh);
    if (n == 1) {
      array<SI> x (2);
      array<SI> y (2);
      x[0]= x[1]= (SI) (sh[0][0] * PIXEL);
      y[0]= y[1]= (SI) (sh[0][1] * PIXEL);
      ren->lines (x, y);
    }
    else if (n>1) {
      array<SI> x (n);
      array<SI> y (n);
      for (int j=0; j<n; j++) {
        x[j]= (SI) (sh[j][0] * PIXEL);
        y[j]= (SI) (sh[j][1] * PIXEL);
      }
      ren->lines (x, y);
    }
  }
  /*
  for (int i=0; i<N(shs); i++) {
    poly_line pl= shs[i];
    array<double> ts= vertices (pl);
    double len= length (pl);
    for (int j=0; j<N(ts); j++) {
      ren->set_color (red);
      point p= access (pl, ts[j] * len);
      SI x= p[0] * PIXEL;
      SI y= p[1] * PIXEL;
      ren->fill (x - 2*PIXEL, y - 2*PIXEL, x + 2*PIXEL, y + 2*PIXEL);
    }
  }
  */
}

void
ink_widget_rep::refresh_last () {
  if (N(shs) > 0) {
    poly_line& sh= shs [N(shs)-1];
    point& p = sh [max (0, N(sh)-2)];
    point& q = sh [N(sh)-1];
    SI x1= min (p[0], q[0]) * PIXEL;
    SI y1= min (p[1], q[1]) * PIXEL;
    SI x2= max (p[0], q[0]) * PIXEL;
    SI y2= max (p[1], q[1]) * PIXEL;
    this << emit_invalidate (x1 - 3*PIXEL, y1 - 3*PIXEL,
                             x2 + 3*PIXEL, y2 + 3*PIXEL);
  }
}

void
ink_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  bool   erase= ev->pressed ("right");

  //cout << type << ", " << x/PIXEL << ", " << y/PIXEL << "\n";
  if (erase) {
    int n= N(shs);
    contours nshs;
    for (int i=0; i<N(shs); i++)
      if (!nearby (point (x/PIXEL, y/PIXEL), shs[i]))
        nshs << shs[i];
    shs= nshs;
    if (N(nshs) != n) {
      this << emit_invalidate_all ();
      commit ();
    }
  }
  else if (type == "press-left") {
    poly_line sh (0);
    sh << point (x/PIXEL, y/PIXEL);
    shs << sh;
    refresh_last ();
    dragging= true;
  }
  else if (type == "leave" && (ev->x < 0 || ev->x >= w)) {
    if (ev->x >= w) cb (list_object (object (true)));
    shs= contours (0);
    this << emit_invalidate_all ();
    if (ev->x < 0) commit ();
  }
  else if (type == "move" || type == "release-left" || type == "leave")
    if (dragging && N(shs) > 0) {
      poly_line& sh= shs [N(shs)-1];
      point& p = sh [N(sh)-1];
      if (p[0] != (x/PIXEL) || p[1] != (y/PIXEL)) {
        sh << point (x/PIXEL, y/PIXEL);
        refresh_last ();
      }
      if (type != "move") {
        dragging= false;
        commit ();
      }
      this << emit_invalidate_all ();
    }
}

void
ink_widget_rep::commit () {
  object l= null_object ();
  for (int k= N(shs)-1; k>=0; k--) {
    poly_line sh= shs[k];
    object obj= null_object ();
    for (int i=N(sh)-1; i>=0; i--) {
      object p= list_object (object (sh[i][0]), object (sh[i][1]));
      obj= cons (p, obj);
    }
    l= cons (obj, l);
  }
  cb (list_object (l));
}

/******************************************************************************
* Learning glyphs
******************************************************************************/

array<contours>       learned_glyphs;
array<string>         learned_names;
array<array<tree> >   learned_disc1;
array<array<double> > learned_cont1;
array<array<tree> >   learned_disc2;
array<array<double> > learned_cont2;

void
register_glyph (string name, contours gl) {
  array<tree>   disc1;
  array<double> cont1;
  invariants (gl, 1, disc1, cont1);
  array<tree>   disc2;
  array<double> cont2;
  invariants (gl, 2, disc2, cont2);
  learned_names  << name;
  learned_glyphs << gl;
  learned_disc1  << disc1;
  learned_cont1  << cont1;
  learned_disc2  << disc2;
  learned_cont2  << cont2;
  //cout << "Added " << name << ", " << disc1 << "\n";
}

string
recognize_glyph (contours gl) {
  array<tree>   disc1;
  array<double> cont1;
  invariants (gl, 1, disc1, cont1);
  array<tree>   disc2;
  array<double> cont2;
  invariants (gl, 2, disc2, cont2);

  string best= "";
  int    best_i= -1;
  double best_rec= -100.0;
  for (int i=0; i<N(learned_names); i++)
    if (N(learned_glyphs[i]) == N(gl) && disc1 == learned_disc1[i]) {
      string        name= learned_names[i];
      array<double> cont= learned_cont1[i];
      double        dist= l2_norm (cont - cont1) / sqrt (N(cont1));
      double        rec = 1.0 - dist;
      if (rec > best_rec) { best_rec= rec; best= name; best_i= i; }
      //cout << name << ": " << 100.0 * rec << "%\n";
    }
  if (best != "") {
    //cout << "disc= " << disc1 << "\n";
    //cout << "cont= " << cont1 << "\n";
    return best;
  }

  for (int i=0; i<N(learned_names); i++)
    if (N(learned_glyphs[i]) == N(gl) && disc2 == learned_disc2[i]) {
      string        name= learned_names[i];
      array<double> cont= learned_cont2[i];
      double        dist= l2_norm (cont - cont2) / sqrt (N(cont2));
      double        rec = 1.0 - dist;
      if (rec > best_rec) { best_rec= rec; best= name; }
      //cout << name << ": " << 100.0 * rec << "%\n";
    }
  return best;
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
ink_wk_widget (command cb) {
  return tm_new<ink_widget_rep> (cb);
}
