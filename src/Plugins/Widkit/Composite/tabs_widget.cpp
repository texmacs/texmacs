
/******************************************************************************
* MODULE     : tabs_widget.cpp
* DESCRIPTION: display one among several tabs
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/composite_widget.hpp"
#include "Widkit/layout.hpp"

#define THIS (wk_widget (this))

void abs_round (SI& l);

/******************************************************************************
* Horizontal lists
******************************************************************************/

class tabs_widget_rep: public composite_widget_rep {
public:
  array<SI> xs;
  SI h1, h2;
  tabs_widget_rep (array<wk_widget> a, array<wk_widget> b);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  void handle_find_child (find_child_event ev);
  void handle_mouse (mouse_event ev);
};

array<wk_widget>
tabs_make (array<wk_widget> a, array<wk_widget> b) {
  array<wk_widget> copy_a= copy (a);
  array<string> names;
  for (int i=0; i<N(a); i++) names << as_string (i);
  copy_a << switch_widget (b, names, 0);
  return copy_a;
}

tabs_widget_rep::tabs_widget_rep (array<wk_widget> a, array<wk_widget> b):
  composite_widget_rep (tabs_make (a, b), south_west),
  xs (), h1 (0), h2 (0) {}

tabs_widget_rep::operator tree () {
  int i;
  tree t (TUPLE, N(a)+1);
  t[0]= "tabs";
  for (i=0; i<N(a); i++) t[i+1]= (tree) a[i];
  return t;
}

void
tabs_widget_rep::handle_get_size (get_size_event ev) {
  SI& w= ev->w;
  SI& h= ev->h;

  int i, l=N(a)-1, ww=0, hh=0;
  for (i=0; i<l; i++) {
    int www= w, hhh= h;
    a[i] << get_size (www, hhh, -1);
    ww= ww+ www + 2*PIXEL;
    hh= max (hh, hhh);
  }
  int www= w, hhh= h - hh;
  a[l] << get_size (www, hhh, ev->mode);
  w= max (ww, www); h= hh + hhh + 3*PIXEL;
}

void
tabs_widget_rep::handle_position (position_event ev) {
  (void) ev;

  int i, l= N(a)-1, tot_w= 0, max_h= 0;
  for (i=0; i<l; i++) {
    SI the_w= w, the_h= h;
    a[i] << get_size (the_w, the_h, -1);
    abs_round (the_w);
    abs_round (the_h);
    tot_w += the_w + 2*PIXEL;
    max_h= max (max_h, the_h);
  }
  SI last_w= w, last_h= h;
  a[l] << get_size (last_w, last_h, 0);
  abs_round (last_w);
  abs_round (last_h);
  SI main_w= max (tot_w, last_w);
  SI main_h= max_h + last_h;
  abs_round (main_w);
  abs_round (main_h);
  xs= array<SI> (); xs << 0;
  int cur_w= 0;
  for (i=0; i<l; i++) {
    SI the_w= w, the_h= h;
    a[i] << get_size (the_w, the_h, -1);
    abs_round (the_w);
    abs_round (the_h);
    a[i] << emit_position (cur_w + PIXEL, last_h + 2*PIXEL,
                           the_w, max_h, south_west);
    cur_w += the_w + 2*PIXEL;
    xs << cur_w;
  }
  a[l] << emit_position (0, 0, w, last_h, south_west);
  h1= last_h; h2= max_h;
}

void
tabs_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  layout_default (ren, 0, 0, w, h);
  if (h1 == 0 || h2 == 0 || N(xs) == 0) return;

  int l= N(a)-1, focus= 0;
  a[l] << get_integer ("switch", focus);
  color pastel= layout_pastel (ren);
  color dark= layout_dark (ren);
  SI fx1= xs[focus] - PIXEL;
  SI fx2= xs[focus+1] - PIXEL;
  SI top= h1 + h2 + 2*PIXEL;
  SI lim= xs[N(xs)-1];

  ren->set_line_style (PIXEL);
  ren->set_color (pastel);
  if (focus > 0)
    ren->line (0, h1, fx1, h1);
  ren->line (fx2, h1, w, h1);
  ren->set_color (dark);
  if (focus > 0)
    ren->line (0, h1 + PIXEL, fx1, h1 + PIXEL);
  ren->line (fx2, h1 + PIXEL, w, h1 + PIXEL);
  ren->set_color (dark);
  ren->line (0, top, lim - PIXEL, top);
  for (int i=0; i<l; i++) {
    ren->set_color (pastel);
    ren->line (xs[i], h1+2*PIXEL, xs[i], top - PIXEL);
    ren->set_color (dark);
    ren->line (xs[i+1]-PIXEL, h1+2*PIXEL, xs[i+1]-PIXEL, top - PIXEL);
  }
}

void
tabs_widget_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  int l= N(a) - 1;
  if (ev->y < a[l]->y2() - oy) {
    i=l; return; }
  for (i=0; i<l; i++)
    if ((ev->x >= a[i]->x1()-ox) && (ev->x < a[i]->x2()-ox)) return;
  i= -1;
}

void
tabs_widget_rep::handle_mouse (mouse_event ev) {
  int    l= N(a) - 1;
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  int    focus;
  THIS << emit_find_child (x, y, focus);
  if (focus >= 0 && focus < l &&
      (type == "release-left" || type == "release-right")) {
    a[l] << set_integer ("switch", focus);
    THIS << emit_update ();
    THIS << emit_reposition ();
    refresh_size (win);
  }
  else composite_widget_rep::handle_mouse (ev);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
tabs_widget (array<wk_widget> a, array<wk_widget> b) {
  return tm_new<tabs_widget_rep> (a, b);
}
