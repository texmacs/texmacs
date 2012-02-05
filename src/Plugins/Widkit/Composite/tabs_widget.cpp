
/******************************************************************************
* MODULE     : tabs_widget.cpp
* DESCRIPTION: display one among several tabs
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/composite_widget.hpp"
#include "window.hpp"

#define THIS (wk_widget (this))

void abs_round (SI& l);

/******************************************************************************
* Horizontal lists
******************************************************************************/

class tabs_widget_rep: public composite_widget_rep {
public:
  tabs_widget_rep (array<wk_widget> a, array<wk_widget> b);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
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
  composite_widget_rep (tabs_make (a, b), south_west) {}

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
    ww= ww+ www;
    hh= max (hh, hhh);
  }
  int www= w, hhh= h - hh;
  a[l] << get_size (www, hhh, ev->mode);
  w= max (ww, www); h= hh + hhh;
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
    tot_w += the_w;
    max_h= max (max_h, the_h);
  }
  SI last_w= w, last_h= h;
  a[l] << get_size (last_w, last_h, 0);
  SI main_w= max (tot_w, last_w);
  SI main_h= max_h + last_h;
  abs_round (main_w);
  abs_round (main_h);
  int cur_w= 0;
  for (i=0; i<l; i++) {
    SI the_w= w, the_h= h;
    a[i] << get_size (the_w, the_h, -1);
    abs_round (the_w);
    abs_round (the_h);
    a[i] << emit_position (cur_w, last_h, the_w, max_h, south_west);
    cur_w += the_w;
  }
  a[l] << emit_position (0, 0, main_w, last_h, south_west);
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
