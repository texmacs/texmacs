
/******************************************************************************
* MODULE     : composite_widget.cpp
* DESCRIPTION: composite list widgets
*              If there is space left in a list widget,
*              then all items are stretched proportionally
*              according to their sizes.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/composite_widget.hpp"

void abs_round (SI& l);

/******************************************************************************
* Horizontal lists
******************************************************************************/

class horizontal_list_rep: public composite_widget_rep {
public:
  horizontal_list_rep (array<wk_widget> a);
  horizontal_list_rep (array<wk_widget> a, array<string> name);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_find_child (find_child_event ev);
};

horizontal_list_rep::horizontal_list_rep (array<wk_widget> a):
  composite_widget_rep (a) {}

horizontal_list_rep::horizontal_list_rep (array<wk_widget> a,
  array<string> n):
  composite_widget_rep (a, n) {}

horizontal_list_rep::operator tree () {
  int i;
  tree t (TUPLE, N(a)+1);
  t[0]= "horizontal list";
  for (i=0; i<N(a); i++) t[i+1]= (tree) a[i];
  return t;
}

void
horizontal_list_rep::handle_get_size (get_size_event ev) {
  SI& w= ev->w;
  SI& h= ev->h;

  if (ev->mode==0) {
    SI ww= w, hh= h;
    this << get_size (ww, hh, 1);
    w= min (w, ww);
    h= min (h, hh);
    ww= w; hh= h;
    this << get_size (ww, hh, -1);
    w= max (w, ww);
    h= hh;
  }
  else {
    int i, ww=0, hh=0;
    for (i=0; i<N(a); i++) {
      int www= w/N(a), hhh= h;
      a[i] << get_size (www, hhh, ev->mode);
      ww= ww+ www;
      hh= max (hh, hhh);
    }
    w= ww; h= hh;
  }
}

void
horizontal_list_rep::handle_position (position_event ev) {
  (void) ev;
  if (N(a)==0) return;

  SI min_w=w, min_h= h;
  this << get_size (min_w, min_h, -1);
  SI max_w=w, max_h= h;
  this << get_size (max_w, max_h,  1);
  double stretch;
  if ((max_w==min_w) || (w<min_w)) stretch= 0.0;
  else if (w>max_w) stretch=1.0;
  else stretch= ((double) (w-min_w))/((double) (max_w-min_w));

  int i;
  SI  cur_w=0;
  for (i=0; i<N(a); i++) {
    SI the_w, the_h= h;
    if (i<N(a)-1) {
      min_w= w/N(a), min_h= h;
      a[i] << get_size (min_w, min_h, -1);
      max_w= w/N(a), max_h= h;
      a[i] << get_size (max_w, max_h,  1);
      the_w= (SI) (min_w+ stretch* (max_w- min_w));
    }
    else the_w= w- cur_w;
    abs_round (the_w);
    a[i] << emit_position (cur_w, 0, the_w, the_h);
    cur_w+=the_w;
  }
}

void
horizontal_list_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  for (i=0; i<N(a); i++)
    if ((ev->x >= a[i]->x1()-ox) && (ev->x < a[i]->x2()-ox)) return;
  i= -1;
}

/******************************************************************************
* Vertical lists
******************************************************************************/

class vertical_list_rep: public composite_widget_rep {
  bool menu_flag;
public:
  vertical_list_rep (array<wk_widget> a, bool mf= false);
  vertical_list_rep (array<wk_widget> a, array<string> name);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_find_child (find_child_event ev);
};

vertical_list_rep::vertical_list_rep (array<wk_widget> a, bool mf):
  composite_widget_rep (a), menu_flag (mf) {}

vertical_list_rep::vertical_list_rep (array<wk_widget> a,
  array<string> name):
    composite_widget_rep (a, name), menu_flag (false) {}

vertical_list_rep::operator tree () {
  int i;
  tree t (TUPLE, N(a)+1);
  t[0]= "vertical list";
  for (i=0; i<N(a); i++) t[i+1]= (tree) a[i];
  return t;
}

void
vertical_list_rep::handle_get_size (get_size_event ev) {
  SI& w= ev->w;
  SI& h= ev->h;

  if (menu_flag) {
      int i;
      SI m1=0, m2=0, c1=0, c2=0;
      for (i=0; i<N(a); i++) {
	a[i] << get_coord2 ("extra width", c1, c2);
	m1= max (m1, c1); m2= max (m2, c2);
      }
      for (i=0; i<N(a); i++)
	a[i] << set_coord2 ("extra width", m1, m2);
    }

  if (ev->mode==0) {
    SI ww= w, hh= h;
    this << get_size (ww, hh, 1);
    w= min (w, ww);
    h= min (h, hh);
    ww= w; hh= h;
    this << get_size (ww, hh, -1);
    w= ww;
    h= max (h, hh);
  }
  else {
    int i, ww=0, hh=0;
    for (i=0; i<N(a); i++) {
      int www= w, hhh= h/N(a);
      a[i] << get_size (www, hhh, ev->mode);
      ww= max (ww, www);
      hh= hh+ hhh;
    }
    w= ww; h= hh;
  }
}

void
vertical_list_rep::handle_position (position_event ev) {
  (void) ev;
  if (N(a)==0) return;

  SI min_w=w, min_h= h;
  this << get_size (min_w, min_h, -1);
  SI max_w=w, max_h= h;
  this << get_size (max_w, max_h,  1);
  double stretch;
  if ((max_h==min_h) || (h<min_h)) stretch= 0.0;
  else if (h>max_h) stretch=1.0;
  else stretch= ((double) (h-min_h))/((double) (max_h-min_h));

  int i;
  SI  cur_h=0;
  for (i=0; i<N(a); i++) {
    SI the_w= w, the_h;
    //if (i<N(a)-1) {
    min_w= w, min_h= h/N(a);
    a[i] << get_size (min_w, min_h, -1);
    max_w= w, max_h= h/N(a);
    a[i] << get_size (max_w, max_h,  1);
    the_h= (SI) (min_h+ stretch* (max_h- min_h));
    //}
    //else the_h= h+ cur_h;
    abs_round (the_h);
    a[i] << emit_position (0, cur_h, the_w, the_h);
    cur_h-=the_h;
  }
}

void
vertical_list_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  for (i=0; i<N(a); i++)
    if ((ev->y >= a[i]->y1()-oy) && (ev->y < a[i]->y2()-oy)) return;
  i= -1;
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
horizontal_list (array<wk_widget> a) {
  return tm_new<horizontal_list_rep> (a);
}

wk_widget
horizontal_list (array<wk_widget> a, array<string> name) {
  return tm_new<horizontal_list_rep> (a, name);
}

wk_widget
vertical_list (array<wk_widget> a) {
  return tm_new<vertical_list_rep> (a);
}

wk_widget
vertical_list (array<wk_widget> a, array<string> name) {
  return tm_new<vertical_list_rep> (a, name);
}

wk_widget
vertical_menu (array<wk_widget> a) {
  return tm_new<vertical_list_rep> (a, true);
}
