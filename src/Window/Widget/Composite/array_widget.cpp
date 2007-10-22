
/******************************************************************************
* MODULE     : array_widget.cpp
* DESCRIPTION: composite array widgets
*              If there is space left in a horizontal array,
*              then only the item named 'stretch_me' is stretched;
*              by default this is the last item of the array.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/composite_widget.hpp"
void abs_round (SI& l);

/******************************************************************************
* Horizontal arrays
******************************************************************************/

class horizontal_array_rep: public composite_widget_rep {
  int stretch_me;

public:
  horizontal_array_rep (array<widget> a, int stretch_me);
  horizontal_array_rep (array<widget> a, array<string> name, int stretch_me);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_find_child (find_child_event ev);
};

horizontal_array_rep::horizontal_array_rep (array<widget> a, int sm):
  composite_widget_rep (a), stretch_me (sm) {}

horizontal_array_rep::horizontal_array_rep (array<widget> a,
  array<string> name, int sm):
    composite_widget_rep (a, name), stretch_me (sm) {}

horizontal_array_rep::operator tree () {
  int i;
  tree t (TUPLE, N(a)+1);
  t[0]= "horizontal array";
  for (i=0; i<N(a); i++) t[i+1]= (tree) a[i];
  return t;
}

void
horizontal_array_rep::handle_get_size (get_size_event ev) {
  SI& w= ev->w;
  SI& h= ev->h;

  if (ev->mode==-1) {
    int i, hh=0;
    for (i=0; i<N(a); i++) {
      int www= w/N(a), hhh= h;
      a[i] << get_size (www, hhh, -1);
      hh= max (hh, hhh);
    }
    w= 0; h= hh;
  }
  else {
    int i, ww=0, hh=0;
    for (i=0; i<N(a); i++) {
      int www= w/N(a), hhh= h;
      a[i] << get_size (www, hhh,  1);
      ww= ww+ www;
      hh= max (hh, hhh);
    }
    if (ev->mode==0) { w= min (w, ww); h= min (h, hh); }
    else { w= ww; h= hh; }
  }
}

void
horizontal_array_rep::handle_position (position_event ev) {
  (void) ev;
  if (N(a)==0) return;

  int i;
  int j= (stretch_me==-1) ? N(a)-1: stretch_me;
  SI  tot_w= 0;
  SI  stretch_w= 0;
  for (i=0; i<N(a); i++) {
    SI the_w= w-tot_w, the_h= h;
    if (i==j) {
      a[i] << get_size (the_w, the_h, -1);
      stretch_w= the_w;
    }
    else a[i] << get_size (the_w, the_h);
    abs_round (the_w);
    tot_w += the_w;
  }

  SI  cur_w= 0;
  SI  extra= max (w- tot_w, 0);
  for (i=0; i<N(a); i++) {
    SI the_w= w-cur_w, the_h= h;
    if (i==j) the_w= stretch_w+ extra;
    else a[i] << get_size (the_w, the_h);
    abs_round (the_w);
    a[i] << emit_position (cur_w, 0, the_w, h);
    cur_w += the_w;
  }
}

void
horizontal_array_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  for (i=0; i<N(a); i++)
    if ((ev->x >= a[i]->x1()-ox) && (ev->x < a[i]->x2()-ox)) return;
  i= -1;
}

/******************************************************************************
* Interface
******************************************************************************/

widget
horizontal_array (array<widget> a, int stretch_me) {
  return new horizontal_array_rep (a, stretch_me);
}

widget
horizontal_array (array<widget> a, array<string> name, int sm) {
  return new horizontal_array_rep (a, name, sm);
}
