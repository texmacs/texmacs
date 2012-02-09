
/******************************************************************************
* MODULE     : split_widget.cpp
* DESCRIPTION: widgets which are separated into two parts and
*              whose separating border can be adjusted using the mouse
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/composite_widget.hpp"
#include "Widkit/layout.hpp"

void abs_round (SI& l);

/******************************************************************************
* Horizontal splits
******************************************************************************/

class hsplit_widget_rep: public composite_widget_rep {
  SI bw;
  bool has_grip;
public:
  hsplit_widget_rep (array<wk_widget> a);
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_find_child (find_child_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

hsplit_widget_rep::hsplit_widget_rep (array<wk_widget> a):
  composite_widget_rep (a), bw (8*PIXEL), has_grip (false) {
    ASSERT (N(a) == 2, "invalid number of children"); }

hsplit_widget_rep::operator tree () {
  return tree (TUPLE, "horizontal split", (tree) a[0], (tree) a[1]);
}

void
hsplit_widget_rep::handle_get_size (get_size_event ev) {
  SI w1= (ev->w-bw)/2, h1= ev->h, w2= (ev->w-bw)/2, h2= ev->h;
  a[0] << get_size (w1, h1, ev->type);
  a[1] << get_size (w2, h2, ev->type);
  ev->w= w1 + w2 + bw;
  ev->h= max (max (h1, h2), 24*PIXEL);
}

void
hsplit_widget_rep::handle_position (position_event ev) {
  SI ew= w - bw;
  SI w1= ew, w2= ew, h1= h, h2= h;
  a[0] << get_size (w1, h1, 0);
  a[1] << get_size (w2, h2, 0);
  SI fw1, fw2;
  if (ew >= w1 + w2) {
    SI dw= ew - (w1 + w2);
    SI mw1= ew, mw2= ew, mh1= h, mh2= h;
    a[0] << get_size (mw1, mh1, 1);
    a[1] << get_size (mw2, mh2, 1);
    SI dw1= mw1 - w1, dw2= mw2 - w2;
    fw1= w1 + (dw * dw1) / (dw1 + dw2);
    fw2= w2 + (dw * dw2) / (dw1 + dw2);
  }
  else {
    SI dw= (w1 + w2) - ew;
    SI mw1= ew, mw2= ew, mh1= h, mh2= h;
    a[0] << get_size (mw1, mh1, -1);
    a[1] << get_size (mw2, mh2, -1);
    SI dw1= w1 - mw1, dw2= w2 - mw2;
    fw1= w1 - (dw * dw1) / (dw1 + dw2);
    fw2= w2 - (dw * dw2) / (dw1 + dw2);
  }
  abs_round (fw1);
  abs_round (fw2);
  a[0] << emit_position (0, 0, fw1, h);
  a[1] << emit_position (fw1 + bw, 0, fw1 + fw2 + bw, h);
}

void
hsplit_widget_rep::handle_find_child (find_child_event ev) {
  int& i= ev->which;
  for (i=0; i<N(a); i++)
    if ((ev->x >= a[i]->x1()-ox) && (ev->x < a[i]->x2()-ox)) return;
  i= -1;
}

void
hsplit_widget_rep::handle_repaint (repaint_event ev) {
  renderer ren= win->get_renderer ();
  layout_default (ren, ev->x1, ev->y1, ev->x2, ev->y2);
  SI xx= (a[1]->x1()-ox) - (bw/2);
  SI yy= -h/2;
  layout_lower (ren, xx - 2*PIXEL, yy - 6*PIXEL, xx + PIXEL, yy + 6*PIXEL);
  basic_widget_rep::handle_repaint (ev);
}

void
hsplit_widget_rep::handle_mouse (mouse_event ev) {
  bool mid= (ev->x >= (a[0]->x2()-ox)) && (ev->x < (a[1]->x1()-ox));
  if (has_grip) {
    if ((!mid && ev->type == "move") || ev->type == "release-left") {
      SI minw1= w-bw, minh1= h, minw2= w-bw, minh2= h;
      SI maxw1= w-bw, maxh1= h, maxw2= w-bw, maxh2= h;
      a[0] << get_size (minw1, minh1, -1);
      a[1] << get_size (minw2, minh2, -1);
      a[0] << get_size (maxw1, maxh1, 1);
      a[1] << get_size (maxw2, maxh2, 1);
      SI w1= ev->x - bw/2;
      SI w2= w - bw - w1;
      if (w1 >= minw1 && w1 <= maxw1 && w2 >= minw2 && w2 <= maxw2) {
        a[0] << emit_position (0, 0, w1, h);
        a[1] << emit_position (w1 + bw, 0, w1 + w2 + bw, h);
        this << emit_invalidate_all ();
      }
    }
    if (ev->type == "release-left") {
      has_grip= false;
      wk_ungrab_pointer (this);
    }
  }
  else if (!mid)
    basic_widget_rep::handle_mouse (ev);
  else if (ev->type == "press-left") {
    has_grip= true;
    wk_grab_pointer (this);
  }
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
hsplit_widget (wk_widget l, wk_widget r) {
  array<wk_widget> a (2);
  a[0]= l; a[1]= r;
  return tm_new<hsplit_widget_rep> (a);
}
