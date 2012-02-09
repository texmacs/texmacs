
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

#include "rectangles.hpp"
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
  SI curw1, curw2;
public:
  hsplit_widget_rep (array<wk_widget> a, array<string> names);
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_find_child (find_child_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

hsplit_widget_rep::hsplit_widget_rep (array<wk_widget> a, array<string> n):
  composite_widget_rep (a, n), bw (8*PIXEL), has_grip (false),
  curw1 (-1), curw2 (-1) {
    ASSERT (N(a) == 2, "invalid number of children"); }

hsplit_widget_rep::operator tree () {
  return tree (TUPLE, "horizontal split", (tree) a[0], (tree) a[1]);
}

void
hsplit_widget_rep::handle_get_size (get_size_event ev) {
  SI w1= (ev->w-bw)/2, h1= ev->h, w2= (ev->w-bw)/2, h2= ev->h;
  a[0] << get_size (w1, h1, ev->mode);
  a[1] << get_size (w2, h2, ev->mode);
  ev->w= w1 + w2 + bw;
  ev->h= max (max (h1, h2), 24*PIXEL);
}

void
hsplit_widget_rep::handle_position (position_event ev) {
  if (curw1 < 0 && curw2 < 0) {
    SI ew= w - bw;
    SI w1= ew, w2= ew, h1= h, h2= h;
    a[0] << get_size (w1, h1, 0);
    a[1] << get_size (w2, h2, 0);
    if (ew >= w1 + w2) {
      SI dw= ew - (w1 + w2);
      curw1= w1 + dw/2;
      curw2= w2 + dw/2;
    }
    else {
      SI dw= (w1 + w2) - ew;
      curw1= w1 - dw/2;
      curw2= w2 - dw/2;
    }
  }
  else {
    SI dw= w - (curw1 + curw2);
    curw1 += dw/2;
    curw2 += dw/2;
  }

  SI minw1= w-bw, minh1= h, minw2= w-bw, minh2= h;
  SI maxw1= w-bw, maxh1= h, maxw2= w-bw, maxh2= h;
  a[0] << get_size (minw1, minh1, -1);
  a[1] << get_size (minw2, minh2, -1);
  a[0] << get_size (maxw1, maxh1, 1);
  a[1] << get_size (maxw2, maxh2, 1);
  SI adjust= 0;
  if      (curw1 < minw1 && curw2 >= minw2)
    adjust=  min (minw1 - curw1, curw2 - minw2);
  else if (curw2 < minw2 && curw1 >= minw1)
    adjust= -min (minw2 - curw2, curw1 - minw1);
  else if (curw1 > maxw1 && curw2 <= maxw2)
    adjust= -min (curw1 - maxw1, maxw2 - curw2);
  else if (curw2 > maxw2 && curw1 <= maxw1)
    adjust=  min (curw2 - maxw2, maxw1 - curw1);
  curw1 += adjust;
  curw2 -= adjust;

  abs_round (curw1);
  abs_round (curw2);
  if (a[0]->ox != ox || a[0]->oy != oy || a[0]->w != curw1 || a[0]->h != h)
    a[0] << emit_position (0, 0, curw1, h);
  a[1] << emit_position (curw1 + bw, 0, curw1 + curw2 + bw, h);
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
  rectangles r1= rectangle (ev->x1, ev->y1, ev->x2, ev->y2);
  rectangles r2= rectangle (0, -h, w, 0);
  rectangles rs= r1 - r2;
  for (int i=0; i<N(rs); i++)
    layout_default (ren, rs[i]->x1, rs[i]->y1, rs[i]->x2, rs[i]->y2);
  SI lx= (a[0]->x2()-ox);
  SI rx= (a[1]->x1()-ox);
  SI xx= (lx+rx)/2;
  SI yy= -h/2;
  layout_default (ren, lx, ev->y1, rx, ev->y2);
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
      abs_round (w1);
      abs_round (w2);
      if (w1 >= minw1 && w1 <= maxw1 && w2 >= minw2 && w2 <= maxw2) {
        curw1= w1;
        curw2= w2;
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
  array<string> names (2);
  names[0]= "left"; names[1]= "right";
  return tm_new<hsplit_widget_rep> (a, names);
}
