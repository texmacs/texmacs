
/******************************************************************************
* MODULE     : scrollable_widget.cpp
* DESCRIPTION: Scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "message.hpp"
#include "Widkit/scroll_widget.hpp"
#include "Widkit/layout.hpp"

SI get_dx (gravity grav, int w);
SI get_dy (gravity grav, int h);

inline bool is_nil (wk_widget_rep* x) { return x==NULL; }

/******************************************************************************
* Routines for scrollable widgets
******************************************************************************/

scrollable_widget_rep::scrollable_widget_rep (
  wk_widget child, gravity grav):
    scroll_widget_rep (1, grav)
{
  a[0]= child;
  scx= 0; scy= 0;
  ex1= 0; ey1= 0;
  ex2= 0; ey2= 0;
}

scrollable_widget_rep::operator tree () {
  return tree (TUPLE, "scrollable", (tree) a[0]);
}

void
scrollable_widget_rep::scroll_event_hor (SI& x, SI& bef, SI& af) {
  abs_round (x);
  if ((x + x1() - ox) < ex1) x = ex1 - x1() + ox;
  if ((x + x2() - ox) > ex2) x = ex2 - x2() + ox;

  if (attached ()) {
    int dx= max (-w, min (w, x- scx));
    if ((dx>-w) && (dx<w) && (dx!=0)) {
      win->translate (x1(), y1(), x2(), y2(), -dx, 0);
    }
    if (dx>0) this << emit_invalidate (x2()-ox-dx, y1()-oy, x2()-ox, y2()-oy);
    if (dx<0) this << emit_invalidate (x1()-ox, y1()-oy, x1()-ox-dx, y2()-oy);
  }

  scx      = x;
  bef      = ox- x1();
  af       = x2()- ox;
  a[0]->ox = ox- scx;
}

void
scrollable_widget_rep::scroll_event_ver (SI& y, SI& bef, SI& af) {
  abs_round (y);
  if ((y + y1() - oy) < ey1) y = ey1 - y1() + oy;
  if ((y + y2() - oy) > ey2) y = ey2 - y2() + oy;

  if (attached ()) {
    int dy= max (-h, min (h, y- scy));
    if ((dy>-h) && (dy<h) && (dy!=0)) {
      win->translate (x1(), y1(), x2(), y2(), 0, -dy);
    }
    if (dy>0) this << emit_invalidate (x1()-ox, y2()-oy-dy, x2()-ox, y2()-oy);
    if (dy<0) this << emit_invalidate (x1()-ox, y1()-oy, x2()-ox, y1()-oy-dy);
  }

  scy      = y;
  bef      = oy- y1();
  af       = y2()- oy;
  a[0]->oy = oy- scy;
}

void
scrollable_widget_rep::scroll_to (SI scx2, SI scy2) {
  if ((scx2 != scx) && (!is_nil (hor))) hor << emit_bar_scroll_to (scx2);
  if ((scy2 != scy) && (!is_nil (ver))) ver << emit_bar_scroll_to (scy2);
}

void
scrollable_widget_rep::set_extents (SI ex1b, SI ey1b, SI ex2b, SI ey2b) {
  ex1= ex1b; ey1= ey1b; ex2= ex2b; ey2= ey2b;
  abs_outer_round (ex1, ey1, ex2, ey2);
  if (is_nil (hor)) scx = ex1- (x1()- ox);
  if (is_nil (ver)) scy = ey1- (y1()- oy);
  a[0]->ox = ox -scx; a[0]->oy = oy -scy ;
  a[0]->w  = ex2-ex1; a[0]->h  = ey2-ey1;
  if (attached ()) this << emit_invalidate_all ();
  if (!is_nil (hor)) hor << emit_bar_set_extents (ex1, ex2);
  if (!is_nil (ver)) ver << emit_bar_set_extents (ey1, ey2);
}

/******************************************************************************
* Event handlers
******************************************************************************/

void
scrollable_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode==-1) {
    ev->w= 8*PIXEL;
    ev->h= 8*PIXEL;
  }
  if (ev->mode== 1) gui_maximal_extents (ev->w, ev->h);
}

void
scrollable_widget_rep::handle_position (position_event ev) { (void) ev;
  set_extents (ex1, ey1, ex2, ey2);
  a[0] << emit_reposition ();
}

void
scrollable_widget_rep::handle_set_widget (set_widget_event ev) {
  if      (ev->which == "hor-bar") hor= ev->w.rep;
  else if (ev->which == "ver-bar") ver= ev->w.rep;
  else attribute_widget_rep::handle_set_widget (ev);
}

void
scrollable_widget_rep::handle_get_coord1 (get_coord1_event ev) {
  if      (ev->which == "width") ev->c1= w;
  else if (ev->which == "height") ev->c1= h;
  else attribute_widget_rep::handle_get_coord1 (ev);
}

void
scrollable_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if      (ev->which == "scroll position") {
    ev->c1= scx; ev->c2= scy; }
  else if (ev->which == "extra width") {
    ev->c1= 0; ev->c2= 0; }
  else attribute_widget_rep::handle_get_coord2 (ev);
}

void
scrollable_widget_rep::handle_get_coord4 (get_coord4_event ev) {
  if      (ev->which == "visible") {
    ev->c1= scx - get_dx (grav, w);
    ev->c2= scy - get_dy (grav, h) - h;
    ev->c3= scx - get_dx (grav, w) + w;
    ev->c4= scy - get_dy (grav, h);
  }
  else if (ev->which == "extents") {
    ev->c1= ex1; ev->c2= ey1;
    ev->c3= ex2; ev->c4= ey2;
  }
  else attribute_widget_rep::handle_get_coord4 (ev);
}

void
scrollable_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if      (ev->which == "scroll position")
    scroll_to (ev->c1, ev->c2);
  else if (ev->which == "extra width");
  else attribute_widget_rep::handle_set_coord2 (ev);
}

void
scrollable_widget_rep::handle_set_coord4 (set_coord4_event ev) {
  if (ev->which == "extents")
    set_extents (ev->c1, ev->c2, ev->c3, ev->c4);
  else attribute_widget_rep::handle_set_coord4 (ev);
}

void
scrollable_widget_rep::handle_scroll (scroll_event ev) {
  if      (ev->which == "hor-bar")
    scroll_event_hor (ev->c1, ev->c2, ev->c3);
  else if (ev->which == "ver-bar")
    scroll_event_ver (ev->c1, ev->c2, ev->c3);
  else { WK_FAILED ("invalid scroll"); }
}

void
scrollable_widget_rep::handle_set_integer (set_integer_event ev) {
  attribute_widget_rep::handle_set_integer (ev);
}

void
scrollable_widget_rep::handle_repaint (repaint_event ev) {
  renderer ren= ev->win;
  SI mx1= x1() - ox;
  SI my1= y1() - oy;
  SI mx2= x2() - ox;
  SI my2= y2() - oy;
  SI sx1= a[0]->x1() - ox;
  SI sy1= a[0]->y1() - oy;
  SI sx2= a[0]->x2() - ox;
  SI sy2= a[0]->y2() - oy;
  color bg= tm_background; // layout_light (ren);
  ren->set_background (bg);
  ren->set_pencil (bg);
  if (ev->x1 < sx1) ren->fill (ev->x1, my1, sx1, my2);
  if (ev->x2 > sx2) ren->fill (sx2, my1, ev->x2, my2);
  if (ev->y1 < sy1) ren->fill (mx1, ev->y1, mx2, sy1);
  if (ev->y2 > sy2) ren->fill (mx1, sy2, mx2, ev->y2);
  basic_widget_rep::handle_repaint (ev);
}
