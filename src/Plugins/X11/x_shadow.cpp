
/******************************************************************************
* MODULE     : x_shadow.cpp
* DESCRIPTION: Off-screen drawables under X
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "X11/x_window.hpp"

/******************************************************************************
* Copying regions
******************************************************************************/

void
x_drawable_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  ASSERT (ren != NULL, "invalid situation");
  if (ren->is_printer ()) return;
  x_drawable_rep* src= (x_drawable_rep*) ren->get_handle ();
  if (src->win == win && x1 == x && y1 == y) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (X1, Y1);
  decode (x1, y1);
  decode (x2, y2);
  src->decode (x, y);
  x += x1 - X1;
  y += y1 - Y1;
  if (x1<x2 && y2<y1)
    XCopyArea (dpy, src->win, win, gc, x, y, x2-x1, y1-y2, x1, y2);
}

/******************************************************************************
* Shadowing
******************************************************************************/

void
x_drawable_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  ((renderer) this) -> get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw == mw && sh == mh) return;
    //cout << "Old: " << sw << ", " << sh << "\n";
    delete_shadow (ren);
  }
  //cout << "Create " << mw << ", " << mh << "\n";
  ren= (renderer) tm_new<x_drawable_rep> (gui, mw, mh);
}

void
x_drawable_rep::delete_shadow (renderer& ren) {
  if (ren != NULL) {
    tm_delete ((x_drawable_rep*) ren->get_handle ());
    ren= NULL;
  }
}

void
x_drawable_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  x_drawable_rep* shadow= (x_drawable_rep*) ren->get_handle ();
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  shadow->ox= ox;
  shadow->oy= oy;
  shadow->cx1= x1+ ox;
  shadow->cy1= y1+ oy;
  shadow->cx2= x2+ ox;
  shadow->cy2= y2+ oy;
  shadow->master= this;
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1)
    XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
}

void
x_drawable_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  x_drawable_rep* shadow= (x_drawable_rep*) ren->get_handle ();
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1)
    XCopyArea (dpy, shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
}

void
x_drawable_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  if (master == NULL) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  x_drawable_rep* xren= (x_drawable_rep*) master->get_handle ();
  xren->encode (x1, y1);
  xren->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}
