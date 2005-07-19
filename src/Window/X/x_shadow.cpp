
/******************************************************************************
* MODULE     : x_shadow.cpp
* DESCRIPTION: Off-screen drawables under X
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "X/x_window.hpp"

/******************************************************************************
* Copying regions
******************************************************************************/

void
x_drawable_rep::fetch (SI x1, SI y1, SI x2, SI y2, ps_device dev, SI x, SI y) {
  if (dev == NULL) fatal_error ("invalid situation", "x_drawable_rep::fetch");
  if (dev->is_printer ()) return;
  x_drawable_rep* src= dev->as_x_drawable ();
  if (src->win == win && x1 == x && y1 == y) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  src->decode (X1, Y1);
  src->decode (x1, y1);
  src->decode (x2, y2);
  decode (x, y);
  x += x1 - X1;
  y += y2 - Y1;
  XCopyArea (dpy, src->win, win, gc, x, y, x2-x1, y1-y2, x1, y2);
  event_status= event_status || src->event_status;
}

/******************************************************************************
* Shadowing
******************************************************************************/

void
x_drawable_rep::new_shadow (ps_device& dev) {
  SI mw, mh, sw, sh;
  ((ps_device) this) -> get_extents (mw, mh);
  if (dev != NULL) {
    dev->get_extents (sw, sh);
    if (sw == mw && sh == mh) return;
    //cout << "Old: " << sw << ", " << sh << "\n";
    delete_shadow (dev);
  }
  //cout << "Create " << mw << ", " << mh << "\n";
  dev= (ps_device) new x_drawable_rep (dis, mw, mh);
}

void
x_drawable_rep::delete_shadow (ps_device& dev) {
  if (dev != NULL) {
    delete dev->as_x_drawable ();
    dev= NULL;
  }
}

void
x_drawable_rep::get_shadow (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  if (dev == NULL) fatal_error ("null device", "x_drawable_rep::get_shadow");
  if (dev->is_printer ()) return;
  x_drawable_rep* shadow= dev->as_x_drawable ();
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
  shadow->event_status= event_status;
  shadow->master= this;
  decode (x1, y1);
  decode (x2, y2);
  XCopyArea (dpy, win, shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
}

void
x_drawable_rep::put_shadow (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  if (dev == NULL) fatal_error ("null device", "x_drawable_rep::put_shadow");
  if (dev->is_printer ()) return;
  x_drawable_rep* shadow= dev->as_x_drawable ();
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  XCopyArea (dpy, shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
}

void
x_drawable_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  if (master == NULL) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  master->as_x_drawable()->encode (x1, y1);
  master->as_x_drawable()->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}
