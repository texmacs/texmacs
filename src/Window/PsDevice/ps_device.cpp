
/******************************************************************************
* MODULE     : ps_device.cpp
* DESCRIPTION: Abstract device for printing post-script graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "ps_device.hpp"

/******************************************************************************
* Constructors
******************************************************************************/

ps_device_rep::ps_device_rep ():
  ox (0), oy (0), cx1 (0), cy1 (0), cx2 (0), cy2 (0),
  sfactor (1), pixel (PIXEL), thicken (0), master (NULL) {}

ps_device_rep::~ps_device_rep () {}

/******************************************************************************
* Device specific
******************************************************************************/

bool
ps_device_rep::is_printer () {
  return false;
}

bool
ps_device_rep::is_x_drawable () {
  return false;
}

void
ps_device_rep::get_extents (int& w, int& h) {
  w= h= 0;
}

x_drawable_rep*
ps_device_rep::as_x_drawable () {
  return NULL;
}

void
ps_device_rep::next_page () {
}

bool
ps_device_rep::check_event (int type) {
  (void) type;
  return false;
}

/******************************************************************************
* Origin and shrinking factor
******************************************************************************/

void
ps_device_rep::set_origin (SI x, SI y) {
  ox= x;
  oy= y;
}

void
ps_device_rep::move_origin (SI dx, SI dy) {
  ox += dx;
  oy += dy;
}

void
ps_device_rep::set_shrinking_factor (int sf) {
  ox  /= sfactor; oy  /= sfactor;
  cx1 /= sfactor; cy1 /= sfactor;
  cx2 /= sfactor; cy2 /= sfactor;
  sfactor= sf;
  pixel  = sf*PIXEL;
  thicken= (sf>>1)*PIXEL;
  ox  *= sfactor; oy  *= sfactor;
  cx1 *= sfactor; cy1 *= sfactor;
  cx2 *= sfactor; cy2 *= sfactor;
}

/******************************************************************************
* Clipping
******************************************************************************/

void
ps_device_rep::get_clipping (SI &x1, SI &y1, SI &x2, SI &y2) {
  x1= cx1- ox; y1= cy1- oy;
  x2= cx2- ox; y2= cy2- oy;
}

void
ps_device_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  cx1= x1+ ox; cy1= y1+ oy;
  cx2= x2+ ox; cy2= y2+ oy;
}

void
ps_device_rep::extra_clipping (SI x1, SI y1, SI x2, SI y2) {
  SI ox1, oy1, ox2, oy2;
  get_clipping (ox1, oy1, ox2, oy2);
  x1= max (x1, ox1); y1= max (y1, oy1);
  x2= max (x1, min (x2, ox2)); y2= max (y1, min (y2, oy2));
  set_clipping (x1, y1, x2, y2);
}

bool
ps_device_rep::is_visible (SI x1, SI y1, SI x2, SI y2) {
  return
    (x2 >= (cx1- ox)) && (y2 >= (cy1- oy)) &&
    (x1 <  (cx2- ox)) && (y1 <  (cy2- oy));
}

#define RND(x) (((x)>=0)?(((x)/pixel)*pixel):((((x)-pixel+1)/pixel)*pixel))

void
ps_device_rep::round (SI& x, SI& y) {
  x= RND (x+ ox)- ox;
  y= RND (y+ oy)- oy;
}

void
ps_device_rep::inner_round (SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= RND (x1+ox+pixel-1) - ox;
  y1= RND (y1+oy+pixel-1) - oy;
  x2= RND (x2+ox) - ox;
  y2= RND (y2+oy) - oy;
}

void
ps_device_rep::outer_round (SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= RND (x1+ox) - ox;
  y1= RND (y1+oy) - oy;
  x2= RND (x2+ox+pixel-1) - ox;
  y2= RND (y2+oy+pixel-1) - oy;
}

#undef RND

#define RND(x) (((x)>=0)?(((x)/PIXEL)*PIXEL):((((x)-PIXEL+1)/PIXEL)*PIXEL))

void
abs_round (SI& l) {
  l= RND (l);
}

void
abs_round (SI& x, SI& y) {
  x= RND (x);
  y= RND (y);
}

void
abs_inner_round (SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= RND (x1+PIXEL-1);
  y1= RND (y1+PIXEL-1);
  x2= RND (x2);
  y2= RND (y2);
}

void
abs_outer_round (SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= RND (x1);
  y1= RND (y1);
  x2= RND (x2+PIXEL-1);
  y2= RND (y2+PIXEL-1);
}

/******************************************************************************
* Default rendering routines
******************************************************************************/

void
ps_device_rep::triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;
  polygon (x, y);
}

#undef RND
