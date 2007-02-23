
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
#include "display.hpp"

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
ps_device_rep::interrupted (bool check) {
  (void) check;
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
* Default property selection and rendering routines
******************************************************************************/

void
ps_device_rep::triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;
  polygon (x, y);
}

void
ps_device_rep::set_background_pattern (tree pat) {
  /*
  pattern= pat;
  if (is_atomic (pattern))
    set_background (current_display () -> get_color (pat->label));
  else if (is_func (pattern, PATTERN, 5))
    set_background (current_display () -> get_color (as_string (pattern[4])));
  */
}

bool is_percentage (tree t);
double as_percentage (tree t);

void
ps_device_rep::clear_pattern (SI x1, SI y1, SI x2, SI y2) {
  /*
  if (is_atomic (pattern))
    clear (x1, y1, x2, y2);
  else if (is_func (pattern, PATTERN)) {
    SI cx1, cy1, cx2, cy2;
    get_clipping (cx1, cy1, cx2, cy2);
    extra_clipping (x1, y1, x2, y2);
    url u= as_string (pattern[0]);
    SI w= x2 - x1, h= y2 - y1;
    if (is_int (pattern[1])) w= as_int (pattern[1]);
    else if (is_percentage (pattern[1]))
      w= (SI) (as_percentage (pattern[1]) * ((double) w));
    if (is_int (pattern[2])) h= as_int (pattern[2]);
    else if (is_percentage (pattern[2]))
      h= (SI) (as_percentage (pattern[2]) * ((double) h));
    SI sx= is_percentage (pattern[1])? 0: ox;
    SI sy= is_percentage (pattern[2])? 0: oy;
    for (int i= ((x1+sx)/w) - 1; i <= ((x2+sx)/w) + 1; i++)
      for (int j= ((y1+sy)/h) - 1; j <= ((y2+sy)/h) + 1; j++) {
	SI X1= i*w     - sx, Y1= j*h     - sy;
	SI X2= (i+1)*w - sx, Y2= (j+1)*h - sy;
	if (X1 < x2 && X2 > x1 && Y1 < y2 && Y2 > y1)
	  image (u, w, h, X1, Y1, 0.0, 0.0, 1.0, 1.0);
      }
    set_clipping (cx1, cy1, cx2, cy2, true);
  }
  */
}

#undef RND
