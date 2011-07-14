
/******************************************************************************
* MODULE     : renderer.cpp
* DESCRIPTION: Abstract graphical rendering primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "gui.hpp"
#include "rectangles.hpp"
#include "image_files.hpp"

/******************************************************************************
* Constructors
******************************************************************************/

renderer_rep::renderer_rep ():
  ox (0), oy (0), cx1 (0), cy1 (0), cx2 (0), cy2 (0),
  sfactor (1), pixel (PIXEL), thicken (0),
  master (NULL), pattern (UNINIT) {}

renderer_rep::~renderer_rep () {}

/******************************************************************************
* Device specific
******************************************************************************/

bool
renderer_rep::is_printer () {
  return false;
}

bool
renderer_rep::is_x_drawable () {
  return false;
}

void
renderer_rep::get_extents (int& w, int& h) {
  w= h= 0;
}

x_drawable_rep*
renderer_rep::as_x_drawable () {
  return NULL;
}

void
renderer_rep::next_page () {
}

bool
renderer_rep::repainted () {
  return true;
}

bool
renderer_rep::interrupted (bool check) {
  (void) check;
  return false;
}

void
renderer_rep::anchor(string label, SI x, SI y) {
  (void) label; (void) x; (void) y;
  return;
}

void
renderer_rep::href(string label, SI x1, SI y1, SI x2, SI y2) {
  (void) label;
  (void) x1; (void) y1; (void) x2; (void) y2;
  return;
}


/******************************************************************************
* Origin and shrinking factor
******************************************************************************/

void
renderer_rep::set_origin (SI x, SI y) {
  ox= x;
  oy= y;
}

void
renderer_rep::move_origin (SI dx, SI dy) {
  ox += dx;
  oy += dy;
}

void
renderer_rep::set_shrinking_factor (int sf) {
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
renderer_rep::get_clipping (SI &x1, SI &y1, SI &x2, SI &y2) {
  x1= cx1- ox; y1= cy1- oy;
  x2= cx2- ox; y2= cy2- oy;
}

void
renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
  outer_round (x1, y1, x2, y2);
  cx1= x1+ ox; cy1= y1+ oy;
  cx2= x2+ ox; cy2= y2+ oy;
}

void
renderer_rep::extra_clipping (SI x1, SI y1, SI x2, SI y2) {
  SI ox1, oy1, ox2, oy2;
  get_clipping (ox1, oy1, ox2, oy2);
  x1= max (x1, ox1); y1= max (y1, oy1);
  x2= max (x1, min (x2, ox2)); y2= max (y1, min (y2, oy2));
  set_clipping (x1, y1, x2, y2);
}

void
renderer_rep::clip (SI x1, SI y1, SI x2, SI y2) {
  rectangle r (cx1, cy1, cx2, cy2);
  clip_stack= rectangles (r, clip_stack);
  set_clipping (x1, y1, x2, y2);
}

void
renderer_rep::unclip () {
  rectangle r (clip_stack->item);
  set_clipping (r->x1- ox, r->y1- oy, r->x2- ox, r->y2- oy);
  clip_stack= clip_stack->next;
}

bool
renderer_rep::is_visible (SI x1, SI y1, SI x2, SI y2) {
  return
    (x2 >= (cx1- ox)) && (y2 >= (cy1- oy)) &&
    (x1 <  (cx2- ox)) && (y1 <  (cy2- oy));
}

#define RND(x) (((x)>=0)?(((x)/pixel)*pixel):((((x)-pixel+1)/pixel)*pixel))

void
renderer_rep::round (SI& x, SI& y) {
  x= RND (x+ ox)- ox;
  y= RND (y+ oy)- oy;
}

void
renderer_rep::inner_round (SI& x1, SI& y1, SI& x2, SI& y2) {
  x1= RND (x1+ox+pixel-1) - ox;
  y1= RND (y1+oy+pixel-1) - oy;
  x2= RND (x2+ox) - ox;
  y2= RND (y2+oy) - oy;
}

void
renderer_rep::outer_round (SI& x1, SI& y1, SI& x2, SI& y2) {
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
renderer_rep::triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;
  polygon (x, y);
}

void
renderer_rep::set_background_pattern (tree pat) {
  pattern= pat;
  if (pattern == "");
  else if (is_atomic (pattern))
    set_background (named_color (pat->label));
  else if (is_func (pattern, PATTERN, 4))
    set_background (named_color (as_string (pattern[3])));
}

tree
renderer_rep::get_background_pattern () {
  if (is_atomic (pattern) || is_func (pattern, PATTERN, 4)) return pattern;
  else {
    tree s= get_named_color (get_background ());
    if (is_func (pattern, PATTERN, 3))
      return pattern * tree (PATTERN, s);
    else return s;
  }
}

bool is_percentage (tree t, string s= "%");
double as_percentage (tree t);

void
renderer_rep::clear_pattern (SI x1, SI y1, SI x2, SI y2) {
  if (pattern == "");
  else if (is_atomic (pattern))
    clear (x1, y1, x2, y2);
  else if (is_func (pattern, PATTERN)) {
    outer_round (x1, y1, x2, y2);
    //cout << "A: " << x1 << ", " << y1 << ", " << x2 << ", " << y2 << "\n";
    //cout << "A: " << x/pixel1 << ", " << y1 << ", " << x2 << ", " << y2 << "\n";
    SI cx1, cy1, cx2, cy2;
    get_clipping (cx1, cy1, cx2, cy2);
    extra_clipping (x1, y1, x2, y2);

    url u= as_string (pattern[0]);
    int imw_pt, imh_pt;
    image_size (u, imw_pt, imh_pt);
    double pt= ((double) 600*PIXEL) / 72.0;
    SI imw= (SI) (((double) imw_pt) * pt);
    SI imh= (SI) (((double) imh_pt) * pt);

    SI w= x2 - x1, h= y2 - y1;
    if (pattern[1] == "") w= imw;
    else if (is_int (pattern[1])) w= as_int (pattern[1]);
    else if (is_percentage (pattern[1]))
      w= (SI) (as_percentage (pattern[1]) * ((double) w));
    else if (is_percentage (pattern[1], "@"))
      w= (SI) (as_percentage (pattern[1]) * ((double) h));
    if (pattern[1] == "") h= imh;
    else if (is_int (pattern[2])) h= as_int (pattern[2]);
    else if (is_percentage (pattern[2]))
      h= (SI) (as_percentage (pattern[2]) * ((double) h));
    else if (is_percentage (pattern[2], "@"))
      h= (SI) (as_percentage (pattern[2]) * ((double) w));
    w= ((w + pixel - 1) / pixel) * pixel;
    h= ((h + pixel - 1) / pixel) * pixel;

    SI sx= 0; //is_percentage (pattern[1])? 0: ox;
    SI sy= 0; //is_percentage (pattern[2])? 0: oy;
    for (int i= ((x1+sx)/w) - 1; i <= ((x2+sx)/w) + 1; i++)
      for (int j= ((y1+sy)/h) - 1; j <= ((y2+sy)/h) + 1; j++) {
	SI X1= i*w     - sx, Y1= j*h     - sy;
	SI X2= (i+1)*w - sx, Y2= (j+1)*h - sy;
	if (X1 < x2 && X2 > x1 && Y1 < y2 && Y2 > y1)
	  image (u, w, h, X1, Y1, 0.0, 0.0, 1.0, 1.0);
      }
    set_clipping (cx1, cy1, cx2, cy2, true);
  }
  else clear (x1, y1, x2, y2);
}

#undef RND
