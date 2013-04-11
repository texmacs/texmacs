
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
#include "frame.hpp"

int std_shrinkf= 5;

/******************************************************************************
* Constructors and handles
******************************************************************************/

renderer_rep::renderer_rep (bool screen_flag):
  ox (0), oy (0), cx1 (0), cy1 (0), cx2 (0), cy2 (0),
  is_screen (screen_flag),
  zoomf (std_shrinkf), shrinkf (1), pixel (PIXEL), thicken (0),
  master (NULL) {}

renderer_rep::~renderer_rep () {}

void*
renderer_rep::get_handle () {
  return NULL;
}

void*
renderer_rep::get_data_handle () {
  return NULL;
}

/******************************************************************************
* Device specific
******************************************************************************/

bool
renderer_rep::is_printer () {
  return false;
}

void
renderer_rep::get_extents (int& w, int& h) {
  w= h= 0;
}

void
renderer_rep::next_page () {
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

double
normal_zoom (double zoom) {
  return 320.0 / ceil (320.0 / zoom - 0.01);
}

void
renderer_rep::set_zoom_factor (double zoom) {
  if (shrinkf != ((int) tm_round (std_shrinkf / zoomf)))
    cout << "Invalid zoom " << zoomf << ", " << shrinkf << LF;
  ox = (SI) tm_round (ox  * zoomf);
  oy = (SI) tm_round (oy  * zoomf);
  //cx1= (SI) ::floor (cx1 * zoomf);
  //cx2= (SI) ::floor (cx2 * zoomf);
  //cy1= (SI) ::ceil  (cy1 * zoomf);
  //cy2= (SI) ::ceil  (cy2 * zoomf);
  cx1= (SI) tm_round (cx1 * zoomf);
  cx2= (SI) tm_round (cx2 * zoomf);
  cy1= (SI) tm_round (cy1 * zoomf);
  cy2= (SI) tm_round (cy2 * zoomf);
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;
  ox = (SI) tm_round (ox  / zoomf);
  oy = (SI) tm_round (oy  / zoomf);
  //cx1= (SI) ::floor (cx1 / zoomf);
  //cx2= (SI) ::floor (cx2 / zoomf);
  //cy1= (SI) ::ceil  (cy1 / zoomf);
  //cy2= (SI) ::ceil  (cy2 / zoomf);
  cx1= (SI) tm_round (cx1 / zoomf);
  cx2= (SI) tm_round (cx2 / zoomf);
  cy1= (SI) tm_round (cy1 / zoomf);
  cy2= (SI) tm_round (cy2 / zoomf);
}

void
renderer_rep::reset_zoom_factor () {
  set_zoom_factor (std_shrinkf);
}

void
renderer_rep::set_shrinking_factor (int sf) {
  set_zoom_factor (((double) std_shrinkf) / ((double) sf));
}

void
renderer_rep::set_transformation (frame fr) {
  (void) fr;
  // FIXME: might replace the renderer by a transformed renderer
}

void
renderer_rep::reset_transformation () {
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

/******************************************************************************
* Reencoding and rounding
******************************************************************************/

void
renderer_rep::encode (SI& x, SI& y) {
  x= (x*pixel) - ox;
  y= ((-y)*pixel) - oy;
}

void
renderer_rep::decode (SI& x, SI& y) {
  x += ox; y += oy;
  if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
  if (y>=0) y= -(y/pixel); else y= -((y-pixel+1)/pixel);
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
renderer_rep::draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;
  polygon (x, y);
}

void
renderer_rep::set_brush (brush b) {
  set_color (b->c);
}

brush
renderer_rep::get_brush () {
  return get_color ();
}

bool is_percentage (tree t, string s= "%");
double as_percentage (tree t);

void
renderer_rep::clear_pattern (SI x1, SI y1, SI x2, SI y2) {
  brush b= get_background ();
  if (b->kind == brush_none);
  else if (b->kind == brush_color)
    clear (x1, y1, x2, y2);
  else if (b->kind == brush_pattern && is_func (b->pattern, PATTERN)) {
    tree pattern= b->pattern;
    int pattern_alpha= b->alpha;
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
    scalable im= load_scalable_image (u, w, h, pixel);
    for (int i= ((x1+sx)/w) - 1; i <= ((x2+sx)/w) + 1; i++)
      for (int j= ((y1+sy)/h) - 1; j <= ((y2+sy)/h) + 1; j++) {
	SI X1= i*w     - sx, Y1= j*h     - sy;
	SI X2= (i+1)*w - sx, Y2= (j+1)*h - sy;
	if (X1 < x2 && X2 > x1 && Y1 < y2 && Y2 > y1)
          draw_scalable (im, X1, Y1, pattern_alpha);
      }
    set_clipping (cx1, cy1, cx2, cy2, true);
  }
  else clear (x1, y1, x2, y2);
}

#undef RND

void
renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  im->draw (this, x, y, alpha);
}

/******************************************************************************
* Drawing selections using alpha transparency
******************************************************************************/

void
renderer_rep::draw_rectangles (rectangles rs) {
  rectangles it= rs;
  while (!is_nil (it)) {
    fill (it->item->x1, it->item->y1, it->item->x2, it->item->y2);
    it= it->next;
  }
}

void
renderer_rep::draw_selection (rectangles rs) {
  color fg= get_color ();
  int r, g, b, a;
  get_rgb_color (fg, r, g, b, a);
  color pfg= rgb_color (r, g, b, (a + 1) / 16);
  rectangles inn= ::thicken (rs, -pixel, -pixel);
  rectangles out= ::simplify (::correct (rs - inn));
  set_color (pfg);
  draw_rectangles (::simplify (inn));
  set_color (fg);
  draw_rectangles (out);
}

/******************************************************************************
* Images
******************************************************************************/

renderer
renderer_rep::shadow (picture& pic, SI x1, SI y1, SI x2, SI y2) {
  SI x0= 0, y0= 0;
  decode (x0, y0);
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  x2= max (x1, x2);
  y2= min (y1, y2);
  pic= native_picture (x2-x1, y1-y2, x0 - x1, (y1 - y2 - 1) - (y0 - y2));
  renderer ren= picture_renderer (pic, zoomf);

  ren->ox = ox;
  ren->oy = oy;
  ren->cx1= cx1;
  ren->cy1= cy1;
  ren->cx2= cx2;
  ren->cy2= cy2;
  ren->is_screen= is_screen;
  ren->zoomf= zoomf;
  ren->shrinkf= shrinkf;
  ren->pixel= pixel;
  ren->thicken= thicken;
  
  int x1b= x0 - pic->get_origin_x ();
  int y2b= y0 + pic->get_origin_y () - (pic->get_height () - 1);
  ren->ox  -= x1b * pixel;
  ren->oy  += y2b * pixel;
  ren->cx1 -= x1b * pixel;
  ren->cy1 += y2b * pixel;
  ren->cx2 -= x1b * pixel;
  ren->cy2 += y2b * pixel;
  return ren;
}

renderer
renderer_rep::shadow (scalable& im , SI x1, SI y1, SI x2, SI y2) {
  (void) im; (void) x1; (void) y1; (void) x2; (void) y2;
  FAILED ("shadowing is not supported");
}

void
renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  (void) p; (void) x; (void) y; (void) alpha;
  FAILED ("rendering pictures is not supported");
}

renderer
scalable_renderer (scalable im) {
  (void) im;
  FAILED ("not yet implemented");
  return NULL;
}

void
delete_renderer (renderer ren) {
  tm_delete (ren);
}

#ifndef QTTEXMACS
#ifndef X11TEXMACS

picture
native_picture (int w, int h, int ox, int oy) {
  (void) w; (void) h; (void) ox; (void) oy;
  FAILED ("not yet implemented");
  return picture ();
}

renderer
picture_renderer (picture p, double zoomf) {
  (void) p; (void) zoomf;
  FAILED ("not yet implemented");
  return NULL;
}

picture
load_picture (url u, int w, int h) {
  (void) u; (void) w; (void) h;
  FAILED ("not yet implemented");
  return picture ();
}

picture
as_native_picture (picture pict) {
  FAILED ("not yet implemented");
  return pict;
}

#endif
#endif
