
/******************************************************************************
* MODULE     : renderer.hpp
* DESCRIPTION: Abstract graphical rendering primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RENDERER_H
#define RENDERER_H
#include "bitmap_font.hpp"
#include "url.hpp"
#include "pencil.hpp"
#include "brush.hpp"
#include "picture.hpp"
#include "scalable.hpp"
#include "spacial.hpp"

#define PIXEL          256
#define PLUS_INFINITY  ((SI) 0x3fffffff)
#define MINUS_INFINITY ((SI) 0xc0000000)
#define PICTURE_ZOOM   1.0

/******************************************************************************
* The abstract renderer class
******************************************************************************/

class renderer_rep;
typedef renderer_rep* renderer;
class x_drawable_rep;
class qt_renderer_rep;
class rectangle;
class frame;
typedef list<rectangle> rectangles;
extern int std_shrinkf;

class renderer_rep {
public:
  SI  ox, oy;               // origin
  SI  cx1, cy1, cx2, cy2;   // visible region (clipping)
  bool is_screen;           // flag for renderers on screen
  double zoomf;             // zoom factor
  int shrinkf;              // shrinking factor
  int pixel;                // size of a pixel on the screen
  int retina_pixel;         // size of a pixel on a retina screen
  int brushpx;              // (hack) -1 or size of a pixel for patterns
  int thicken;              // extra thinkening when anti-aliasing characters
  renderer master;          // master renderer in case of shadow renderers
  rectangles clip_stack;    // stack with clipping regions
  int cur_page;             // current page number

public:
  renderer_rep (bool screen_flag);
  virtual ~renderer_rep ();
  virtual bool is_started ();
  virtual void* get_handle ();
  virtual void* get_data_handle ();

  /* coordinate system */
  void set_origin (SI x, SI y);
  void move_origin (SI dx, SI dy);
  virtual void set_zoom_factor (double zoom);
  void reset_zoom_factor ();
  void set_shrinking_factor (int sf);
  virtual void set_transformation (frame fr);
  virtual void reset_transformation ();

  /* clipping */
  virtual void get_clipping (SI &x1, SI &y1, SI &x2, SI &y2);
  virtual void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  void extra_clipping (SI x1, SI y1, SI x2, SI y2);
  void clip (SI x1, SI y1, SI x2, SI y2);
  void unclip ();

  /* reencoding and rounding */
  virtual void decode (SI& x, SI& y);
  virtual void encode (int& x, int& y);
  virtual void decode (SI x, SI y, double& rx, double& ry);
  virtual void encode (double x, double y, SI& rx, SI& ry);
  void round (SI& x, SI& y);
  void inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
  void outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  friend void abs_round (SI& l);
  friend void abs_round (SI& x, SI& y);
  friend void abs_inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
  friend void abs_outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  bool is_visible (SI x1, SI y1, SI x2, SI y2);

  /* graphical state */
  virtual pencil get_pencil () = 0;
  virtual brush get_brush ();
  virtual brush get_background () = 0;
  virtual void set_pencil (pencil p) = 0;
  virtual void set_brush (brush b);
  virtual void set_background (brush b) = 0;

  /* drawing */
  virtual void draw (int char_code, font_glyphs fn, SI x, SI y) = 0;
  virtual void line (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void lines (array<SI> x, array<SI> y) = 0;
  virtual void clear (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void clear_pattern (SI mx1, SI my1, SI mx2, SI my2,
                              SI x1, SI y1, SI x2, SI y2);
  virtual void clear_pattern (SI x1, SI y1, SI x2, SI y2);
  virtual void fill (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) = 0;
  virtual void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) = 0;
  virtual void polygon (array<SI> x, array<SI> y, bool convex=true) = 0;
  virtual void draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3);
  virtual void draw_spacial (spacial obj);
  virtual void draw_rectangles (rectangles rs);
  virtual void draw_selection (rectangles rs);

  /* shadowing and copying rectangular regions across renderers */
  virtual void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y)=0;
  virtual void new_shadow (renderer& ren) = 0;
  virtual void delete_shadow (renderer& ren) = 0;
  virtual void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void apply_shadow (SI x1, SI y1, SI x2, SI y2) = 0;

  /* images */
  virtual renderer shadow (picture & pic, SI x1, SI y1, SI x2, SI y2);
  virtual renderer shadow (scalable& im , SI x1, SI y1, SI x2, SI y2);
  virtual void draw_picture  (picture  pic, SI x, SI y, int alpha= 255);
  virtual void draw_scalable (scalable im , SI x, SI y, int alpha= 255);

  /* special routines for printers */
  virtual bool is_printer ();
  virtual void get_extents (int& w, int& h);
  virtual void set_page_nr (int nr);
  virtual void next_page ();
  virtual void anchor (string label, SI x1, SI y1, SI x2, SI y2);
  virtual void href (string label, SI x1, SI y1, SI x2, SI y2);
  virtual void toc_entry (string kind, string title, SI x, SI y);
  virtual void set_metadata (string kind, string val);
};

/* native pictures and rendering on pictures */
renderer picture_renderer (picture p, double zoom);
renderer scalable_renderer (scalable im);
void delete_renderer (renderer ren);

double normal_zoom (double zoom);
void abs_round (SI& l);
void abs_round (SI& x, SI& y);
void abs_inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
void abs_outer_round (SI& x1, SI& y1, SI& x2, SI& y2);

extern bool reverse_colors;
void reverse (int& r, int& g, int& b);

extern bool   retina_manual;
extern bool   retina_iman;
extern int    retina_factor;
extern int    retina_icons;
extern double retina_scale;
int    get_retina_factor ();
int    get_retina_icons ();
double get_retina_scale ();
void   set_retina_factor (int f);
void   set_retina_icons (int i);
void   set_retina_scale (double s);

#endif // defined RENDERER_H
