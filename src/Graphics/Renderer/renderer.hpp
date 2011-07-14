
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

typedef int SI;
typedef int color;

#define PIXEL          256
#define PLUS_INFINITY  ((SI) 0x3fffffff)
#define MINUS_INFINITY ((SI) 0xc0000000)

/******************************************************************************
* The abstract renderer class
******************************************************************************/

class renderer_rep;
typedef renderer_rep* renderer;
class x_drawable_rep;
class rectangle;
typedef list<rectangle> rectangles;

class renderer_rep {
public:
  SI  ox, oy;               // origin
  SI  cx1, cy1, cx2, cy2;   // visible region (clipping)
  int sfactor;              // shrinking factor
  int pixel;                // PIXEL*sfactor
  int thicken;              // extra thinkening = (sfactor>>1)*PIXEL
  renderer master;          // master renderer in case of shadow renderers
  tree pattern;             // current background pattern
  rectangles clip_stack;    // stack with clipping regions

public:
  renderer_rep ();
  virtual ~renderer_rep ();

  /* routines for specific renderers */
  virtual bool is_printer ();
  virtual bool is_x_drawable ();
  virtual x_drawable_rep* as_x_drawable ();
  virtual void get_extents (int& w, int& h);
  virtual void next_page ();
  virtual bool repainted ();
  virtual bool interrupted (bool check= false);

  /* basic routines */
  void set_origin (SI x, SI y);
  void move_origin (SI dx, SI dy);
  void set_shrinking_factor (int sfactor);
  void round (SI& x, SI& y);
  void inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
  void outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  friend void abs_round (SI& l);
  friend void abs_round (SI& x, SI& y);
  friend void abs_inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
  friend void abs_outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  bool is_visible (SI x1, SI y1, SI x2, SI y2);

  /* color */
  virtual color get_color () = 0;
  virtual color get_background () = 0;
  virtual tree  get_background_pattern ();

  /* main graphical routines */
  virtual void set_color (color c) = 0;
  virtual void set_background (color c) = 0;
  virtual void set_background_pattern (tree t);
  virtual void draw (int char_code, font_glyphs fn, SI x, SI y) = 0;
  virtual void set_line_style (SI w, int type=0, bool round=true) = 0;
  virtual void line (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void lines (array<SI> x, array<SI> y) = 0;
  virtual void clear (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void clear_pattern (SI x1, SI y1, SI x2, SI y2);
  virtual void fill (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) = 0;
  virtual void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) = 0;
  virtual void polygon (array<SI> x, array<SI> y, bool convex=true) = 0;
  virtual void triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3);
  virtual void xpm (url file_name, SI x, SI y) = 0;
  virtual void image (url u, SI w, SI h, SI x, SI y,
		      double cx1, double cy1, double cx2, double cy2) = 0;
  virtual void get_clipping (SI &x1, SI &y1, SI &x2, SI &y2);
  virtual void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  void extra_clipping (SI x1, SI y1, SI x2, SI y2);
  void clip (SI x1, SI y1, SI x2, SI y2);
  void unclip ();

  /* shadowing and copying rectangular regions across renderers */
  virtual void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y)=0;
  virtual void new_shadow (renderer& ren) = 0;
  virtual void delete_shadow (renderer& ren) = 0;
  virtual void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void apply_shadow (SI x1, SI y1, SI x2, SI y2) = 0;

  /* href and stuff */
  virtual void anchor(string label, SI x, SI y);
  virtual void href(string label, SI x1, SI y1, SI x2, SI y2);
};

void abs_round (SI& l);
void abs_round (SI& x, SI& y);
void abs_inner_round (SI& x1, SI& y1, SI& x2, SI& y2);
void abs_outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
    
#endif // defined RENDERER_H
