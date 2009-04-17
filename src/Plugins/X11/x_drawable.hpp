
/******************************************************************************
* MODULE     : x_drawable.hpp
* DESCRIPTION: Drawables under X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef X_DRAWABLE_H
#define X_DRAWABLE_H
#include "renderer.hpp"
#include "X11/x_gui.hpp"
#include "X11/x_font.hpp"
#include "rectangles.hpp"
#include "array.hpp"

/******************************************************************************
* The x_drawable class
******************************************************************************/

class x_window_rep;
class x_drawable_rep: public renderer_rep {
  x_gui          gui;
  Display*       dpy;
  Drawable       win;
  x_window_rep*  x_win;
  int            w, h;
  GC             gc;
  color          cur_fg, cur_bg;

public:

  x_drawable_rep (x_gui gui, x_window_rep* x_win);
  x_drawable_rep (x_gui gui, int w, int h);
  ~x_drawable_rep ();

  bool is_x_drawable ();
  x_drawable_rep* as_x_drawable ();
  void get_extents (int& w, int& h);
  bool interrupted (bool check= false);

  void encode (SI& x, SI& y);  // X coordinates -> mathematical coordinates
  void decode (SI& x, SI& y);  // mathematical coordinates -> X coordinates

  /**************** subroutines for drawing text and xpms ********************/

  void draw_clipped (Pixmap pm, Pixmap bm, int w, int h, SI x, SI y);
  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void xpm_initialize (url file_name);

  /********************** routines from renderer.hpp *************************/

  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  color get_color ();
  color get_background ();
  void  set_color (color c);
  void  set_background (color c);
  void  set_line_style (SI w, int type=0, bool round=true);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  xpm (url file_name, SI x, SI y);
  void  image (url u, SI w, SI h, SI x, SI y,
	       double cx1, double cy1, double cx2, double cy2);

  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  /****************************** friends ************************************/

  friend class x_gui_rep;
  friend class x_window_rep;
  friend Bool my_selnotify_predicate (Display* dpy, XEvent* ev, XPointer arg);
};

#endif // defined X_DRAWABLE_H
