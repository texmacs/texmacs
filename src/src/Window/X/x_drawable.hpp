
/******************************************************************************
* MODULE     : x_drawable.hpp
* DESCRIPTION: Drawables under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef X_DRAWABLE_H
#define X_DRAWABLE_H
#include "ps_device.hpp"
#include "X/x_display.hpp"
#include "X/x_font.hpp"
#include "rectangles.hpp"
#include "array.hpp"

/******************************************************************************
* The x_drawable class
******************************************************************************/

class x_drawable_rep: virtual public ps_device_rep {
  x_display dis;
  Display*  dpy;
  Drawable  win;
  int       w, h;
  GC        gc;
  color     cur_fg, cur_bg;
  bool      event_status;

public:

  x_drawable_rep (x_display dis, int w=0, int h=0);
  ~x_drawable_rep ();
  int get_type ();

  void encode (SI& x, SI& y);  // X coordinates -> mathematical coordinates
  void decode (SI& x, SI& y);  // mathematical coordinates -> X coordinates

  /**************** subroutines for drawing text and xpms ********************/

  void draw_clipped (Pixmap pm, Pixmap bm, int w, int h, SI x, SI y);
  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void xpm_initialize (url file_name);

  /******************** routines from ps_device.hpp ************************/

  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  color rgb (int r, int g, int b);
  void  get_rgb (color col, int& r, int& g, int& b);
  color get_color ();
  color get_color (string s);
  color get_background ();
  void  set_color (color c);
  void  set_background (color c);
  void  set_line_style (SI w, int type=0, bool round=true);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  xpm (url file_name, SI x, SI y);
  void  postscript (url image,
		    SI w, SI h, SI x, SI y,
		    int x1, int y1, int x2, int y2);

  void next_page ();
  bool check_event (int type);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  /****************************** friends ************************************/

  friend class x_display_rep;
  friend class x_window_rep;
  friend Bool my_predicate (Display* dpy, XEvent* ev, XPointer arg);
};

#endif // defined X_DRAWABLE_H
