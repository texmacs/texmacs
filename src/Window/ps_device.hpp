
/******************************************************************************
* MODULE     : ps_device.hpp
* DESCRIPTION: Abstract device for printing post-script graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PS_DEVICE_H
#define PS_DEVICE_H
#include "bitmap_font.hpp"
#include "url.hpp"

typedef int SI;
typedef int color;

#define PIXEL          256
#define PLUS_INFINITY  ((SI) 0x3fffffff)
#define MINUS_INFINITY ((SI) 0xc0000000)

#define ANY_EVENT      0
#define INPUT_EVENT    1
#define DRAG_EVENT     2
#define MOTION_EVENT   3
#define MENU_EVENT     4
#define EVENT_STATUS   5

#define PS_DEVICE_SCREEN   0
#define PS_DEVICE_PRINTER  1

/******************************************************************************
* The abstract ps_device class
******************************************************************************/

class ps_device_rep {
public:
  SI  ox, oy;               // origin
  SI  cx1, cy1, cx2, cy2;   // visible region (clipping)
  int sfactor;              // shrinking factor
  int pixel;                // PIXEL*sfactor
  int thicken;              // extra thinkening = (sfactor>>1)*PIXEL

  ps_device_rep ();
  virtual ~ps_device_rep ();
  virtual int get_type () = 0;

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
  void triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3);

  /* color */
  color black, white, red, green, blue;
  color yellow, magenta, orange, brown, pink;
  color light_grey, grey, dark_grey;
  virtual color rgb (int r, int g, int b) = 0;
  virtual void  get_rgb (color col, int& r, int& g, int& b) = 0;
  virtual color get_color () = 0;
  virtual color get_background () = 0;

  /* main graphical routines */
  virtual void set_color (color c) = 0;
  virtual void set_background (color c) = 0;
  virtual void draw (int char_code, bitmap_font fn, SI x, SI y) = 0;
  virtual void set_line_style (SI w, int type=0) = 0;
  virtual void line (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void clear (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void fill (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) = 0;
  virtual void polygon (array<SI> x, array<SI> y) = 0;
  virtual void xpm (url file_name, SI x, SI y) = 0;
  virtual void postscript (url image,
			   SI w, SI h, SI x, SI y,
			   int x1, int y1, int x2, int y2) = 0;
  virtual void set_clipping (SI x1, SI y1, SI x2, SI y2);

  /* routines for specific devices */
  virtual void next_page () = 0;
  virtual bool check_event (int type) = 0;
  virtual void apply_shadow (SI x1, SI y1, SI x2, SI y2) = 0;
};

typedef ps_device_rep* ps_device;

#endif // defined PS_DEVICE_H
