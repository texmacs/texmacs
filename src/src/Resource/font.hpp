
/******************************************************************************
* MODULE     : font.hpp
* DESCRIPTION: fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef FONT_H
#define FONT_H
#include "space.hpp"
#include "encoding.hpp"
#include "display.hpp"
#include "ps_device.hpp"

RESOURCE(font);

class   bitmap_char;

/******************************************************************************
* The font structure
******************************************************************************/

struct font_rep: rep<font> {
  display  dis;              // underlying display
  encoding enc;              // underlying encoding of the font
  SI       size;             // requested size
  SI       design_size;      // design size in points/256
  SI       display_size;     // display size in points/PIXEL
  double   slope;            // italic slope
  space    spc;              // usual space between words
  space    extra;            // extra space at end of words
  SI       sep;              // separation space between close components

  SI       y1;               // bottom y position
  SI       y2;               // top y position
  SI       yx;               // height of the x character
  SI       yfrac;            // vertical position fraction bar
  SI       ysub_lo_base;     // base line for subscripts
  SI       ysub_hi_lim;      // upper limit for subscripts
  SI       ysup_lo_lim;      // lower limit for supscripts
  SI       ysup_lo_base;     // base line for supscripts
  SI       ysup_hi_lim;      // upper limit for supscripts
  SI       yshift;           // vertical script shift inside fractions

  SI       wpt;              // width of one point in font
  SI       wquad;            // wpt * design size in points
  SI       wline;            // width of fraction bars and so

  font_rep (display dis, string name);
  font_rep (display dis, string name, font fn);
  void copy_math_pars (font fn);

  virtual void   get_extents (string s, text_extents& ex) = 0;
  virtual void   get_xpositions (string s, SI* xpos);
  virtual void   draw (ps_device dev, string s, SI x, SI y) = 0;

  virtual double get_left_slope  (string s);
  virtual double get_right_slope (string s);
  virtual SI     get_left_correction  (string s);
  virtual SI     get_right_correction (string s);

  void var_get_extents (string s, text_extents& ex);
  void var_get_xpositions (string s, SI* xpos);
  void var_draw (ps_device dev, string s, SI x, SI y);
  virtual bitmap_char get_bitmap (string s);
};

font join (font fn1, font fn2);
font subfont (font fn, encoding enc);
font error_font (font fn);

font virtual_font (font base, string family, int size, int dpi);
font tt_font (display dis, string family, int size, int dpi);
font ps_font (display dis, string family, int size, int dpi);
font tex_font (display dis, string fam, int size, int dpi, int dsize=10);
font tex_cm_font (display dis, string fam, int size, int dpi, int dsize=10);
font tex_ec_font (display dis, string fam, int size, int dpi, int dsize=10);
font tex_la_font (display dis, string fam, int size, int dpi, int dsize=10);
font tex_adobe_font (display dis, string fam, int size, int dpi, int dsize=10);
font tex_rubber_font (display dis, string trl_name,
		      string fam, int size, int dpi, int dsize=10);
font tex_dummy_rubber_font (font base_fn);

void font_rule (tree which, tree by);
font find_font (display dis, string family, string fn_class,
		string series, string shape, int sz, int dpi);

int  script (int sz, int level);

font math_font (scheme_tree t, font base_fn, font error_fn);

#endif // defined FONT_H
