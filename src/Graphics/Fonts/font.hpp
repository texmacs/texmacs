
/******************************************************************************
* MODULE     : font.hpp
* DESCRIPTION: fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FONT_H
#define FONT_H
#include "space.hpp"
#include "renderer.hpp"

RESOURCE(font);

struct glyph;

#define FONT_TYPE_TEX      0
#define FONT_TYPE_UNICODE  1
#define FONT_TYPE_OTHER    2

/******************************************************************************
* The font structure
******************************************************************************/

struct font_rep: rep<font> {
  int      type;             // font type
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
  SI       wfn;              // wpt * design size in points
  SI       wline;            // width of fraction bars and so
  SI       wquad;            // quad space (often width of widest character M)

  font_rep (string name);
  font_rep (string name, font fn);
  void copy_math_pars (font fn);

  virtual void   get_extents (string s, metric& ex) = 0;
  virtual void   get_xpositions (string s, SI* xpos);
  virtual void   draw (renderer ren, string s, SI x, SI y) = 0;

  virtual double get_left_slope  (string s);
  virtual double get_right_slope (string s);
  virtual SI     get_left_correction  (string s);
  virtual SI     get_right_correction (string s);

  void var_get_extents (string s, metric& ex);
  void var_get_xpositions (string s, SI* xpos);
  void var_draw (renderer ren, string s, SI x, SI y);
  virtual glyph get_glyph (string s);
};

font error_font (font fn);
font virtual_font (font base, string family, int size, int dpi);
font tt_font (string family, int size, int dpi);
font unicode_font (string family, int size, int dpi);
font unicode_math_font (font up, font it, font bup, font bit, font fb);
font x_font (string family, int size, int dpi);
font tex_font (string fam, int size, int dpi, int dsize=10);
font tex_cm_font (string fam, int size, int dpi, int dsize=10);
font tex_ec_font (string fam, int size, int dpi, int dsize=10);
font tex_la_font (string fam, int size, int dpi, int dsize=10);
font tex_adobe_font (string fam, int size, int dpi, int dsize=10);
font tex_rubber_font (string trl_name,
		      string fam, int size, int dpi, int dsize=10);
font tex_dummy_rubber_font (font base_fn);

void font_rule (tree which, tree by);
font find_font (scheme_tree t);
font find_font (string family, string fn_class,
		string series, string shape, int sz, int dpi);

font math_font (scheme_tree t, font base_fn, font error_fn);
font compound_font (scheme_tree def);

int  script (int sz, int level);

#endif // defined FONT_H
