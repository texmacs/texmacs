
/******************************************************************************
* MODULE     : x_font.hpp
* DESCRIPTION: X11 fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef X_FONT_H
#define X_FONT_H
#include "font.hpp"

/******************************************************************************
* X bitmaps
******************************************************************************/

struct Bitmap_rep {
  Pixmap bm;
  int width, height;
  SI xoff, yoff;
};
typedef Bitmap_rep* Bitmap;

/******************************************************************************
* The x_font representation class
******************************************************************************/

struct x_font_rep: font_rep {
  string      family;
  int         dpi;
  font_metric fnm;
  font_glyphs fng;

  x_font_rep (string name, string family, int size, int dpi);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
  glyph get_glyph (string s);
};

#endif // defined X_FONT_H
