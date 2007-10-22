
/******************************************************************************
* MODULE     : x_font.hpp
* DESCRIPTION: Abstract display class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  void draw (ps_device dev, string s, SI x, SI y);
  glyph get_glyph (string s);
};

#endif // defined X_FONT_H
