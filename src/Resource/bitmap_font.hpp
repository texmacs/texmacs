
/******************************************************************************
* MODULE     : bitmap_font.hpp
* DESCRIPTION: bitmap fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef BITMAP_FONT_H
#define BITMAP_FONT_H
#include "resource.hpp"

RESOURCE(font_metric);
RESOURCE(font_glyphs);

typedef int SI;
typedef unsigned char QN;

struct metric_struct {
  SI x1, y1;
  SI x2, y2;
  SI x3, y3;
  SI x4, y4;
};

typedef metric_struct metric[1];

/******************************************************************************
* The glyph structure
******************************************************************************/

struct glyph_rep: concrete_struct {
  short depth;               // number of bits per pixel >= depth
  short width, height;       // width and height in pixels
  short xoff, yoff;          // offset of origin
  short lwidth;              // logical width of character
  short status;              // status for extensible characters
  QN*   raster;              // character definition

  glyph_rep (int w, int h, int xoff, int yoff, int depth, int status=0);
  ~glyph_rep ();
  inline int  get_1 (int i, int j);
  inline void set_1 (int i, int j, int with);
  int  get_x (int i, int j);
  void set_x (int i, int j, int with);
  int  get (int i, int j);
  void set (int i, int j, int with);
  void adjust_bot ();
  void adjust_top ();
};

struct glyph {
  CONCRETE_NULL(glyph);
  glyph (int w2, int h2, int xoff2, int yoff2, int depth2=1, int status2= 0);
};
CONCRETE_NULL_CODE(glyph);

inline int
glyph_rep::get_1 (int i, int j) {
  int bit= j*width+i;
  return (raster[bit>>3] >> (bit&7)) & 1;
}

inline void
glyph_rep::set_1 (int i, int j, int with) {
  int bit= j*width+i;
  if (with==0) raster[bit>>3] &= ~(1 << (bit&7));
  else raster[bit>>3] |= (1 << (bit&7));
}

ostream& operator << (ostream& out, glyph gl);

glyph shrink     (glyph gl, int xf, int yf, SI& xo, SI& yo);
glyph join       (glyph gl1, glyph gl2);
glyph glue       (glyph gl1, glyph gl2);
glyph add        (glyph gl1, glyph gl2);
glyph move       (glyph gl, SI x, SI y);
glyph enlarge    (glyph gl, SI x1, SI y1, SI x2, SI y3);
glyph hor_flip   (glyph gl);
glyph ver_flip   (glyph gl);
glyph pos_rotate (glyph gl);
glyph hor_extend (glyph gl, int pos, int by);
glyph ver_extend (glyph gl, int pos, int by);

/******************************************************************************
* Abstract bitmap fonts and font metrics
******************************************************************************/

struct font_metric_rep: rep<font_metric> {
  font_metric_rep (string name);
  virtual ~font_metric_rep ();
  virtual metric& get (int char_code) = 0;
};

struct font_glyphs_rep: rep<font_glyphs> {
  font_glyphs_rep (string name);
  virtual ~font_glyphs_rep ();
  virtual glyph& get (int char_code) = 0;
};

font_metric std_font_metric (string s, metric* fnm, int bc, int ec);
font_glyphs std_font_glyphs (string name, glyph* fng, int bc, int ec);

#endif // defined BITMAP_FONT_H
