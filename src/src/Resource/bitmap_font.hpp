
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

RESOURCE(bitmap_metric);
RESOURCE(bitmap_font);

typedef int SI;
typedef unsigned char QN;

struct text_extents_struct {
  SI x1, y1;
  SI x2, y2;
  SI x3, y3;
  SI x4, y4;
};

typedef text_extents_struct text_extents[1];

/******************************************************************************
* The bitmap_char structure
******************************************************************************/

struct bitmap_char_rep: concrete_struct {
  short depth;               // number of bits per pixel >= depth
  short width, height;       // width and height in pixels
  short xoff, yoff;          // offset of origin
  short lwidth;              // logical width of character
  short status;              // status for extensible characters
  QN*   raster;              // character definition

  bitmap_char_rep (int w, int h, int xoff, int yoff, int depth, int status=0);
  ~bitmap_char_rep ();
  inline int  get_1 (int i, int j);
  inline void set_1 (int i, int j, int with);
  int  get_x (int i, int j);
  void set_x (int i, int j, int with);
  int  get (int i, int j);
  void set (int i, int j, int with);
  void adjust_bot ();
  void adjust_top ();
};

struct bitmap_char {
  CONCRETE_NULL(bitmap_char);
  bitmap_char (int w2, int h2, int xoff2, int yoff2,
	       int depth2=1, int status2= 0);
};
CONCRETE_NULL_CODE(bitmap_char);

inline int
bitmap_char_rep::get_1 (int i, int j) {
  int bit= j*width+i;
  return (raster[bit>>3] >> (bit&7)) & 1;
}

inline void
bitmap_char_rep::set_1 (int i, int j, int with) {
  int bit= j*width+i;
  if (with==0) raster[bit>>3] &= ~(1 << (bit&7));
  else raster[bit>>3] |= (1 << (bit&7));
}

ostream& operator << (ostream& out, bitmap_char bmc);

bitmap_char shrink     (bitmap_char bmc, int xf, int yf, SI& xo, SI& yo);
bitmap_char join       (bitmap_char bmc1, bitmap_char bmc2);
bitmap_char glue       (bitmap_char bmc1, bitmap_char bmc2);
bitmap_char add        (bitmap_char bmc1, bitmap_char bmc2);
bitmap_char move       (bitmap_char bmc, SI x, SI y);
bitmap_char enlarge    (bitmap_char bmc, SI x1, SI y1, SI x2, SI y3);
bitmap_char hor_flip   (bitmap_char bmc);
bitmap_char ver_flip   (bitmap_char bmc);
bitmap_char pos_rotate (bitmap_char bmc);
bitmap_char hor_extend (bitmap_char bmc, int pos, int by);
bitmap_char ver_extend (bitmap_char bmc, int pos, int by);

/******************************************************************************
* Bitmap metrics
******************************************************************************/

struct bitmap_metric_rep:rep<bitmap_metric> {
  int bc, ec;
  text_extents* bmm;
  text_extents  on_error;

  bitmap_metric_rep (string name, text_extents* bmm, int bc, int ec);
  ~bitmap_metric_rep ();
  inline text_extents_struct* get (int char_code);
};

inline text_extents_struct*
bitmap_metric_rep::get (int c) {
  if ((c<bc) || (c>ec)) return on_error;
  return bmm [c-bc];
}

/******************************************************************************
* Bitmap fonts
******************************************************************************/

struct bitmap_font_rep:rep<bitmap_font> {
  int bc, ec;
  bitmap_char* bmf; // definitions of the characters

  bitmap_font_rep (string name, bitmap_char* bmf, int bc, int ec);
  ~bitmap_font_rep ();
  inline bitmap_char get (int char_code);
};

inline bitmap_char
bitmap_font_rep::get (int c) {
  if ((c<bc) || (c>ec)) return bitmap_char ();
  return bmf [c-bc];
}

#endif // defined BITMAP_FONT_H
