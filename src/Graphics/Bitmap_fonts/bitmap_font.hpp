
/******************************************************************************
* MODULE     : bitmap_font.hpp
* DESCRIPTION: bitmap fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BITMAP_FONT_H
#define BITMAP_FONT_H
#include "resource.hpp"

class frame;

RESOURCE(font_metric);
RESOURCE(font_glyphs);

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
  unsigned int index;        // physical index of glyph in font
  short depth;               // number of bits per pixel >= depth
  short width, height;       // width and height in pixels
  short xoff, yoff;          // offset of origin
  short lwidth;              // logical width of character
  short status;              // status for extensible characters
  short artistic;            // result of applying an artistic effect
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

tm_ostream& operator << (tm_ostream& out, glyph gl);

void  get_bounding_box (glyph gl, SI& x1, SI& y1, SI& x2, SI& y2);
bool  empty_column (glyph gl, int i);
bool  empty_row (glyph gl, int j);
int   first_in_row (glyph gl, int j);
int   first_in_rows (glyph gl, int j1, int j2);
int   last_in_row (glyph gl, int j);
int   last_in_rows (glyph gl, int j1, int j2);
int   first_in_column (glyph gl, int i);
int   last_in_column (glyph gl, int i);
SI    collision_offset (glyph gl1, glyph gl2, bool overlap);
int   probe (glyph gl, int x, int y, int dx, int dy);
void  transform (metric& ey, metric ex, frame fr);
void  rotate (metric& ey, metric ex, double angle, double ox, double oy);
frame reslash (metric slash, metric proto);
void  normalize_borders (glyph& gl, metric& ex);

glyph shrink      (glyph gl, int xf, int yf, SI& xo, SI& yo);
glyph join        (glyph gl1, glyph gl2);
glyph intersect   (glyph gl1, glyph gl2);
glyph exclude     (glyph gl1, glyph gl2);
glyph glue        (glyph gl1, glyph gl2);
glyph add         (glyph gl1, glyph gl2);
glyph move        (glyph gl, SI x, SI y);
glyph bar_right   (glyph gl1, glyph gl2);
glyph bar_bottom  (glyph gl1, glyph gl2);
glyph copy        (glyph gl);
glyph simplify    (glyph gl);
glyph padded      (glyph gl, int l, int t, int r, int b);
glyph clip        (glyph gl, SI x1, SI y1, SI x2, SI y3);
glyph hor_flip    (glyph gl);
glyph ver_flip    (glyph gl);
glyph transpose   (glyph gl);
glyph pos_rotate  (glyph gl);
glyph hor_extend  (glyph gl, int pos, int by);
glyph ver_extend  (glyph gl, int pos, int by);
glyph hor_take    (glyph gl, int pos, int nr);
glyph ver_take    (glyph gl, int pos, int nr);
glyph bottom_edge (glyph gl, SI penh, SI keepy);
glyph flood_fill  (glyph gl, SI px, SI py);
glyph slanted     (glyph gl, double slant);
glyph stretched   (glyph gl, double xf, double yf);
glyph deepen      (glyph gl, double yf, SI penw);
glyph widen       (glyph gl, double xf, SI penw);
glyph mono        (glyph gl, SI lw, SI phw);
glyph bolden      (glyph gl, SI dpen, SI dtot, SI dver);
glyph make_bbb    (glyph gl, int code, SI penw, SI penh, SI fatw);
glyph distorted   (glyph gl, tree kind, SI em, int c);
glyph transform   (glyph gl, frame fr);
glyph rotate      (glyph gl, double angle, double ox, double oy);
glyph curly       (glyph gl);
glyph unserif     (glyph gl, int code, SI penw);

glyph circle_glyph (SI rad, SI penw);

int pixel_count (glyph g);
double left_protrusion (glyph g, glyph o);
double right_protrusion (glyph g, glyph o);
double fill_rate (glyph g);
int vertical_stroke_width (glyph g);
int horizontal_stroke_width (glyph g);
bool is_sans_serif (glyph g);
double get_slant (glyph g);
int italic_a_status (glyph g);

/******************************************************************************
* Abstract bitmap fonts and font metrics
******************************************************************************/

struct font_metric_rep: rep<font_metric> {
  bool bad_font_metric; // when font metric could not be loaded
  font_metric_rep (string name);
  virtual ~font_metric_rep ();
  virtual bool exists (int char_code);
  virtual metric& get (int char_code) = 0;
  virtual SI kerning (int left_code, int right_code);
};

struct font_glyphs_rep: rep<font_glyphs> {
  bool bad_font_glyphs; // when font glyphs could not be loaded
  font_glyphs_rep (string name);
  virtual ~font_glyphs_rep ();
  virtual glyph& get (int char_code) = 0;
};

font_metric std_font_metric (string s, metric* fnm, int bc, int ec);
font_glyphs std_font_glyphs (string name, glyph* fng, int bc, int ec);

font_metric slanted (font_metric fnm, double slant);
font_glyphs slanted (font_glyphs fng, double slant);
font_metric stretched (font_metric fnm, double xf, double yf);
font_glyphs stretched (font_glyphs fng, double xf, double yf);
font_glyphs extended (font_glyphs fng, double xf, SI penw);
font_metric mono (font_metric fnm, SI lw, SI phw);
font_glyphs mono (font_glyphs fng, SI lw, SI phw);
font_metric bolden (font_metric fnm, SI dtot, SI dver);
font_glyphs bolden (font_glyphs fng, SI dpen, SI dtot, SI dver);
font_glyphs make_bbb (font_glyphs fng, SI penw, SI penh, SI fatw);
font_glyphs distorted (font_glyphs fng, tree kind, SI em);
font_metric effected (font_metric fnm, tree eff, SI em);
font_glyphs effected (font_glyphs fng, tree eff, SI em);

#endif // defined BITMAP_FONT_H
