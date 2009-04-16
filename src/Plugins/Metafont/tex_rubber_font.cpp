
/******************************************************************************
* MODULE     : tex_rubber_font.cpp
* DESCRIPTION: TeX rubber fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "translator.hpp"
#include "Metafont/load_tex.hpp"

/******************************************************************************
* TeX rubber fonts
******************************************************************************/

struct tex_rubber_font_rep: font_rep {
  string           family;
  int              dpi;
  int              dsize;
  translator       ext;
  tex_font_metric  tfm;
  font_glyphs      pk;
  double           unit;

  tex_rubber_font_rep (string name, string trl_name,
		       string family, int size, int dpi, int dsize);
  void get_extents (int c, metric& ex);
  void get_partial_extents (int c, metric& ex);
  void get_extents (string s, metric& ex);
  void draw (renderer ren, int c, SI x, SI& y, SI& real_y);
  void draw (renderer ren, string s, SI x, SI y);

  double get_left_slope (string s);
  double get_right_slope (string s);
  SI     get_right_correction (string s);
};

struct tex_dummy_rubber_font_rep: font_rep {
  font base_fn;
  tex_dummy_rubber_font_rep (string name, font base_fn);
  void get_extents (string s, metric& ex);
  void draw (renderer ren, string s, SI x, SI y);
};

/******************************************************************************
* The implementation of TeX rubber fonts
******************************************************************************/

#define conv(x) ((SI) (((double) (x))*unit))

tex_rubber_font_rep::tex_rubber_font_rep (string name,
  string trl_name, string family2, int size2, int dpi2, int dsize2):
    font_rep (name), dsize (dsize2), ext (load_translator (trl_name))
{
  load_tex (family2, size2, dpi2, dsize, tfm, pk);

  family       = family2;
  size         = size2;
  dpi          = dpi2;
  design_size  = tfm->design_size () >> 12;
  display_size = (((design_size*dpi)/72)*PIXEL) >> 8;
  unit         = ((double) display_size) / ((double) (1<<20));
  slope        = tfm->slope ();
  spc->def     = conv (tfm->spc ());
  spc->min     = spc->def - conv (tfm->spc_shrink ());
  spc->max     = spc->def + conv (tfm->spc_stretch ());
  extra        = conv (tfm->spc_extra ());
  extra->min   = extra->min >> 1;
  extra->max   = extra->min << 1;
  sep          = ((((dpi*PIXEL)/72)*design_size) >> 8) / 10;

  y1           = conv (-262080);
  y2           = y1+ display_size;
  yx           = conv (tfm->x_height ());
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  wpt          = (dpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;
  wquad        = conv (tfm->spc_quad ());
}

font
tex_rubber_font (string trl_name,
		 string family, int size, int dpi, int dsize) {
  string name= "tex-rubber:"*family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_rubber_font_rep> (name, trl_name, family, size, dpi, dsize));
}

/******************************************************************************
* Drawing rubber boxes and computing extents
******************************************************************************/

void
tex_rubber_font_rep::get_extents (int c, metric& ex) {
  glyph gl= pk->get (c);
  if (is_nil (gl))
    ex->x1= ex->y1= ex->x2= ex->y2= ex->x3= ex->y3= ex->x4= ex->y4= 0;
  else {
    ex->x1=  0;
    ex->y1= -conv (tfm->d(c));
    ex->x2=  conv (tfm->w(c));
    ex->y2=  conv (tfm->h(c));
    ex->x3= -((int) gl->xoff) * PIXEL;
    ex->x4=  ((int) (gl->width- gl->xoff)) * PIXEL;
    ex->y3=  ((int) (gl->yoff- gl->height)) * PIXEL;
    ex->y4=  ((int) gl->yoff) * PIXEL;

    ex->x3 -= 2*PIXEL; ex->x4 += 2*PIXEL;
    ex->y3 -= 2*PIXEL; ex->y4 += 3*PIXEL;
  }
}

void
tex_rubber_font_rep::get_partial_extents (int c, metric& ex) {
  metric ey;
  get_extents (c, ey);
  ex->x1= min (ex->x1, ey->x1); ex->y1 -= (ey->y2-ey->y1);
  ex->x2= max (ex->x2, ey->x2);
  ex->x3= min (ex->x3, ey->x3); ex->y3= min (ex->y3, ex->y1+ (ey->y3-ey->y1));
  ex->x4= max (ex->x4, ey->x4); ex->y4= max (ex->y4, ex->y1+ (ey->y4-ey->y1));
}

void
tex_rubber_font_rep::get_extents (string s, metric& ex) {
  ASSERT ((N(s)>=2) && (s[0]=='<') && (s[N(s)-1]=='>'),
	  "invalid rubber character");

  // determining base character and serial number
  int i;
  for (i=N(s)-1; i>0; i--) if (s[i]=='-') break;
  string r= s (0, i) * ">";
  QN pre_c= ext->dict[r];
  int n= as_int (s (i+1, N(s)-1));
  if ((pre_c<tfm->bc) || (pre_c>tfm->ec)) {
    ex->x1= ex->y1= ex->x2= ex->y2= ex->x3= ex->y3= ex->x4= ex->y4= 0;
    return;
  }

  // get extents
  QN c= tfm->nth_in_list (pre_c, n);
  if (tfm->tag (c) != 3) get_extents (c, ex);
  else {
    int i, nr_rep= n- tfm->list_len (pre_c);

    ex->x1= ex->x3= ex->y3= PLUS_INFINITY;
    ex->x2= ex->x4= ex->y4= MINUS_INFINITY;
    ex->y1= ex->y2= 0;

    if (tfm->top (c)!=0) get_partial_extents (tfm->top (c), ex);
    if ((tfm->rep (c)!=0) && (tfm->mid (c)!=0))
      for (i=0; i<nr_rep; i++) get_partial_extents (tfm->rep (c), ex);
    if (tfm->mid (c)!=0) get_partial_extents (tfm->mid (c), ex);
    if (tfm->rep (c)!=0)
      for (i=0; i<nr_rep; i++) get_partial_extents (tfm->rep (c), ex);
    if (tfm->bot (c)!=0) get_partial_extents (tfm->bot (c), ex);
  }

  if ((N(s) >= 7) && (s(0,5) == "<big-") && (n == 2)) {
    // correction for very big operators
    ex->y1 += ex->y2;
    ex->y2  = 0;
  }
  else {
    // correction for large delimiters
    int j;
    for (j=1; j<N(s); j++) if (s[j]=='-') break;
    if (j==N(s)) return; else j++;
    switch (s[j]) {
    case 'r':
      if ((N(s) >= j+6) && (s(j+1,j+6) == "angle")) {
	ex->x1= ex->x3- sep;
	ex->x2= ex->x4+ sep;
	break;
      }
    case ']':
    case '|':
      ex->x1= ex->x3- sep;
      ex->x2= ex->x4+ (3*sep/2);
      break;
    case 's':
      break;
    default:
      ex->x1= ex->x3- sep;
      ex->x2= ex->x4+ sep;
    }
  }
}

void
tex_rubber_font_rep::draw (renderer ren, int c, SI x, SI& y, SI& real_y) {
  ren->draw (c, pk, x, y);
  SI delta  = conv (tfm->h (c)+ tfm->d (c));
  SI pixel  = PIXEL * ren->sfactor;
  y        -= pixel * (delta/pixel);
  real_y   -= delta;
  while (y >= real_y + pixel) y -= pixel;
}

void
tex_rubber_font_rep::draw (renderer ren, string s, SI x, SI y) {
  metric ex;
  get_extents (s, ex);

  // determining base character and serial number
  int i;
  for (i=N(s)-1; i>0; i--) if (s[i]=='-') break;
  string r= s (0, i) * ">";
  QN pre_c= ext->dict[r];
  int n= as_int (s (i+1, N(s)-1));

  // draw the character
  if ((pre_c<tfm->bc) || (pre_c>tfm->ec)) return;
  QN c = tfm->nth_in_list (pre_c, n);

  if (tfm->tag (c) != 3) ren->draw (c, pk, x, y);
  else {
    int i;
    int nr_rep= n- tfm->list_len (pre_c);

    SI real_y= y; // may be necessary to round y
                  // using SI temp= x; decode (temp, y); encode (temp, y);
    if (tfm->top (c)!=0) draw (ren, tfm->top (c), x, y, real_y);
    if (tfm->rep (c)!=0)
      for (i=0; i<nr_rep; i++)
	draw (ren, tfm->rep (c), x, y, real_y);
    if (tfm->mid (c)!=0) draw (ren, tfm->mid (c), x, y, real_y);
    if ((tfm->rep (c)!=0) && (tfm->mid (c)!=0))
      for (i=0; i<nr_rep; i++)
	draw (ren, tfm->rep (c), x, y, real_y);
    if (tfm->bot (c)!=0) draw (ren, tfm->bot (c), x, y, real_y);
  }
}

/******************************************************************************
* Metric properties of rubber boxes
******************************************************************************/

double
tex_rubber_font_rep::get_left_slope (string s) {
  if ((N(s)>=5) && (s(0,5)=="<big-") && (get_right_correction(s)!=0))
    return 0.25;
  return slope;
}

double
tex_rubber_font_rep::get_right_slope (string s) {
  if ((N(s)>=5) && (s(0,5)=="<big-") && (get_right_correction(s)!=0))
    return 0.25;
  return slope;
}

SI
tex_rubber_font_rep::get_right_correction (string s) {
  if ((N(s)>=5) && (s(0,5)=="<big-")) {
    int i;
    for (i=N(s)-1; i>0; i--) if (s[i]=='-') break;
    string r= s (0, i) * ">";
    QN  pre_c= ext->dict[r];
    int n    = as_int (s (i+1, N(s)-1));
    QN  c    = tfm->nth_in_list (pre_c, n);
    return conv (tfm->i (c));
  }
  return 0;
}

#undef conv

/******************************************************************************
* The implementation of TeX dummy rubber fonts for \left. \right. and \big.
******************************************************************************/

tex_dummy_rubber_font_rep::tex_dummy_rubber_font_rep (string name, font fn):
  font_rep (name), base_fn (fn) {}

void
tex_dummy_rubber_font_rep::get_extents (string s, metric& ex) {
  string r= s;
  if (s(0,8) == "<large-.") r= "<left-(" * s (8, N(s));
  if (s(0,7) == "<left-.") r= "<left-(" * s (7, N(s));
  if (s(0,6) == "<mid-.") r= "<left-(" * s (6, N(s));
  if (s(0,8) == "<right-.") r= "<left-(" * s (8, N(s));
  if (s(0,6) == "<big-.") r= "<big-sum" * s (6, N(s));
  base_fn->get_extents (r, ex);
  ex->x1= ex->x2= ex->x3= ex->x4= 0;
}

void
tex_dummy_rubber_font_rep::draw (renderer ren, string s, SI x, SI y) {
  (void) ren; (void) s; (void) x; (void) y;
}

font
tex_dummy_rubber_font (font base_fn) {
  string name= "tex-dummy(" * base_fn->res_name * ")";
  return make (font, name, tm_new<tex_dummy_rubber_font_rep> (name, base_fn));
}
