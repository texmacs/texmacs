
/******************************************************************************
* MODULE     : font.cpp
* DESCRIPTION: fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"

RESOURCE_CODE(font);

/******************************************************************************
* Constructors for fonts
******************************************************************************/

font_rep::font_rep (display dis2, string s):
  rep<font> (s),
  dis       (dis2),
  enc       (always_enc),
  spc       (0),
  extra     (0)
{
}

font_rep::font_rep (display dis2, string s, font fn):
  rep<font>    (s),
  dis          (dis2),
  enc          (always_enc),
  size         (fn->size),
  design_size  (fn->design_size),
  display_size (fn->display_size),
  slope        (fn->slope),
  spc          (fn->spc),
  extra        (fn->extra),
  sep          (fn->sep)
{
  copy_math_pars (fn);
}

void
font_rep::copy_math_pars (font fn) {
  y1           = fn->y1;
  y2           = fn->y2;
  yx           = fn->yx;
  yfrac        = fn->yfrac;
  ysub_lo_base = fn->ysub_lo_base;
  ysub_hi_lim  = fn->ysub_hi_lim;
  ysup_lo_lim  = fn->ysup_lo_lim;
  ysup_lo_base = fn->ysup_lo_base;
  ysup_hi_lim  = fn->ysup_hi_lim;
  yshift       = fn->yshift;
  wpt          = fn->wpt;
  wquad        = fn->wquad;
  wline        = fn->wline;
}

double font_rep::get_left_slope  (string s) { (void) s; return slope; }
double font_rep::get_right_slope (string s) { (void) s; return slope; }
SI     font_rep::get_left_correction  (string s) { (void) s; return 0; }
SI     font_rep::get_right_correction (string s) { (void) s; return 0; }

void
font_rep::get_xpositions (string s, SI* xpos) {
  int i= 0;
  SI  x= 0;
  metric ex;
  while (i < N(s)) {
    if (s[i] == '<')
      while ((i < N(s)) && (s[i] != '>')) {
	i++;
	xpos[i]= x;
      }
    i++;
    get_extents (s (0, i), ex);
    x= ex->x2;
    xpos[i]= x;
  }
}

void
font_rep::var_get_extents (string s, metric& ex) {
  bool flag=true;
  int start=0, end;
  get_extents ("", ex);
  while (start<N(s)) {
    for (end=start; (end<N(s)) && (s[end]!=' '); end++);
    if (start<end) {
      metric ey;
      get_extents (s (start, end), ey);
      if (flag) {
	ex->x3= ey->x3+ ex->x2; ex->y3= ey->y3+ ex->x2;
	ex->x4= ey->x4; ex->y4= ey->y4;
	ex->x2 += ey->x2;
	flag= false;
      }
      else {
	ex->x3= min (ex->x3, ex->x2+ ey->x3);
	ex->x4= max (ex->x4, ex->x2+ ey->x4);
	ex->y3= min (ex->y3, ey->y3);
	ex->y4= max (ex->y4, ey->y4);
	ex->x2 += ey->x2;
      }
    }
    for (; (end<N(s)) && (s[end]==' '); end++) ex->x2 += spc->def;
    start= end;
  }
}

void
font_rep::var_get_xpositions (string s, SI* xpos) {
  (void) s; (void) xpos;
  fatal_error ("Not yet implemented", "font_rep::var_get_xpositions");
}

void
font_rep::var_draw (ps_device dev, string s, SI x, SI y) {
  SI dx=0;
  int start=0, end;
  while (start<N(s)) {
    for (end=start; (end<N(s)) && (s[end]!=' '); end++);
    if (start<end) {
      metric ex;
      draw (dev, s (start, end), x+dx, y);
      get_extents (s (start, end), ex);
      dx += ex->x2;
    }
    for (; (end<N(s)) && (s[end]==' '); end++) dx += spc->def;
    start= end;
  }
}

glyph
font_rep::get_glyph (string s) {
  fatal_error ("No bitmap available for " * s, "font_rep::get_glyph");
  return glyph(); // avoids error message when C++ compiler behaves badly
}

/******************************************************************************
* Joining fonts: the global font properties are taken from the first font.
* If a string needs to be converted to a box, we first try to convert
* it using the first font, and next using the second one.
******************************************************************************/

struct join_font_rep: font_rep {
  font fn1, fn2;

  join_font_rep (string name, font fn1, font fn2);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (ps_device dev, string s, SI x, SI y);
};

join_font_rep::join_font_rep (string name, font fn1b, font fn2b):
  font_rep (fn1b->dis, name, fn1b), fn1 (fn1b), fn2 (fn2b)
{
  enc= join (fn1->enc, fn2->enc);
}

void
join_font_rep::get_extents (string s, metric& ex) {
  if (fn1->enc->valid (s)) fn1->get_extents (s, ex);
  else fn2->get_extents (s, ex);
}

void
join_font_rep::get_xpositions (string s, SI* xpos) {
  if (fn1->enc->valid (s)) fn1->get_xpositions (s, xpos);
  else fn2->get_xpositions (s, xpos);
}

void
join_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  if (fn1->enc->valid (s)) fn1->draw (dev, s, x, y);
  else fn2->draw (dev, s, x, y);
}

font
join (font fn1, font fn2) {
  string name= fn1->res_name * "|" * fn2->res_name;
  return make (font, name, new join_font_rep (name, fn1, fn2));
}

/******************************************************************************
* Restricting fonts: restricting of the fonts underlying encoding,
* we decrease the number of valid strings which can be printed in the font
******************************************************************************/

struct subfont_rep: font_rep {
  font fn;
  subfont_rep (string name, font fn, encoding enc);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (ps_device dev, string s, SI x, SI y);
};

subfont_rep::subfont_rep (string name, font fn2, encoding enc2):
  font_rep (fn2->dis, name, fn2), fn (fn2)
{
  enc= enc2;
}

void
subfont_rep::get_extents (string s, metric& ex) {
  fn->get_extents (s, ex);
}

void
subfont_rep::get_xpositions (string s, SI* xpos) {
  fn->get_xpositions (s, xpos);
}

void
subfont_rep::draw (ps_device dev, string s, SI x, SI y) {
  fn->draw (dev, s, x, y);
}

font
subfont (font fn, encoding enc) {
  string name= fn->res_name * ":" * enc->res_name;
  return make (font, name, new subfont_rep (name, fn, enc));
}

/******************************************************************************
* Error font: used to draw unindentified characters
******************************************************************************/

struct error_font_rep: font_rep {
  font fn;
  error_font_rep (string name, font fn);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (ps_device dev, string s, SI x, SI y);
};

error_font_rep::error_font_rep (string name, font fnb):
  font_rep (fnb->dis, name, fnb), fn (fnb)
{
  enc= fn->enc;
}

void
error_font_rep::get_extents (string s, metric& ex) {
  fn->get_extents (s, ex);
}

void
error_font_rep::get_xpositions (string s, SI* xpos) {
  fn->get_xpositions (s, xpos);
}

void
error_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  dev->set_color (fn->dis->red);
  fn->draw (dev, s, x, y);
}

font
error_font (font fn) {
  string name= "error-" * fn->res_name;
  return make (font, name, new error_font_rep (name, fn));
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

int
script (int sz, int level) {
  int i;
  if (level<0) level=0;
  if (level>2) level=2;
  for (i=0; i<level; i++) sz= (sz*2+2)/3;
  return sz;
}
