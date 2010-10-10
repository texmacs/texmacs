
/******************************************************************************
* MODULE     : font.cpp
* DESCRIPTION: fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "gui.hpp"

RESOURCE_CODE(font);

/******************************************************************************
* Constructors for fonts
******************************************************************************/

font_rep::font_rep (string s):
  rep<font> (s),
  type      (FONT_TYPE_TEX),
  spc       (0),
  extra     (0)
{
}

font_rep::font_rep (string s, font fn):
  rep<font>    (s),
  type         (fn->type),
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
  wfn          = fn->wfn;
  wline        = fn->wline;
  wquad        = fn->wquad;
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
    for (end=start; (end<N(s)) && (s[end]!=' '); end++) {}
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
  FAILED ("not yet implemented");
}

void
font_rep::var_draw (renderer ren, string s, SI x, SI y) {
  SI dx=0;
  int start=0, end;
  while (start<N(s)) {
    for (end=start; (end<N(s)) && (s[end]!=' '); end++) {}
    if (start<end) {
      metric ex;
      draw (ren, s (start, end), x+dx, y);
      get_extents (s (start, end), ex);
      dx += ex->x2;
    }
    for (; (end<N(s)) && (s[end]==' '); end++) dx += spc->def;
    start= end;
  }
}

glyph
font_rep::get_glyph (string s) {
  cerr << "glyph name: " << s << "\n";
  FAILED ("no bitmap available");
  return glyph ();
}

/******************************************************************************
* Error font: used to draw unindentified characters
******************************************************************************/

struct error_font_rep: font_rep {
  font fn;
  error_font_rep (string name, font fn);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
};

error_font_rep::error_font_rep (string name, font fnb):
  font_rep (name, fnb), fn (fnb) {}

void
error_font_rep::get_extents (string s, metric& ex) {
  fn->get_extents (s, ex);
}

void
error_font_rep::get_xpositions (string s, SI* xpos) {
  fn->get_xpositions (s, xpos);
}

void
error_font_rep::draw (renderer ren, string s, SI x, SI y) {
  ren->set_color (red);
  fn->draw (ren, s, x, y);
}

font
error_font (font fn) {
  string name= "error-" * fn->res_name;
  return make (font, name, tm_new<error_font_rep> (name, fn));
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
