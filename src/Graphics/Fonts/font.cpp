
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
#include "Freetype/tt_file.hpp"

RESOURCE_CODE(font);

/******************************************************************************
* Constructors for fonts
******************************************************************************/

font_rep::font_rep (string s):
  rep<font> (s),
  type      (FONT_TYPE_TEX),
  spc       (0),
  extra     (0),
  last_zoom (0.0),
  zoomed_fn (NULL)
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
  sep          (fn->sep),
  last_zoom    (0.0),
  zoomed_fn    (NULL)
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

void
font_rep::draw (renderer ren, string s, SI x, SI y) {
  if (ren->zoomf == 1.0 || !ren->is_screen)
    draw_fixed (ren, s, x, y);
  else if (ren->zoomf != last_zoom) {
    last_zoom= ren->zoomf;
    zoomed_fn= magnify (ren->zoomf);
    draw (ren, s, x, y);
  }
  else {
    // FIXME: low level rendering hack
    SI     old_ox     = ren->ox;
    SI     old_oy     = ren->oy;
    SI     old_cx1    = ren->cx1;
    SI     old_cy1    = ren->cy1;
    SI     old_cx2    = ren->cx2;
    SI     old_cy2    = ren->cy2;
    double old_zoomf  = ren->zoomf;
    int    old_shrinkf= ren->shrinkf;
    SI     old_pixel  = ren->pixel;
    SI     old_thicken= ren->thicken;

    ren->ox     = (SI) ::round (old_ox  * old_zoomf);
    ren->oy     = (SI) ::round (old_oy  * old_zoomf);
    ren->cx1    = (SI) ::round (old_cx1 * old_zoomf);
    ren->cx2    = (SI) ::round (old_cx2 * old_zoomf);
    ren->cy1    = (SI) ::round (old_cy1 * old_zoomf);
    ren->cy2    = (SI) ::round (old_cy2 * old_zoomf);
    ren->zoomf  = 1.0;
    ren->shrinkf= std_shrinkf;
    ren->pixel  = std_shrinkf * PIXEL;
    ren->thicken= (std_shrinkf >> 1) * PIXEL;

    SI xx= (SI) round (x * old_zoomf);
    SI yy= (SI) round (y * old_zoomf);
    zoomed_fn->draw_fixed (ren, s, xx, yy);

    ren->ox     = old_ox;
    ren->oy     = old_oy;
    ren->cx1    = old_cx1;
    ren->cx2    = old_cx2;
    ren->cy1    = old_cy1;
    ren->cy2    = old_cy2;
    ren->zoomf  = old_zoomf;
    ren->shrinkf= old_shrinkf;
    ren->pixel  = old_pixel;
    ren->thicken= old_thicken;
  }
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
  void draw_fixed (renderer ren, string s, SI x, SI y);
  font magnify (double zoom);
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
error_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  ren->set_color (red);
  fn->draw_fixed (ren, s, x, y);
}

font
error_font_rep::magnify (double zoom) {
  return error_font (fn->magnify (zoom));
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

string
default_chinese_font_name () {
  if (tt_font_exists ("fireflysung")) return "fireflysung";
  if (tt_font_exists ("uming")) return "uming";
  if (tt_font_exists ("儷黑 Pro")) return "lihei";
  if (tt_font_exists ("华文细黑")) return "heiti";
  if (tt_font_exists ("simsun")) return "simsun";
  if (tt_font_exists ("SimSun")) return "apple-simsun";
  return "roman";
}

string
default_japanese_font_name () {
  if (tt_font_exists ("ipam")) return "modern";
  if (tt_font_exists ("sazanami")) return "sazanami";
  if (tt_font_exists ("ttf-japanese-gothic")) return "ttf-japanese";
  if (tt_font_exists ("ヒラギノ明朝 ProN W6")) return "kaku";
  if (tt_font_exists ("MS PGothic")) return "ms-gothic";
  if (tt_font_exists ("MS PMincho")) return "ms-mincho";
  return "roman";  
}

string
default_korean_font_name () {
  if (tt_font_exists ("unbatang")) return "modern";
  if (tt_font_exists ("UnBatang")) return "modern";
  if (tt_font_exists ("AppleGothic")) return "apple-gothic";
  if (tt_font_exists ("Gulim")) return "gulim";
  return "roman";
}
