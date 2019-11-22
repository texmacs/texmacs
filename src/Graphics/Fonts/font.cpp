
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
#include "iterator.hpp"
#include "file.hpp"
#include "convert.hpp"

RESOURCE_CODE(font);

hashmap<string,double> lsub_guessed_table ();
hashmap<string,double> lsup_guessed_table ();
hashmap<string,double> rsub_guessed_table ();
hashmap<string,double> rsup_guessed_table ();
hashmap<string,double> above_guessed_table ();
hashmap<string,double> below_guessed_table ();

/******************************************************************************
* Constructors for fonts
******************************************************************************/

static int
get_math_type (string s) {
  s= locase_all (s);
  if (starts (s, "unicode:")) s= s (8, N(s));
  if (starts (s, "texgyre")) s= s (7, N(s));
  if (starts (s, "stix-") || starts (s, "stixintegrals"))
    return MATH_TYPE_STIX;
  if (starts (s, "bonum-") || starts (s, "pagella-") ||
      starts (s, "schola-") ||starts (s, "termes-"))
    return MATH_TYPE_TEX_GYRE;
  return MATH_TYPE_NORMAL;
}

font_rep::font_rep (string s):
  rep<font> (s),
  type      (FONT_TYPE_TEX),
  math_type (get_math_type (s)),
  spc       (0),
  extra     (0),
  mspc      (0),
  last_zoom (0.0),
  zoomed_fn (NULL),
  global_lsub_correct (0),
  global_lsup_correct (0),
  global_rsub_correct (0),
  global_rsup_correct (0),
  lsub_correct (0.0),
  lsup_correct (0.0),
  rsub_correct (0.0),
  rsup_correct (0.0),
  above_correct (0.0),
  below_correct (0.0),
  protrusion_maps (-1)
{
  lsub_correct = lsub_guessed_table ();
  lsup_correct = lsup_guessed_table ();
  rsub_correct = rsub_guessed_table ();
  rsup_correct = rsup_guessed_table ();
  above_correct= above_guessed_table ();
  below_correct= below_guessed_table ();
}

font_rep::font_rep (string s, font fn):
  rep<font>    (s),
  type         (fn->type),
  math_type    (fn->math_type),
  size         (fn->size),
  design_size  (fn->design_size),
  display_size (fn->display_size),
  slope        (fn->slope),
  spc          (fn->spc),
  extra        (fn->extra),
  mspc         (fn->mspc),
  sep          (fn->sep),
  last_zoom    (0.0),
  zoomed_fn    (NULL),
  global_lsub_correct (0),
  global_lsup_correct (0),
  global_rsub_correct (0),
  global_rsup_correct (0),
  lsub_correct (0.0),
  lsup_correct (0.0),
  rsub_correct (0.0),
  rsup_correct (0.0),
  protrusion_maps (-1)
{
  lsub_correct = lsub_guessed_table ();
  lsup_correct = lsup_guessed_table ();
  rsub_correct = rsub_guessed_table ();
  rsup_correct = rsup_guessed_table ();
  above_correct= above_guessed_table ();
  below_correct= below_guessed_table ();
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
  hpt          = fn->hpt;
  wfn          = fn->wfn;
  wline        = fn->wline;
  wquad        = fn->wquad;
}

void
font_rep::draw (renderer ren, string s, SI x, SI y, SI xk, bool ext) {
  if (ren->zoomf == 1.0 || ren->is_printer ()) {
    if (ext) draw_fixed (ren, s, x, y, xk);
    else draw_fixed (ren, s, x, y);
  }
  else if (ren->zoomf != last_zoom) {
    last_zoom= ren->zoomf;
    zoomed_fn= magnify (ren->zoomf);
    draw (ren, s, x, y, xk, ext);
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
    SI     old_thicken= ren->thicken;
    SI     old_pixel       = ren->pixel;
    SI     old_retina_pixel= ren->retina_pixel;

    ren->ox     = (SI) tm_round (old_ox  * old_zoomf);
    ren->oy     = (SI) tm_round (old_oy  * old_zoomf);
    //ren->cx1    = (SI) ::floor (old_cx1 * old_zoomf);
    //ren->cx2    = (SI) ::floor (old_cx2 * old_zoomf);
    //ren->cy1    = (SI) ::ceil  (old_cy1 * old_zoomf);
    //ren->cy2    = (SI) ::ceil  (old_cy2 * old_zoomf);
    ren->cx1    = (SI) tm_round (old_cx1 * old_zoomf);
    ren->cx2    = (SI) tm_round (old_cx2 * old_zoomf);
    ren->cy1    = (SI) tm_round (old_cy1 * old_zoomf);
    ren->cy2    = (SI) tm_round (old_cy2 * old_zoomf);
    ren->zoomf  = 1.0;
    ren->shrinkf= std_shrinkf;
    ren->brushpx= ren->pixel;
    ren->thicken= (std_shrinkf >> 1) * PIXEL;
    ren->pixel       = std_shrinkf * PIXEL;
    ren->retina_pixel= std_shrinkf * PIXEL;

    SI xx= (SI) tm_round (x * old_zoomf);
    SI yy= (SI) tm_round (y * old_zoomf);
    if (ext) {
      SI kk= (SI) tm_round (xk * old_zoomf);
      zoomed_fn->draw_fixed (ren, s, xx, yy, kk);
    }
    else zoomed_fn->draw_fixed (ren, s, xx, yy);

    ren->ox     = old_ox;
    ren->oy     = old_oy;
    ren->cx1    = old_cx1;
    ren->cx2    = old_cx2;
    ren->cy1    = old_cy1;
    ren->cy2    = old_cy2;
    ren->zoomf  = old_zoomf;
    ren->shrinkf= old_shrinkf;
    ren->brushpx= -1;
    ren->thicken= old_thicken;
    ren->pixel       = old_pixel;
    ren->retina_pixel= old_retina_pixel;
  }
}

void
font_rep::draw (renderer ren, string s, SI x, SI y) {
  draw (ren, s, x, y, 0, false);
}

void
font_rep::draw (renderer ren, string s, SI x, SI y, SI xk) {
  draw (ren, s, x, y, xk, true);
}

void
font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool ligf) {
  (void) ligf;
  draw_fixed (ren, s, x, y);
}

void
font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  int i= 0;
  while (i<N(s)) {
    int old= i;
    tm_char_forwards (s, i);
    draw_fixed (ren, s (old, i), x + xpos[old], y, false);
  }
  STACK_DELETE_ARRAY (xpos);  
}

double font_rep::get_left_slope  (string s) { (void) s; return slope; }
double font_rep::get_right_slope (string s) { (void) s; return slope; }
SI     font_rep::get_left_correction  (string s) { (void) s; return 0; }
SI     font_rep::get_right_correction (string s) { (void) s; return 0; }

SI
font_rep::get_lsub_correction (string s) {
  //cout << "lsub " << this->res_name << ", " << s << LF;
  SI r= -get_left_correction (s) + global_lsub_correct;
  if (lsub_correct->contains (s))
    r += (SI) (lsub_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[0]) &&
           lsub_correct->contains (s (0, 1)))
    r += (SI) (lsub_correct[s (0, 1)] * wfn);
  return r;
}

SI
font_rep::get_lsup_correction (string s) {
  //cout << "lsup " << this->res_name << ", " << s << LF;
  //SI r= get_right_correction (s) + global_lsup_correct;
  SI r= global_lsup_correct;
  if (lsup_correct->contains (s))
    r += (SI) (lsup_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[0]) &&
           lsup_correct->contains (s (0, 1)))
    r += (SI) (lsup_correct[s (0, 1)] * wfn);
  return r;
}

SI
font_rep::get_rsub_correction (string s) {
  //cout << "rsub " << this->res_name << ", " << s << LF;
  SI r= global_rsub_correct;
  if (rsub_correct->contains (s))
    r += (SI) (rsub_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[N(s)-1]) &&
           rsub_correct->contains (s (N(s)-1, N(s))))
    r += (SI) (rsub_correct[s (N(s)-1, N(s))] * wfn);
  return r;
}

SI
font_rep::get_rsup_correction (string s) {
  //cout << "rsup " << this->res_name << ", " << s << LF;
  SI r= get_right_correction (s) + global_rsup_correct;
  if (rsup_correct->contains (s))
    r += (SI) (rsup_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[N(s)-1]) &&
           rsup_correct->contains (s (N(s)-1, N(s))))
    r += (SI) (rsup_correct[s (N(s)-1, N(s))] * wfn);
  return r;
}

SI
font_rep::get_wide_correction (string s, int mode) {
  if (mode > 0 && above_correct->contains (s))
    return (SI) (above_correct[s] * wfn);
  else if (mode < 0 && below_correct->contains (s))
    return (SI) (below_correct[s] * wfn);
  else return 0;
}

void
font_rep::get_extents (string s, metric& ex, bool ligf) {
  if (ligf) get_extents (s, ex);
  else get_extents (s, ex, 0);
}

void
font_rep::get_extents (string s, metric& ex, SI xk) {
  get_extents (s, ex);
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  SI d= xpos[N(s)] - ex->x2;
  ex->x2 += d;
  ex->x4 += d - xk;
  STACK_DELETE_ARRAY (xpos);
}

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
    if (i < N(s)) i++;
    get_extents (s (0, i), ex);
    x= ex->x2;
    xpos[i]= x;
  }
}

void
font_rep::get_xpositions (string s, SI* xpos, bool ligf) {
  (void) ligf;
  get_xpositions (s, xpos);
}

void
font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  get_xpositions (s, xpos, false);
  int n= tm_string_length (s);
  if (n == 0) return;
  int i= 0, count= 0;
  xpos[0]= xk;
  while (i < N(s)) {
    SI dx= (2*count + 1) * xk;
    if (s[i] == '<')
      while ((i < N(s)) && (s[i] != '>')) {
        i++;
        xpos[i] += dx;
      }
    if (i < N(s)) i++;
    count++;
    if (i == N(s)) dx= 2 * count * xk;
    else dx= (2*count + 1) * xk;
    xpos[i] += dx;
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

//bool get_glyph_fatal= true;
bool get_glyph_fatal= false;

void
font_rep::advance_glyph (string s, int& pos, bool ligf) {
  (void) ligf;
  tm_char_forwards (s, pos);
}

glyph
font_rep::get_glyph (string s) {
  if (get_glyph_fatal) {
    failed_error << "glyph name: " << s << "\n";
    failed_error << "font      : " << res_name << "\n";    
    FAILED ("no bitmap available");
  }
  else {
    cout << "TeXmacs] warning, no bitmap available for " << s << "\n";
    cout << "TeXmacs]   in font " << res_name << "\n";
  }
  return glyph (0, 0, 0, 0);
}

int
font_rep::index_glyph (string s, font_metric& fnm, font_glyphs& fng) {
  (void) fnm; (void) fng;
  if (get_glyph_fatal) {
    failed_error << "glyph name: " << s << "\n";
    failed_error << "font      : " << res_name << "\n";    
    FAILED ("no bitmap available");
  }
  else {
    cout << "TeXmacs] warning, no glyph index available for " << s << "\n";
    cout << "TeXmacs]   in font " << res_name << "\n";
  }
  return -1;
}

font
font_rep::poor_magnify (double zoomx, double zoomy) {
  return poor_stretched_font (this, zoomx, zoomy);
}

font
font_rep::magnify (double zoom) {
  return magnify (zoom, zoom);
}

/******************************************************************************
* Error font: used to draw unindentified characters
******************************************************************************/

struct error_font_rep: font_rep {
  font fn;
  error_font_rep (string name, font fn);
  bool supports (string c);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  font magnify (double zoomx, double zoomy);
};

error_font_rep::error_font_rep (string name, font fnb):
  font_rep (name, fnb), fn (fnb) {}

bool
error_font_rep::supports (string c) {
  (void) c;
  return true;
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
error_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  ren->set_pencil (red);
  fn->draw_fixed (ren, s, x, y);
}

font
error_font_rep::magnify (double zoomx, double zoomy) {
  return error_font (fn->magnify (zoomx, zoomy));
}

font
error_font (font fn) {
  string name= "error-" * fn->res_name;
  return make (font, name, tm_new<error_font_rep> (name, fn));
}

/******************************************************************************
* System dependent fonts
******************************************************************************/

#ifndef X11TEXMACS

font
x_font (string family, int size, int dpi) {
  (void) family; (void) size; (void) dpi;
  return font ();
}

#endif

#ifndef QTTEXMACS

font
qt_font (string family, int size, int dpi) {
  (void) family; (void) size; (void) dpi;
  return font ();
}

#endif

/******************************************************************************
* Miscellaneous
******************************************************************************/

static hashmap<string,font> larger_font_table;
bool has_poor_rubber= true;

bool
use_poor_rubber (font fn) {
  return has_poor_rubber && fn->type == FONT_TYPE_UNICODE &&
    !starts (fn->res_name, "stix-");
}

static font
make_rubber_font (font fn) {
  if (starts (fn->res_name, "stix-"))
    return rubber_stix_font (fn);
  else if (occurs ("mathlarge=", fn->res_name) ||
           occurs ("mathrubber=", fn->res_name))
    return fn;
  else if (has_poor_rubber && fn->type == FONT_TYPE_UNICODE)
    return poor_rubber_font (fn);
  else if (fn->type == FONT_TYPE_UNICODE)
    return rubber_unicode_font (fn);
  else
    return fn;
}

font
rubber_font (font base) {
  if (larger_font_table->contains (base->res_name))
    return larger_font_table (base->res_name);
  font larger= make_rubber_font (base);
  larger_font_table (base->res_name)= larger;
  return larger;
}

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
  if (tt_font_exists ("FandolSong-Regular")) return "FandolSong";
  if (tt_font_exists ("simsun")) return "simsun";
  if (tt_font_exists ("fireflysung")) return "fireflysung";
  if (tt_font_exists ("uming")) return "uming";
  if (tt_font_exists ("儷黑 Pro")) return "lihei";
  if (tt_font_exists ("华文细黑")) return "heiti";
  if (tt_font_exists ("SimSun")) return "apple-simsun";
  return "roman";
}

string
default_japanese_font_name () {
  if (tt_font_exists ("ipam")) return (new_fonts? "IPAMincho": "modern");
  if (tt_font_exists ("sazanami")) return "sazanami";
  if (tt_font_exists ("ttf-japanese-gothic")) return "ttf-japanese";
  if (tt_font_exists ("ヒラギノ明朝 ProN W6")) return "kaku";
  if (tt_font_exists ("MS PGothic")) return "ms-gothic";
  if (tt_font_exists ("MS PMincho")) return "ms-mincho";
  return "roman";  
}

string
default_korean_font_name () {
  if (tt_font_exists ("unbatang")) return (new_fonts? "UnBatang": "modern");
  if (tt_font_exists ("UnBatang")) return (new_fonts? "UnBatang": "modern");
  if (tt_font_exists ("AppleGothic")) return "apple-gothic";
  if (tt_font_exists ("Gulim")) return "gulim";
  return "roman";
}
