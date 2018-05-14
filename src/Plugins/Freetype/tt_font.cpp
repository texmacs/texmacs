
/******************************************************************************
* MODULE     : tt_font.cpp
* DESCRIPTION: True Type fonts (using FreeType II)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_face.hpp"

#ifdef USE_FREETYPE

#define std_dpi 600
#define std_pixel (std_shrinkf*256)
#define ROUND(l) ((l*hdpi+(std_dpi>>1))/std_dpi)
#define FLOOR(l) ((((l*hdpi)/std_dpi)/std_pixel)*std_pixel)
#define CEIL(l) (((((l*hdpi+(std_dpi-1))/std_dpi)+std_pixel-1)/std_pixel)*std_pixel)

font tt_font (string family, int size, int hdpi, int vdpi);

/******************************************************************************
* True Type fonts
******************************************************************************/

struct tt_font_rep: font_rep {
  string      family;
  int         hdpi;
  int         vdpi;
  font_metric fnm;
  font_glyphs fng;

  tt_font_rep (string name, string family, int size, int hdpi, int vdpi);

  bool  supports (string c);
  void  get_extents (string s, metric& ex);
  void  get_xpositions (string s, SI* xpos);
  void  draw_fixed (renderer ren, string s, SI x, SI y);
  font  magnify (double zoomx, double zoomy);
  void  advance_glyph (string s, int& pos, bool ligf);
  glyph get_glyph (string s);
  int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

tt_font_rep::tt_font_rep (string name, string family2, int size2,
                          int hdpi2, int vdpi2):
  font_rep (name), family (family2), hdpi (hdpi2), vdpi (vdpi2)
{
  size= size2;
  fnm = tt_font_metric (family, size, std_dpi, (std_dpi * vdpi) / hdpi);
  fng = tt_font_glyphs (family, size, hdpi, vdpi);
  if (fnm->bad_font_metric || fng->bad_font_glyphs) {
    fnm= std_font_metric (res_name, NULL, 0, -1);
    fng= std_font_glyphs (res_name, NULL, 0, -1);
    if (DEBUG_AUTO)
      debug_fonts << "Font " << family << " " << size << "pt "
                  << "at " << hdpi << " dpi could not be loaded\n";
    
  }

  // get main font parameters
  metric ex;
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  display_size = y2-y1;
  design_size  = size << 8;

  // get character dimensions
  get_extents ("x", ex);
  yx           = ex->y2;
  get_extents ("M", ex);
  wquad        = ex->x2;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute other widths
  wpt          = (hdpi*PIXEL)/72;
  hpt          = (vdpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc= space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (3*(ex->x2-ex->x1))>>1);
  extra   = spc/2;
  math_spc= spc;
  sep     = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size) - 0.05;
  if (slope<0.15) slope= 0.0;
}

/******************************************************************************
* Routines for font
******************************************************************************/

bool
tt_font_rep::supports (string c) {
  if (N(c) == 1) return fnm->exists ((unsigned int) c[0]);
  if (c == "<less>") return fnm->exists ((unsigned int) '<');
  if (c == "<gtr>") return fnm->exists ((unsigned int) '>');
  return false;
}

void
tt_font_rep::get_extents (string s, metric& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    QN c= s[0];
    metric_struct* first= fnm->get (c);
    ex->x1= ROUND (first->x1);
    ex->y1= ROUND (first->y1);
    ex->x2= ROUND (first->x2);
    ex->y2= ROUND (first->y2);
    ex->x3= FLOOR (first->x3);
    ex->y3= FLOOR (first->y3);
    ex->x4= CEIL  (first->x4);
    ex->y4= CEIL  (first->y4);
    SI x= ROUND (first->x2);

    int i;
    for (i=1; i<N(s); i++) {
      if (i>0) x += ROUND (fnm->kerning ((QN) s[i-1], (QN) s[i]));
      QN c= s[i];
      metric_struct* next= fnm->get (c);
      ex->x1= min (ex->x1, x+ ROUND (next->x1));
      ex->y1= min (ex->y1, ROUND (next->y1));
      ex->x2= max (ex->x2, x+ ROUND (next->x2));
      ex->y2= max (ex->y2, ROUND (next->y2));
      ex->x3= min (ex->x3, x+ FLOOR (next->x3));
      ex->y3= min (ex->y3, FLOOR (next->y3));
      ex->x4= max (ex->x4, x+ CEIL  (next->x4));
      ex->y4= max (ex->y4, CEIL  (next->y4));
      x += ROUND (next->x2);
    }
  }
}

void
tt_font_rep::get_xpositions (string s, SI* xpos) {
  register int i, n= N(s);
  if (n == 0) return;
  
  register SI x= 0;
  for (i=0; i<N(s); i++) {
    if (i>0) x += ROUND (fnm->kerning ((QN) s[i-1], (QN) s[i]));
    metric_struct* next= fnm->get ((QN) s[i]);
    x += ROUND (next->x2);
    xpos[i+1]= x;
  }
}

void
tt_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  if (N(s)!=0) {
    int i;
    for (i=0; i<N(s); i++) {
      if (i>0) x += ROUND (fnm->kerning ((QN) s[i-1], (QN) s[i]));
      QN c= s[i];
      ren->draw (c, fng, x, y);
      metric_struct* ex= fnm->get (c);
      x += ROUND (ex->x2);
    }
  }
}

font
tt_font_rep::magnify (double zoomx, double zoomy) {
  return tt_font (family, size,
                  (int) tm_round (hdpi * zoomx),
                  (int) tm_round (vdpi * zoomy));
}

void
tt_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  (void) ligf;
  if (pos < N(s)) pos++;
}

glyph
tt_font_rep::get_glyph (string s) {
  if (N(s)!=1) return font_rep::get_glyph (s);
  int c= ((QN) s[0]);
  glyph gl= fng->get (c);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

int
tt_font_rep::index_glyph (string s, font_metric& rm, font_glyphs& rg) {
  if (N(s)!=1) return font_rep::index_glyph (s, rm, rg);
  int c= ((QN) s[0]);
  glyph gl= fng->get (c);
  if (is_nil (gl)) return font_rep::index_glyph (s, rm, rg);
  rm= fnm;
  rg= fng;
  return c;
}

/******************************************************************************
* Interface
******************************************************************************/

font
tt_font (string family, int size, int hdpi, int vdpi) {
  string name= "tt:" * family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  return make (font, name,
               tm_new<tt_font_rep> (name, family, size, hdpi, vdpi));
}

font
tt_font (string family, int size, int dpi) {
  return tt_font (family, size, dpi, dpi);
}

#else

font
tt_font (string family, int size, int dpi) {
  string name= "tt:" * family * as_string (size) * "@" * as_string (dpi);
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
