
/******************************************************************************
* MODULE     : unicode_font.cpp
* DESCRIPTION: True Type fonts (using FreeType II)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_face.hpp"
#include "analyze.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct unicode_font_rep: font_rep {
  font_metric fnm;
  font_glyphs fng;

  unicode_font_rep (string name, string family, int size, int dpi);

  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
  glyph get_glyph (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

#define conv(x) ((SI) (((double) (x))*unit))

unicode_font_rep::unicode_font_rep (string name,
  string family, int size2, int dpi):
  font_rep (name)
{
  size= size2;
  fnm = tt_font_metric (family, size, dpi);
  fng = tt_font_glyphs (family, size, dpi);
  if (fnm->bad_font_metric || fng->bad_font_glyphs) {
    fnm= std_font_metric (res_name, NULL, 0, -1);
    fng= std_font_glyphs (res_name, NULL, 0, -1);
    if (DEBUG_AUTO)
      cout << "TeXmacs] Font " << family << " " << size
	   << "pt at " << dpi << " dpi could not be loaded\n";
    
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
  wpt          = (dpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (ex->x2-ex->x1)<<1);
  extra= spc;
  sep  = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size);
  if (slope<0.15) slope= 0.0;
}

/******************************************************************************
* Routines for font
******************************************************************************/

static unsigned int
read_unicode_char (string s, int& i) {
  if (s[i] == '<') {
    i++; if (s[i] == '#') i++;
    int start= i;
    while (s[i] != '>') i++;
    return (unsigned int) from_hexadecimal (s (start, i++));
  }
  else return (unsigned int) s[i++];
}

void
unicode_font_rep::get_extents (string s, metric& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    int i= 0, n= N(s);
    unsigned int uc= read_unicode_char (s, i);
    metric_struct* first= fnm->get (uc);
    ex->x1= first->x1; ex->y1= first->y1;
    ex->x2= first->x2; ex->y2= first->y2;
    ex->x3= first->x3; ex->y3= first->y3;
    ex->x4= first->x4; ex->y4= first->y4;
    SI x= first->x2;

    while (i<n) {
      uc= read_unicode_char (s, i);
      metric_struct* next= fnm->get (uc);
      ex->x1= min (ex->x1, x+ next->x1); ex->y1= min (ex->y1, next->y1);
      ex->x2= max (ex->x2, x+ next->x2); ex->y2= max (ex->y2, next->y2);
      ex->x3= min (ex->x3, x+ next->x3); ex->y3= min (ex->y3, next->y3);
      ex->x4= max (ex->x4, x+ next->x4); ex->y4= max (ex->y4, next->y4);
      x += next->x2;
    }
  }
}

void
unicode_font_rep::get_xpositions (string s, SI* xpos) {
  int i= 0, n= N(s);
  if (n == 0) return;
  
  register SI x= 0;
  while (i<n) {
    int start= i;
    metric_struct* next= fnm->get (read_unicode_char (s, i));
    for (int j= start; j<i; j++) xpos[j]= x;
    x += next->x2;
  }
  xpos[n]= x;
}

void
unicode_font_rep::draw (renderer ren, string s, SI x, SI y) {
  int i= 0, n= N(s);
  while (i<n) {
    unsigned int uc= read_unicode_char (s, i);
    ren->draw (uc, fng, x, y);
    metric_struct* ex= fnm->get (uc);
    x += ex->x2;
  }
}

glyph
unicode_font_rep::get_glyph (string s) {
  int i= 0, n= N(s);
  unsigned int uc= read_unicode_char (s, i);
  if (i != n) return font_rep::get_glyph (s);
  glyph gl= fng->get (uc);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

/******************************************************************************
* Interface
******************************************************************************/

font
unicode_font (string family, int size, int dpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<unicode_font_rep> (name, family, size, dpi));
}

#else

font
unicode_font (string family, int size, int dpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string(dpi);
  cerr << "\n\nFont name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
