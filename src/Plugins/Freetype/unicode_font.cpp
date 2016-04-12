
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
#include "converter.hpp"

#ifdef USE_FREETYPE

#define std_dpi 600
#define std_pixel (std_shrinkf*256)
#define ROUND(l) ((l*hdpi+(std_dpi>>1))/std_dpi)
#define FLOOR(l) ((((l*hdpi)/std_dpi)/std_pixel)*std_pixel)
#define CEIL(l) (((((l*hdpi+(std_dpi-1))/std_dpi)+std_pixel-1)/std_pixel)*std_pixel)

#define LIGATURE_FF   1
#define LIGATURE_FI   2
#define LIGATURE_FL   4
#define LIGATURE_FT   8
#define LIGATURE_FFI 16
#define LIGATURE_FFL 32
#define LIGATURE_ST  64

font unicode_font (string family, int size, int hdpi, int vdpi);

/******************************************************************************
* True Type fonts
******************************************************************************/

struct unicode_font_rep: font_rep {
  string      family;
  int         hdpi;
  int         vdpi;
  font_metric fnm;
  font_glyphs fng;
  int         ligs;

  unicode_font_rep (string name, string family, int size, int hdpi, int vdpi);

  unsigned int ligature_replace (unsigned int c, string s, int& i);
  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos, bool ligf);
  void   get_xpositions (string s, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction  (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

unicode_font_rep::unicode_font_rep (string name,
  string family2, int size2, int hdpi2, int vdpi2):
    font_rep (name), family (family2), hdpi (hdpi2), vdpi (vdpi2), ligs (0)
{
  type= FONT_TYPE_UNICODE;
  size= size2;
  fnm = tt_font_metric (family, size, std_dpi, (std_dpi * vdpi) / hdpi);
  fng = tt_font_glyphs (family, size, hdpi, vdpi);
  if (fnm->bad_font_metric || fng->bad_font_glyphs) {
    fnm= std_font_metric (res_name, NULL, 0, -1);
    fng= std_font_glyphs (res_name, NULL, 0, -1);
    if (DEBUG_AUTO)
      debug_fonts << "TeXmacs] Font " << family << " " << size << "pt "
                  << "at " << hdpi << " dpi could not be loaded\n";
    
  }

  // get main font parameters
  metric ex;
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  get_extents ("p", ex);
  y1= min (y1, ex->y1);
  y2= max (y2, ex->y2);
  get_extents ("d", ex);
  y1= min (y1, ex->y1);
  y2= max (y2, ex->y2);
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

  // get fraction bar parameters; reasonable compromise between several fonts
  if (supports ("<#2212>")) get_extents ("<#2212>", ex);
  else if (supports ("+")) get_extents ("+", ex);
  else if (supports ("-")) get_extents ("-", ex);
  else get_extents ("x", ex);
  yfrac= (ex->y1 + ex->y2) >> 1;
  if (supports ("<#2212>") || supports ("+") || supports ("-")) {
    wline= ex->y2 - ex->y1;
    if (supports ("<#2212>"));
    else if (supports ("<#2013>")) {
      get_extents ("<#2013>", ex);
      wline= min (wline, ex->y2 - ex->y1);
    }
    wline= max (min (wline, wfn/8), wfn/48);
    if (!supports ("<#2212>")) yfrac += wline/4;
  }
  if (starts (res_name, "unicode:Papyrus.")) wline= (2*wline)/3;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (3*(ex->x2-ex->x1))>>1);
  extra= spc/2;
  sep  = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size) - 0.05;
  if (slope<0.15) slope= 0.0;

  // determine whether we are dealing with a monospaced font
  get_extents ("m", ex);
  SI em= ex->x2 - ex->x1;
  get_extents ("i", ex);
  SI ei= ex->x2 - ex->x1;
  bool mono= (em == ei);

  // available standard ligatures
  if (!mono) {
    if (fnm->exists (0xfb00)) ligs += LIGATURE_FF;
    if (fnm->exists (0xfb01)) ligs += LIGATURE_FI;
    if (fnm->exists (0xfb02)) ligs += LIGATURE_FL;
    if (fnm->exists (0xfb03)) ligs += LIGATURE_FFI;
    if (fnm->exists (0xfb04)) ligs += LIGATURE_FFL;
    if (fnm->exists (0xfb05)) ligs += LIGATURE_FT;
    if (fnm->exists (0xfb06)) ligs += LIGATURE_ST;
  }
  if (family == "Times New Roman")
    ligs= LIGATURE_FI + LIGATURE_FL;
  if (family == "Zapfino")
    ligs= LIGATURE_FF + LIGATURE_FI + LIGATURE_FL + LIGATURE_FFI;
  //cout << "ligs= " << ligs << ", " << family << ", " << size << "\n";
}

/******************************************************************************
* Routines for font
******************************************************************************/

static unsigned int
read_unicode_char (string s, int& i) {
  if (s[i] == '<') {
    i++;
    int start= i;
    while (s[i] != '>') i++;
    if (s[start] == '#') {
      start++;
      return (unsigned int) from_hexadecimal (s (start, i++));
    }
    else {
      string ss= s (start-1, ++i);
      string uu= cork_to_utf8 (ss);
      if (uu == ss) return 0;
      int j= 0;
      return decode_from_utf8 (uu, j);
    }
  }
  else {
    unsigned int c= (unsigned int) s[i++];
    if (c >= 32 && c <= 127) return c;
    string ss= s (i-1, i);
    string uu= cork_to_utf8 (ss);
    int j= 0;
    return decode_from_utf8 (uu, j);
  }
}

unsigned int
unicode_font_rep::ligature_replace (unsigned int uc, string s, int& i) {
  int n= N(s);
  if (((char) uc) == 'f') {
    if (i<n && s[i] == 'i' && (ligs & LIGATURE_FI) != 0) {
      i++; return 0xfb01; }
    else if (i<n && s[i] == 'l' && (ligs & LIGATURE_FL) != 0) {
      i++; return 0xfb02; }
    else if (i<n && s[i] == 't' && (ligs & LIGATURE_FT) != 0) {
      i++; return 0xfb05; }
    else if ((i+1)<n && s[i] == 'f' && s[i+1] == 'i' &&
             (ligs & LIGATURE_FFI) != 0) {
      i+=2; return 0xfb03; }
    else if ((i+1)<n && s[i] == 'f' && s[i+1] == 'l' &&
             (ligs & LIGATURE_FFL) != 0) {
      i+=2; return 0xfb04; }
    else if (i<n && s[i] == 'f' && (ligs & LIGATURE_FF) != 0) {
      i++; return 0xfb00; }
    else return uc;
  }
  else if (((char) uc) == 's') {
    if (i<n && s[i] == 't' && (ligs & LIGATURE_ST) != 0) {
      i++; return 0xfb06; }
    else return uc;
  }
  else return uc;
}

bool
unicode_font_rep::supports (string c) {
  if (N(c) == 0) return false;
  int i= 0;
  unsigned int uc= read_unicode_char (c, i);
  if (uc == 0 || !fnm->exists (uc)) return false;
  if (uc >= 0x42 && uc <= 0x5a && !fnm->exists (0x41)) return false;
  if (uc >= 0x62 && uc <= 0x7a && !fnm->exists (0x61)) return false;
  metric_struct* m= fnm->get (uc);
  return m->x1 < m->x2 && m->y1 < m->y2;
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
    if (ligs > 0 && (((char) uc) == 'f' || ((char) uc) == 's'))
      uc= ligature_replace (uc, s, i);
    metric_struct* first= fnm->get (uc);
    ex->x1= ROUND (first->x1);
    ex->y1= ROUND (first->y1);
    ex->x2= ROUND (first->x2);
    ex->y2= ROUND (first->y2);
    ex->x3= FLOOR (first->x3);
    ex->y3= FLOOR (first->y3);
    ex->x4= CEIL  (first->x4);
    ex->y4= CEIL  (first->y4);
    SI x= ROUND (first->x2);

    while (i<n) {
      unsigned int pc= uc;
      uc= read_unicode_char (s, i);
      if (ligs > 0 && (((char) uc) == 'f' || ((char) uc) == 's'))
        uc= ligature_replace (uc, s, i);
      x += ROUND (fnm->kerning (pc, uc));
      metric_struct* next= fnm->get (uc);
      ex->x1= min (ex->x1, x+ ROUND (next->x1));
      ex->y1= min (ex->y1, ROUND (next->y1));
      ex->x2= max (ex->x2, x+ ROUND (next->x2));
      ex->y2= max (ex->y2, ROUND (next->y2));
      ex->x3= min (ex->x3, x+ FLOOR (next->x3));
      ex->y3= min (ex->y3, FLOOR (next->y3));
      ex->x4= max (ex->x4, x+ CEIL (next->x4));
      ex->y4= max (ex->y4, CEIL (next->y4));
      x += ROUND (next->x2);
      //if (fnm->kerning (pc, uc) != 0)
      //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (next->x2) << "\n";
    }
  }
}

void
unicode_font_rep::get_xpositions (string s, SI* xpos, bool ligf) {
  int i= 0, n= N(s);
  if (n == 0) return;
  
  register SI x= 0;
  unsigned int uc= 0xffffffff;
  while (i<n) {
    int start= i;
    unsigned int pc= uc;
    uc= read_unicode_char (s, i);
    if (ligs > 0 && ligf && (((char) uc) == 'f' || ((char) uc) == 's'))
      uc= ligature_replace (uc, s, i);
    if (pc != 0xffffffff) x += ROUND (fnm->kerning (pc, uc));
    metric_struct* next= fnm->get (uc);
    for (int j= start; j<i; j++) xpos[j]= x;
    x += ROUND (next->x2);
    //if (fnm->kerning (pc, uc) != 0)
    //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (next->x2) << "\n";
  }
  xpos[n]= x;
}

void
unicode_font_rep::get_xpositions (string s, SI* xpos) {
  get_xpositions (s, xpos, true);
}

void
unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool ligf) {
  int i= 0, n= N(s);
  unsigned int uc= 0xffffffff;
  while (i<n) {
    unsigned int pc= uc;
    uc= read_unicode_char (s, i);
    if (ligs > 0 && ligf && (((char) uc) == 'f' || ((char) uc) == 's'))
      uc= ligature_replace (uc, s, i);
    if (pc != 0xffffffff) x += ROUND (fnm->kerning (pc, uc));
    ren->draw (uc, fng, x, y);
    metric_struct* ex= fnm->get (uc);
    x += ROUND (ex->x2);
    //if (fnm->kerning (pc, uc) != 0)
    //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (ex->x2) << "\n";
  }
}

void
unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  draw_fixed (ren, s, x, y, true);
}


font
unicode_font_rep::magnify (double zoomx, double zoomy) {
  return unicode_font (family, size,
                       (int) tm_round (hdpi * zoomx),
                       (int) tm_round (vdpi * zoomy));
}

void
unicode_font_rep::advance_glyph (string s, int& pos) {
  if (pos >= N(s)) return;
  unsigned int uc= read_unicode_char (s, pos);
  if (ligs > 0 && (((char) uc) == 'f' || ((char) uc) == 's'))
    uc= ligature_replace (uc, s, pos);
}

glyph
unicode_font_rep::get_glyph (string s) {
  int i= 0, n= N(s);
  unsigned int uc= read_unicode_char (s, i);
  if (ligs > 0 && (((char) uc) == 'f' || ((char) uc) == 's'))
    uc= ligature_replace (uc, s, i);
  if (i != n) return font_rep::get_glyph (s);
  glyph gl= fng->get (uc);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

int
unicode_font_rep::index_glyph (string s, font_metric& rm, font_glyphs& rg) {
  int i= 0, n= N(s);
  unsigned int uc= read_unicode_char (s, i);
  if (ligs > 0 && (((char) uc) == 'f' || ((char) uc) == 's'))
    uc= ligature_replace (uc, s, i);
  if (i != n) return font_rep::index_glyph (s, rm, rg);
  glyph gl= fng->get (uc);
  if (is_nil (gl)) return font_rep::index_glyph (s, rm, rg);
  rm= fnm;
  rg= fng;
  return uc;
}

double
unicode_font_rep::get_left_slope (string s) {
  if (N(s) == 0) return slope;
  int pos= 0;
  tm_char_forwards (s, pos);
  if (pos == 1) return slope;
  metric ex;
  get_extents (s (0, pos), ex);
  if (ex->y3 >= 0) return slope;
  double sl= ((double) (ex->x3 - ex->x1)) / ((double) ex->y3);
  if (sl > slope + 0.05) return sl;
  else return slope;
}

double
unicode_font_rep::get_right_slope (string s) {
  if (N(s) == 0) return slope;
  int pos= N(s);
  tm_char_backwards (s, pos);
  if (pos == N(s) - 1) return slope;
  metric ex;
  get_extents (s (pos, N(s)), ex);
  if (ex->y4 <= 0) return slope;
  double sl= ((double) (ex->x4 - ex->x2)) / ((double) ex->y4);
  if (sl > slope + 0.05) return sl;
  else return slope;
}

SI
unicode_font_rep::get_left_correction  (string s) {
  metric ex;
  get_extents (s, ex);
  if (ex->x3 < ex->x1) return ex->x1 - ex->x3;
  return 0;
}

SI
unicode_font_rep::get_right_correction (string s) {
  metric ex;
  get_extents (s, ex);
  if (ex->x4 > ex->x2) return ex->x4 - ex->x2;
  return 0;
}

/******************************************************************************
* Interface
******************************************************************************/

font
unicode_font (string family, int size, int hdpi, int vdpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  return make (font, name,
               tm_new<unicode_font_rep> (name, family, size, hdpi, vdpi));
}

font
unicode_font (string family, int size, int dpi) {
  return unicode_font (family, size, dpi, dpi);
}

#else

font
unicode_font (string family, int size, int dpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string (dpi);
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
