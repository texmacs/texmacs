
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Assemble rubber characters from pieces in Unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "converter.hpp"
#include "translator.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_assemble_font_rep: font_rep {
  font base;
  array<bool> initialized;
  array<font> larger;
  translator virt;

  rubber_assemble_font_rep (string name, font base);
  font get_font (int nr);
  int search_font (string s, string& r);

  bool  supports (string c);
  void  get_extents (string s, metric& ex);
  void  draw_fixed (renderer ren, string s, SI x, SI y);
  void  draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font  magnify (double zoomx, double zoomy);
  glyph get_glyph (string s);
  int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

#define MAGNIFIED_NUMBER 4

rubber_assemble_font_rep::rubber_assemble_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  initialized << true;
  larger << base;
  for (int i=1; i<=MAGNIFIED_NUMBER; i++) {
    initialized << false;
    larger << base;
  }
  int hdpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  int vdpi= (72 * base->hpt + (PIXEL/2)) / PIXEL;
  initialized << true;
  larger << virtual_font (base, "emu-alt-large", base->size, hdpi, vdpi, false);
  virt= load_translator ("emu-alt-large");
}

font
rubber_assemble_font_rep::get_font (int nr) {
  ASSERT (nr < N(larger), "wrong font number");
  if (initialized[nr]) return larger[nr];
  initialized[nr]= true;
  larger[nr]= base->magnify (pow (2.0, ((double) nr) / 4.0));
  return larger[nr];
}

int
rubber_assemble_font_rep::search_font (string s, string& r) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      r= s (6, pos);
      int num= as_int (s (pos+1, N(s)-1));
      int nr= num - 5;
      int code;
      if (num <= MAGNIFIED_NUMBER) return num;
      else if (r == "(")
        code= virt->dict ["<rubber-lparenthesis-#>"];
      else if (r == ")")
        code= virt->dict ["<rubber-rparenthesis-#>"];
      else if (r == "[")
        code= virt->dict ["<rubber-lbracket-#>"];
      else if (r == "]")
        code= virt->dict ["<rubber-rbracket-#>"];
      else if (r == "{")
        code= virt->dict ["<rubber-lcurly-#>"];
      else if (r == "}")
        code= virt->dict ["<rubber-rcurly-#>"];
      else if (r == "lfloor")
        code= virt->dict ["<rubber-lfloor-#>"];
      else if (r == "rfloor")
        code= virt->dict ["<rubber-rfloor-#>"];
      else if (r == "lceil")
        code= virt->dict ["<rubber-lceil-#>"];
      else if (r == "rceil")
        code= virt->dict ["<rubber-rceil-#>"];
      else if (r == ".")
        code= virt->dict ["<rubber-nosymbol-#>"];
      else if (r == "nosymbol")
        code= virt->dict ["<rubber-nosymbol-#>"];
      else if (r == "|")
        code= virt->dict ["<rubber-bar-#>"];
      else if (r == "||")
        code= virt->dict ["<rubber-parallel-#>"];
      else if (r == "interleave")
        code= virt->dict ["<rubber-interleave-#>"];
      else
        code= virt->dict ["<rubber-lparenthesis-#>"];
      r= string ((char) code) * as_string (nr) * ">";
      return MAGNIFIED_NUMBER + 1;
    }
  }
  r= s;
  return 0;
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_assemble_font_rep::supports (string s) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      return
	r == "(" || r == ")" ||
	r == "[" || r == "]" ||
	r == "{" || r == "}" ||
	r == "lfloor" || r == "rfloor" ||
	r == "lceil" || r == "rceil" ||
	r == "|" || r == "||" ||
	r == "interleave";
    }
  }
  return false;
}

void
rubber_assemble_font_rep::get_extents (string s, metric& ex) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> get_extents (name, ex);
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> draw (ren, name, x, y);
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> draw (ren, name, x, y, xk);
}

font
rubber_assemble_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_assemble_font (base->magnify (zoomx, zoomy));
}

glyph
rubber_assemble_font_rep::get_glyph (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_glyph (name);
}

int
rubber_assemble_font_rep::index_glyph (string s, font_metric& fnm,
                                                 font_glyphs& fng) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> index_glyph (name, fnm, fng);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_assemble_font (font base) {
  string name= "rubberassemble[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_assemble_font_rep> (name, base));
}

#else

font
rubber_assemble_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
