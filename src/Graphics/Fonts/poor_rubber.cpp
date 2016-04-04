
/******************************************************************************
* MODULE     : poor_rubber.cpp
* DESCRIPTION: Assemble rubber characters from pieces of common characters
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "converter.hpp"
#include "translator.hpp"

/******************************************************************************
* True Type fonts
******************************************************************************/

struct poor_rubber_font_rep: font_rep {
  font base;
  array<bool> initialized;
  array<font> larger;
  translator virt;

  poor_rubber_font_rep (string name, font base);
  font get_font (int nr);
  int search_font (string s, string& r);

  bool supports (string c);
  void get_extents (string s, metric& ex);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  void draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font magnify (double zoom);
  glyph get_glyph (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

#define MAGNIFIED_NUMBER 4

poor_rubber_font_rep::poor_rubber_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  initialized << true;
  larger << base;
  for (int i=1; i<=MAGNIFIED_NUMBER; i++) {
    initialized << false;
    larger << base;
  }
  int dpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  initialized << true;
  larger << virtual_font (base, "poorlong", base->size, dpi);
  virt= load_translator ("poorlong");
}

font
poor_rubber_font_rep::get_font (int nr) {
  ASSERT (nr < N(larger), "wrong font number");
  if (initialized[nr]) return larger[nr];
  initialized[nr]= true;
  larger[nr]= base->magnify (pow (2.0, ((double) nr) / 4.0));
  return larger[nr];
}

int
poor_rubber_font_rep::search_font (string s, string& r) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
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
poor_rubber_font_rep::supports (string s) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
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
poor_rubber_font_rep::get_extents (string s, metric& ex) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> get_extents (name, ex);
}

void
poor_rubber_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> draw (ren, name, x, y);
}

void
poor_rubber_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  string name;
  int num= search_font (s, name);
  get_font (num) -> draw (ren, name, x, y, xk);
}

font
poor_rubber_font_rep::magnify (double zoom) {
  return poor_rubber_font (base->magnify (zoom));
}

glyph
poor_rubber_font_rep::get_glyph (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_glyph (name);
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_rubber_font (font base) {
  string name= "poorrubber[" * base->res_name * "]";
  return make (font, name, tm_new<poor_rubber_font_rep> (name, base));
}
