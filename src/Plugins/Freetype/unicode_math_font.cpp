
/******************************************************************************
* MODULE     : unicode_math_font.cpp
* DESCRIPTION: True Type math fonts (using FreeType II)
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "converter.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct unicode_math_font_rep: font_rep {
  font upright;
  font italic;
  font bold_upright;
  font bold_italic;
  font fall_back;
  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  unicode_math_font_rep (string name,
			 font upright, font italic,
			 font bold_upright, font bold_italic,
			 font fall_back);
  int search_font_sub (string s);
  font search_font (string& s);

  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
  glyph get_glyph (string s);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

unicode_math_font_rep::unicode_math_font_rep
  (string name, font up, font it, font bup, font bit, font fb):
    font_rep (name, it),
    upright (up), italic (it),
    bold_upright (bup), bold_italic (bit), fall_back (fb),
    mapper (0), rewriter ("")
{
  this->copy_math_pars (it);
}

/******************************************************************************
* Find the font and the corresponding character
******************************************************************************/

static bool
unicode_provides (string s) {
  return cork_to_utf8 (s) != s;
}

static unsigned int
cork_to_unicode (string s) {
  int i= 0;
  return decode_from_utf8 (cork_to_utf8 (s), i);
}

//static string
//unicode_to_hexcode (unsigned int i) {
//  return "<#" * as_hexadecimal (i) * ">";
//}

int
unicode_math_font_rep::search_font_sub (string s) {
  //cout << "Searching " << s << "\n";
  if (N(s) == 0) return 1;
  else if (N(s) == 1) {
    if (s[0] == '*') {
      rewriter(s)= "";
      return 2;
    }
    if (s[0] >= 'a' && s[0] <= 'z') return 3;
    if (s[0] >= 'A' && s[0] <= 'Z') return 3;
    return 1;
  }
  else if (s[0] == '<' && s[N(s)-1] == '>') {
    if (starts (s, "<cal-")) return 6; // Temporary fix
    if (starts (s, "<b-")) {
      string ss= s (3, N(s)-1);
      if (N(ss) != 1) ss= "<" * ss * ">";
      unsigned int c= search_font_sub (ss);
      rewriter (s)= ss;
      if (c == 1 || c == 2) return 4;
      if (c == 3) return 5;
      rewriter (s)= s;
      return 6;
    }
    if (starts (s, "<big-") && ends (s, "-2>")) {
      string ss= s (0, N(s)-3) * "-1>";
      if (unicode_provides (ss)) {
	unsigned int c= search_font_sub (ss);
	rewriter (s)= ss;
	if (c == 1) return 2;
	return c;
      }
    }
    if (!unicode_provides (s)) return 6;
    unsigned int c= cork_to_unicode (s);
    if (c >= 0x3ac && c <= 0x3d6) return 3;
    return 1;
  }
  else {
    return 1;
  }
}

font
unicode_math_font_rep::search_font (string& s) {
  if (N(s) >= 2 && s[0] != '<') return upright;
  else switch (mapper[s]) {
    case 0:
      mapper(s)= search_font_sub (s);
      return search_font (s);
    case 1:
      return upright;
    case 2:
      s= rewriter[s];
      return upright;
    case 3:
      return italic;
    case 4:
      s= rewriter[s];
      return bold_upright;
    case 5:
      s= rewriter[s];
      return bold_italic;
    case 6:
      return fall_back;
    default:
      return upright;
    }
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

void
unicode_math_font_rep::get_extents (string s, metric& ex) {
  font fn= search_font (s);
  fn->get_extents (s, ex);
}

void
unicode_math_font_rep::get_xpositions (string s, SI* xpos) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos);
  else {
    int i, n=N(s);
    for (i=1; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1);
  }
}

void
unicode_math_font_rep::draw (renderer ren, string s, SI x, SI y) {
  font fn= search_font (s);
  fn->draw (ren, s, x, y);
}

glyph
unicode_math_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
unicode_math_font_rep::get_left_slope  (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
unicode_math_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
unicode_math_font_rep::get_left_correction  (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
unicode_math_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

/******************************************************************************
* Interface
******************************************************************************/

font
unicode_math_font (font up, font it, font bup, font bit, font fb) {
  string name=
    "unimath[" *
    up->res_name * "," *
    it->res_name * "," *
    bup->res_name * "," *
    bit->res_name * "," *
    fb->res_name * "]";
  return make (font, name,
    tm_new<unicode_math_font_rep> (name, up, it, bup, bit, fb));
}

#else

font
unicode_math_font (font up, font it, font bup, font bit, font fb) {
  string name=
    "unimath[" *
    up->res_name * "," *
    it->res_name * "," *
    bup->res_name * "," *
    bit->res_name * "," *
    fb->res_name * "]";
  cerr << "\n\nFont name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
