
/******************************************************************************
* MODULE     : unicode_math_font.cpp
* DESCRIPTION: True Type math fonts (using FreeType II)
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
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
  int    search_font_sub (string s);
  font   search_font (string& s);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
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
  return strict_cork_to_utf8 (s) != s;
}

static unsigned int
cork_to_unicode (string s) {
  int i= 0;
  return decode_from_utf8 (strict_cork_to_utf8 (s), i);
}

//static string
//unicode_to_hexcode (unsigned int i) {
//  return "<#" * as_hexadecimal (i) * ">";
//}

int
unicode_math_font_rep::search_font_sub (string s) {
  //cout << "Searching " << s << "\n";
  if (N(s) == 0) return 1;
  else if (s == "*" || starts (s, "<big-.") ||
	   s == "<noplus>" || s == "<nocomma>" || s == "<nospace>" ||
	   s == "<nobracket>" || s == "<nosymbol>") {
    rewriter(s)= "";
    return 2;
  }
  else if (s == "-") { rewriter (s)= "<minus>"; return 2; }
  else if (s == "|") { rewriter (s)= "<mid>"; return 2; }
  else if (s == "'") { rewriter (s)= "<#2B9>"; return 2; }
  else if (s == "`") { rewriter (s)= "<backprime>"; return 2; }
  else if (N(s) == 1) {
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
    if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
      string ss= s (0, N(s)-3) * ">";
      //cout << "Search " << ss << "\n";
      if (unicode_provides (ss)) {
	unsigned int c= search_font_sub (ss);
	rewriter (s)= ss;
	if (c == 1) return 2;
	return c;
      }
      ss= "<big" * s (5, N(s)-3) * ">";
      //cout << "Search " << ss << "\n";
      if (unicode_provides (ss)) {
	unsigned int c= search_font_sub (ss);
	rewriter (s)= ss;
	if (c == 1) return 2;
	return c;
      }
      ss= "<" * s (5, N(s)-3) * ">";
      if (ends (ss, "lim>")) ss= ss (0, N(ss)-4) * ">";
      //cout << "Search " << ss << "\n";
      if (unicode_provides (ss)) {
	unsigned int c= search_font_sub (ss);
	rewriter (s)= ss;
	if (c == 1) return 2;
	return c;
      }
    }
    if (!unicode_provides (s)) return 6;
    unsigned int c= cork_to_unicode (s);
    if (c >= 0x3ac && c <= 0x3d6 && !starts (s, "<math")) return 3;
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

bool
unicode_math_font_rep::supports (string c) {
  font fn= search_font (c);
  return fn->supports (c);
}

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
unicode_math_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (s == "") return;
  string r= s;
  font fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos, xk);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos, xk);
  else {
    int i, n=N(s);
    for (i=0; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1, xk);
  }
}

void
unicode_math_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y);
}

void
unicode_math_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y, xk);
}

font
unicode_math_font_rep::magnify (double zoomx, double zoomy) {
  return unicode_math_font (upright->magnify (zoomx, zoomy),
			    italic->magnify (zoomx, zoomy),
			    bold_upright->magnify (zoomx, zoomy),
			    bold_italic->magnify (zoomx, zoomy),
			    fall_back->magnify (zoomx, zoomy));
}

glyph
unicode_math_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

int
unicode_math_font_rep::index_glyph (string s, font_metric& fnm,
                                              font_glyphs& fng) {
  font fn= search_font (s);
  return fn->index_glyph (s, fnm, fng);
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

SI
unicode_math_font_rep::get_lsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
unicode_math_font_rep::get_lsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
unicode_math_font_rep::get_rsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
unicode_math_font_rep::get_rsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
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
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
