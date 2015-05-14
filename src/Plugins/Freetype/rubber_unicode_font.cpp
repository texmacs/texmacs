
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Rubber unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
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

struct rubber_unicode_font_rep: font_rep {
  font base;
  font baseS;
  font baseL;
  font baseD;
  font assemble;
  bool big_sums;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  rubber_unicode_font_rep (string name, font base);
  int search_font_sub (string s, string& rew);
  int search_font_cached (string s, string& rew);
  font search_font (string& s, SI& dy);
  font search_font (string& s);

  bool supports (string c);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void get_xpositions (string s, SI* xpos, SI xk);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  void draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font magnify (double zoom);
  glyph get_glyph (string s);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

rubber_unicode_font_rep::rubber_unicode_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  big_sums= false;
  if (base->supports ("<sum>")) {
    metric ex;
    base->get_extents ("<sum>", ex);
    //cout << base->res_name << " -> "
    //<< ((double) (ex->y2-ex->y1)) / base->yx << LF;
    if ((((double) (ex->y2-ex->y1)) / base->yx) >= 1.55) big_sums= true;
  }
  baseS= base->magnify (sqrt (0.5));
  baseL= base->magnify (sqrt (2.0));
  baseD= base->magnify (2.0);
  assemble= rubber_assemble_font (base);
}

/******************************************************************************
* Find the font
******************************************************************************/

int
rubber_unicode_font_rep::search_font_sub (string s, string& rew) {
  if (starts (s, "<big-") && ends (s, "-1>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 0;
      return 2;
    }
  }
  if (starts (s, "<big-") && ends (s, "-2>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 2;
      return 3;
    }
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      if ((r == "(" && base->supports ("<#239C>")) ||
          (r == ")" && base->supports ("<#239F>")) ||
          (r == "[" && base->supports ("<#23A2>")) ||
          (r == "]" && base->supports ("<#23A5>")) ||
          ((r == "{" || r == "}") && base->supports ("<#23AA>"))) {
        rew= s;
        return 4;
      }
    }
  }
  rew= s;
  return 0;
}

int
rubber_unicode_font_rep::search_font_cached (string s, string& rew) {
  if (mapper->contains (s)) {
    rew= rewriter[s];
    return mapper[s];
  }
  int nr= search_font_sub (s, rew);
  mapper(s)= nr;
  rewriter(s)= rew;
  //cout << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_unicode_font_rep::search_font (string& s, SI& dy) {
  string rew;
  int nr= search_font_cached (s, rew);
  s= rew;
  switch (nr) {
  case 0:
    dy= 0;
    return base;
  case 1:
    dy= 0;
    return baseS;
  case 2:
    dy= 0;
    return baseL;
  case 3:
    dy= 0;
    return baseD;
  case 4:
    dy= 0;
    return assemble;
  default:
    dy= 0;
    return base;
  }
}

font
rubber_unicode_font_rep::search_font (string& s) {
  SI dy;
  return search_font (s, dy);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_unicode_font_rep::supports (string s) {
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (N(r) > 1) r= "<" * r * ">";
    return base->supports (r);
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      if (N(r) > 1) r= "<" * r * ">";
      return base->supports (r);
    }
  }
  return base->supports (s);
}

void
rubber_unicode_font_rep::get_extents (string s, metric& ex) {
  SI dy;
  font fn= search_font (s, dy);
  fn->get_extents (s, ex);
  ex->y1 += dy; ex->y2 += dy;
  ex->y3 += dy; ex->y4 += dy;
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos) {
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
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
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
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  SI dy;
  font fn= search_font (s, dy);
  fn->draw_fixed (ren, s, x, y + dy);
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  SI dy;
  font fn= search_font (s, dy);
  fn->draw_fixed (ren, s, x, y + dy, xk);
}

font
rubber_unicode_font_rep::magnify (double zoom) {
  return rubber_unicode_font (base->magnify (zoom));
}

glyph
rubber_unicode_font_rep::get_glyph (string s) {
  SI dy;
  font fn= search_font (s, dy);
  return move (fn->get_glyph (s), 0, dy);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
rubber_unicode_font_rep::get_left_slope  (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
rubber_unicode_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
rubber_unicode_font_rep::get_left_correction  (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
rubber_unicode_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base));
}

#else

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
