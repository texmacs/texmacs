
/******************************************************************************
* MODULE     : rubber_stix_font.cpp
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

struct rubber_stix_font_rep: font_rep {
  font base;
  font baseL;
  font baseD;
  font intsD;
  font up_ints;
  font up_intsD;
  font size1;
  font size2;
  font size3;
  font size4;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  rubber_stix_font_rep (string name, font base);
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

rubber_stix_font_rep::rubber_stix_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  int dpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  baseL= base->magnify (sqrt (2.0));
  baseD= base->magnify (2.0);
  if (!occurs ("-bold-", base->res_name)) {
    intsD= unicode_font ("STIXIntegralsD-Regular", base->size, dpi);
    up_ints= unicode_font ("STIXIntegralsUp-Regular", base->size, dpi);
    up_intsD= unicode_font ("STIXIntegralsUpD-Regular", base->size, dpi);
    size1= unicode_font ("STIXSizeOneSym-Regular", base->size, dpi);
    size2= unicode_font ("STIXSizeTwoSym-Regular", base->size, dpi);
    size3= unicode_font ("STIXSizeThreeSym-Regular", base->size, dpi);
    size4= unicode_font ("STIXSizeFourSym-Regular", base->size, dpi);
  }
  else {
    intsD= unicode_font ("STIXIntegralsD-Bold", base->size, dpi);
    up_ints= unicode_font ("STIXIntegralsUp-Bold", base->size, dpi);
    up_intsD= unicode_font ("STIXIntegralsUpD-Bold", base->size, dpi);
    size1= unicode_font ("STIXSizeOneSym-Bold", base->size, dpi);
    size2= unicode_font ("STIXSizeTwoSym-Bold", base->size, dpi);
    size3= unicode_font ("STIXSizeThreeSym-Bold", base->size, dpi);
    size4= unicode_font ("STIXSizeFourSym-Bold", base->size, dpi);
  }
}

/******************************************************************************
* Find the font
******************************************************************************/

int
rubber_stix_font_rep::search_font_sub (string s, string& rew) {
  if (starts (s, "<big-") && ends (s, "-1>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up") && ends (r, "int")) {
      rew= "<big-" * r (2, N(r)) * ">";
      return 4;
    }
    if (ends (r, "int") || r == "sum" || r == "prod" || r == "pluscup") {
      rew= s;
      return 0;
    }
    rew= s;
    return 1;
  }
  if (starts (s, "<big-") && ends (s, "-2>")) {
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up") && ends (r, "int")) {
      rew= "<big-" * r (2, N(r)) * ">";
      return 5;
    }
    if (ends (r, "int")) {
      rew= "<big-" * r * ">";
      return 3;
    }
    if (r == "sum" || r == "prod" || r == "amalg" ||
        r == "cap" || r == "cup" || r == "vee" || r == "wedge") {
      rew= "<big-" * r * ">";
      return 6;
    }
    if (r == "pluscup") {
      rew= "<big-" * r * ">";
      return 1;
    }
    rew= s;
    return 2;
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-|")) {
    // TODO: how to do this in a better way?
    rew= s;
    return 0;
  }
  if (starts (s, "<left-") && ends (s, "-1>")) {
    string r= s (6, N(s) - 3);
    if (N(r) > 1) r= "<" * r * ">";
    rew= r;
    return 7;
  }
  if (starts (s, "<left-") && ends (s, "-2>")) {
    string r= s (6, N(s) - 3);
    if (N(r) > 1) r= "<" * r * ">";
    rew= r;
    return 8;
  }
  if (starts (s, "<left-") && ends (s, "-3>")) {
    string r= s (6, N(s) - 3);
    if (N(r) > 1) r= "<" * r * ">";
    rew= r;
    return 9;
  }
  if (starts (s, "<left-") && ends (s, "-4>")) {
    string r= s (6, N(s) - 3);
    if (N(r) > 1) r= "<" * r * ">";
    rew= r;
    return 10;
  }
  rew= s;
  return 0;
}

int
rubber_stix_font_rep::search_font_cached (string s, string& rew) {
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
rubber_stix_font_rep::search_font (string& s, SI& dy) {
  string rew;
  int nr= search_font_cached (s, rew);
  s= rew;
  switch (nr) {
  case 0:
    dy= 0;
    return base;
  case 1:
    dy= 0;
    return baseL;
  case 2:
    dy= 0;
    return baseD;
  case 3:
    dy= (2 * base->yx) / 3;
    return intsD;
  case 4:
    dy= (2 * base->yx) / 3;
    return up_ints;
  case 5:
    dy= (2 * base->yx) / 3;
    return up_intsD;
  case 6:
    dy= (2 * base->yx) / 3;
    return size1;
  case 7:
    dy= 0;
    return size1;
  case 8:
    dy= 0;
    return size2;
  case 9:
    dy= 0;
    return size3;
  case 10:
    dy= 0;
    return size4;
  default:
    dy= 0;
    return base;
  }
}

font
rubber_stix_font_rep::search_font (string& s) {
  SI dy;
  return search_font (s, dy);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_stix_font_rep::supports (string c) {
  font fn= search_font (c);
  return fn->supports (c);
}

void
rubber_stix_font_rep::get_extents (string s, metric& ex) {
  SI dy;
  font fn= search_font (s, dy);
  fn->get_extents (s, ex);
  ex->y1 += dy; ex->y2 += dy;
  ex->y3 += dy; ex->y4 += dy;
}

void
rubber_stix_font_rep::get_xpositions (string s, SI* xpos) {
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
rubber_stix_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
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
rubber_stix_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  SI dy;
  font fn= search_font (s, dy);
  fn->draw_fixed (ren, s, x, y + dy);
}

void
rubber_stix_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  SI dy;
  font fn= search_font (s, dy);
  fn->draw_fixed (ren, s, x, y + dy, xk);
}

font
rubber_stix_font_rep::magnify (double zoom) {
  return rubber_stix_font (base->magnify (zoom));
}

glyph
rubber_stix_font_rep::get_glyph (string s) {
  SI dy;
  font fn= search_font (s, dy);
  return move (fn->get_glyph (s), 0, dy);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
rubber_stix_font_rep::get_left_slope  (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
rubber_stix_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
rubber_stix_font_rep::get_left_correction  (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
rubber_stix_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_stix_font (font base) {
  string name= "rubberstix[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_stix_font_rep> (name, base));
}

#else

font
rubber_stix_font (font base) {
  string name= "rubberstix[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
