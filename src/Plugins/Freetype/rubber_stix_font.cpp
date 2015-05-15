
/******************************************************************************
* MODULE     : rubber_stix_font.cpp
* DESCRIPTION: Rubber Stix fonts
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
  font wide1;
  font wide2;
  font wide3;
  font wide4;
  font wide5;
  font assemble1;
  font assemble2;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;
  hashmap<string,string> delimiter;

  rubber_stix_font_rep (string name, font base);
  int search_font_sub (string s, string& rew, string& ltype);
  int search_font_cached (string s, string& rew, string& ltype);
  font search_font (string& s, SI& dy, string& ltype);
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
    wide1= size1;
    wide2= size2;
    wide3= size3;
    wide4= size4;
    wide5= unicode_font ("STIXSizeFiveSym-Regular", base->size, dpi);
    assemble1= rubber_assemble_font (base);
    assemble2= rubber_assemble_font (size1);
  }
  else {
    intsD= unicode_font ("STIXIntegralsD-Bold", base->size, dpi);
    up_ints= unicode_font ("STIXIntegralsUp-Bold", base->size, dpi);
    up_intsD= unicode_font ("STIXIntegralsUpD-Bold", base->size, dpi);
    size1= unicode_font ("STIXSizeOneSym-Bold", base->size, dpi);
    size2= unicode_font ("STIXSizeTwoSym-Bold", base->size, dpi);
    size3= unicode_font ("STIXSizeThreeSym-Bold", base->size, dpi);
    size4= unicode_font ("STIXSizeFourSym-Bold", base->size, dpi);
    wide1= unicode_font ("STIXSizeOneSym-Regular", base->size, dpi);
    wide2= unicode_font ("STIXSizeTwoSym-Regular", base->size, dpi);
    wide3= unicode_font ("STIXSizeThreeSym-Regular", base->size, dpi);
    wide4= unicode_font ("STIXSizeFourSym-Regular", base->size, dpi);
    wide5= unicode_font ("STIXSizeFiveSym-Regular", base->size, dpi);
    assemble1= rubber_assemble_font (base);
    assemble2= rubber_assemble_font (wide1);
  }
}

/******************************************************************************
* Find the font
******************************************************************************/

static string
large_type (string s) {
  int pos= search_backwards ("-", N(s), s);
  if (pos > 6) return s (1, pos);
  else if (!starts (s, "<") || !ends (s, ">")) return s;
  else return s (1, N(s) - 1);
}

static int
large_size (string s) {
  int pos= search_backwards ("-", N(s), s);
  if (pos > 6) {
    string r= s (pos + 1, N(s) - 1);
    return as_int (r);
  }
  return 0;
}

int
rubber_stix_font_rep::search_font_sub (string s, string& rew, string& ltype) {
  ltype= "";
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
    if (r == "box") s= "<big-square-1>";
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
    if (r == "box") s= "<big-square-2>";
    rew= s;
    return 2;
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) ltype= s (6, pos);
  }
  if (starts (s, "<left-|") || starts (s, "<left-interleave-")) {
    int nr= large_size (s);
    if (nr == 0) rew= s;
    else rew= "<" * large_type (s) * "-" * as_string (nr + 4) * ">";
    return 16;
  }
  if (starts (s, "<left-") && ends (s, "-0>")) {
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
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      if (r == "(" || r == ")" ||
          r == "[" || r == "]" ||
          r == "{" || r == "}" ||
          r == "lfloor" || r == "rfloor" ||
          r == "lceil" || r == "rceil") {
        rew= s;
        return 17;
      }
      if (r == "/" || r == "\\" ||
          r == "langle" || r == "rangle") {
        if (N(r) == 1) rew= r;
        else rew= "<" * r * ">";
        return 10;
      }
    }
  }
  if (starts (s, "<rubber-") && ends (s, ">")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 8) {
      string r= s (8, pos);
      int nr= as_int (s (pos+1, N(s)-1));
      if (nr < 0) nr= 0;
      if (nr > 5) nr= 5;
      if (r == "hat") rew= "<#2C6>";
      else if (r == "tilde") rew= "<#2DC>";
      else if (r == "check") rew= "<#2C7>";
      else if (r == "bar") rew= "<#203E>";
      else if (r == "vect") rew= "<#20D7>";
      else if (r == "breve") rew= "<#2D8>";
      else if (r == "invbreve") rew= "<#311>";
      else if (r == "punderbrace") rew= "<#23DD>";
      else if (r == "punderbrace*") rew= "<#23DD>";
      else if (r == "underbrace") rew= "<#23DF>";
      else if (r == "underbrace*") rew= "<#23DF>";
      else if (r == "squnderbrace") rew= "<#23B5>";
      else if (r == "squnderbrace*") rew= "<#23B5>";
      else if (r == "poverbrace") rew= "<#23DC>";
      else if (r == "poverbrace*") rew= "<#23DC>";
      else if (r == "overbrace") rew= "<#23DE>";
      else if (r == "overbrace*") rew= "<#23DE>";
      else if (r == "sqoverbrace") rew= "<#23B4>";
      else if (r == "sqoverbrace*") rew= "<#23B4>";
      else {
        rew= s;
        return 0;
      }
      if (nr == 0) return 0;
      else return 10 + nr;
    }
  }
  rew= s;
  return 0;
}

int
rubber_stix_font_rep::search_font_cached (string s, string& rew, string& ltype) {
  if (mapper->contains (s)) {
    rew= rewriter[s];
    ltype= delimiter[s];
    return mapper[s];
  }
  int nr= search_font_sub (s, rew, ltype);
  mapper(s)= nr;
  rewriter(s)= rew;
  delimiter(s)= ltype;
  //cout << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_stix_font_rep::search_font (string& s, SI& dy, string& ltype) {
  string rew;
  int nr= search_font_cached (s, rew, ltype);
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
  case 11:
    dy= 0;
    return wide1;
  case 12:
    dy= 0;
    return wide2;
  case 13:
    dy= 0;
    return wide3;
  case 14:
    dy= 0;
    return wide4;
  case 15:
    dy= 0;
    return wide5;
  case 16:
    dy= 0;
    return assemble1;
  case 17:
    dy= 0;
    return assemble2;
  default:
    dy= 0;
    return base;
  }
}

font
rubber_stix_font_rep::search_font (string& s) {
  SI dy;
  string ltype;
  return search_font (s, dy, ltype);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_stix_font_rep::supports (string c) {
  font fn= search_font (c);
  return fn->supports (c);
}

inline void
adjust_hspace (metric& ex, SI plus) {
  ex->x1 -= plus;
  ex->x2 += plus;
}

void
rubber_stix_font_rep::get_extents (string s, metric& ex) {
  SI dy;
  string ltype;
  string orig= s;
  font fn= search_font (s, dy, ltype);
  fn->get_extents (s, ex);
  ex->y1 += dy; ex->y2 += dy;
  ex->y3 += dy; ex->y4 += dy;
  if (ltype != "") {
    int nr= large_size (orig);
    if (ltype[0] == '|' || ltype == "interleave") {
      if (ex->y2 - ex->y1 >= 9 * base->yx)
        adjust_hspace (ex, base->wfn / 10);
      else if (ex->y2 - ex->y1 >= 6 * base->yx)
        adjust_hspace (ex, base->wfn / 15);
      else if (ex->y2 - ex->y1 >= 3 * base->yx)
        adjust_hspace (ex, base->wfn / 20);
    }
    else if (ltype[0] == '(') {
      if (nr == 2)
        ex->x1 += base->wfn / 24;
      else if (nr == 3)
        ex->x1 += base->wfn / 10;
      else if (nr == 4)
        ex->x1 += base->wfn / 18;
      else if (nr > 4) {
        ex->x1 -= base->wfn / 8;
        ex->x2 += base->wfn / 16;
      }
    }
    else if (ltype[0] == ')') {
      if (nr == 2)
        ex->x2 -= base->wfn / 24;
      else if (nr == 3)
        ex->x2 -= base->wfn / 10;
      else if (nr == 4)
        ex->x2 -= base->wfn / 18;
      else if (nr > 4) {
        ex->x2 += base->wfn / 8;
        ex->x1 -= base->wfn / 16;
      }
    }
    else if (ltype[0] == '{') {
      if (nr == 2)
        adjust_hspace (ex, -base->wfn / 15);
      else if (nr == 3)
        adjust_hspace (ex, -base->wfn / 10);
      else if (nr == 4) {
        ex->x2 -= base->wfn / 5;
        ex->x1 += base->wfn / 10;
      }
    }
    else if (ltype[0] == '}') {
      if (nr == 2)
        adjust_hspace (ex, -base->wfn / 15);
      else if (nr == 3)
        adjust_hspace (ex, -base->wfn / 10);
      else if (nr == 4) {
        ex->x1 += base->wfn / 5;
        ex->x2 -= base->wfn / 10;
      }
    }
    else if (ltype[0] == '[' || ltype == "llbracket" ||
             ltype == "lfloor" || ltype == "lceil") {
      if (nr == 2)
        ex->x1 += base->wfn / 30;
      else if (nr == 3)
        ex->x1 += base->wfn / 20;
      else if (nr == 4)
        ex->x1 += base->wfn / 10;
      else if (nr > 4 && ltype != "llbracket") {
        ex->x1 -= base->wfn / 8;
        ex->x2 += base->wfn / 32;
      }
    }
    else if (ltype[0] == ']' || ltype == "rrbracket" ||
             ltype == "rfloor" || ltype == "rceil") {
      if (nr == 2)
        ex->x2 -= base->wfn / 30;
      else if (nr == 3)
        ex->x2 -= base->wfn / 20;
      else if (nr == 4)
        ex->x2 -= base->wfn / 10;
      else if (nr > 4 && ltype != "rrbracket") {
        ex->x2 += base->wfn / 8;
        ex->x1 -= base->wfn / 32;
      }
    }
    else if (ltype == "langle" || ltype == "rangle") {
      if (nr == 2)
        adjust_hspace (ex, -base->wfn / 36);
      else if (nr == 3)
        adjust_hspace (ex, -base->wfn / 24);
      else if (nr >= 4)
        adjust_hspace (ex, -base->wfn / 12);
    }
  }
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
  string ltype;
  string orig= s;
  font fn= search_font (s, dy, ltype);
  fn->draw_fixed (ren, s, x, y + dy);
}

void
rubber_stix_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  SI dy;
  string ltype;
  font fn= search_font (s, dy, ltype);
  fn->draw_fixed (ren, s, x, y + dy, xk);
}

font
rubber_stix_font_rep::magnify (double zoom) {
  return rubber_stix_font (base->magnify (zoom));
}

glyph
rubber_stix_font_rep::get_glyph (string s) {
  SI dy;
  string ltype;
  font fn= search_font (s, dy, ltype);
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
