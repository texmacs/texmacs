
/******************************************************************************
* MODULE     : rubber_stix_font.cpp
* DESCRIPTION: Rubber Stix fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
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

struct rubber_stix_font_rep: font_rep {
  font base;
  int  dpi;
  bool reg;

  array<bool> initialized;
  array<font> subfn;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;
  hashmap<string,string> delimiter;

  rubber_stix_font_rep (string name, font base);
  font   get_font_sub (int nr);
  font   get_font (int nr);
  int    search_font_sub (string s, string& rew, string& ltype);
  int    search_font_cached (string s, string& rew, string& ltype);
  font   search_font (string& s, SI& dy, string& ltype);
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
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

rubber_stix_font_rep::rubber_stix_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  dpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  reg= !occurs ("-bold-", base->res_name);
  for (int i=0; i<19; i++) {
    initialized << false;
    subfn << base;
  }
}

font
rubber_stix_font_rep::get_font_sub (int nr) {
  switch (nr) {
  case 0: return base;
  case 1: return base->magnify (sqrt (2.0));
  case 2: return base->magnify (2.0);
  case 3:
    if (reg) return unicode_font ("STIXIntegralsD-Regular", base->size, dpi);
    else return unicode_font ("STIXIntegralsD-Bold", base->size, dpi);
  case 4:
    if (reg) return unicode_font ("STIXIntegralsUp-Regular", base->size, dpi);
    else return unicode_font ("STIXIntegralsD-Bold", base->size, dpi);
  case 5:
    if (reg) return unicode_font ("STIXIntegralsUpD-Regular", base->size, dpi);
    else return unicode_font ("STIXIntegralsUpD-Bold", base->size, dpi);
  case 6:
    if (reg) return unicode_font ("STIXSizeOneSym-Regular", base->size, dpi);
    else return unicode_font ("STIXSizeOneSym-Bold", base->size, dpi);
  case 7:
    if (reg) return unicode_font ("STIXSizeOneSym-Regular", base->size, dpi);
    else return unicode_font ("STIXSizeOneSym-Bold", base->size, dpi);
  case 8:
    if (reg) return unicode_font ("STIXSizeTwoSym-Regular", base->size, dpi);
    else return unicode_font ("STIXSizeTwoSym-Bold", base->size, dpi);
  case 9:
    if (reg) return unicode_font ("STIXSizeThreeSym-Regular", base->size, dpi);
    else return unicode_font ("STIXSizeThreeSym-Bold", base->size, dpi);
  case 10:
    if (reg) return unicode_font ("STIXSizeFourSym-Regular", base->size, dpi);
    else return unicode_font ("STIXSizeFourSym-Bold", base->size, dpi);
  case 11: return unicode_font ("STIXSizeOneSym-Regular", base->size, dpi);
  case 12: return unicode_font ("STIXSizeTwoSym-Regular", base->size, dpi);
  case 13: return unicode_font ("STIXSizeThreeSym-Regular", base->size, dpi);
  case 14: return unicode_font ("STIXSizeFourSym-Regular", base->size, dpi);
  case 15: return unicode_font ("STIXSizeFiveSym-Regular", base->size, dpi);
  case 16: return rubber_assemble_font (base);
  case 17: return rubber_assemble_font (get_font (11));
  case 18: return unicode_font ("STIXMath-Regular", base->size, dpi);
  default: return base;
  }
}

font
rubber_stix_font_rep::get_font (int nr) {
  ASSERT (nr < N(subfn), "wrong font number");
  if (initialized[nr]) return subfn[nr];
  subfn[nr]= get_font_sub (nr);
  initialized[nr]= true;
  return subfn[nr];
}

/******************************************************************************
* Find the font
******************************************************************************/

static string
large_type (string s) {
  int pos= search_backwards ("-", N(s), s);
  if (pos > 6 && s[pos-1] == '-') pos--;
  if (pos > 6) return s (1, pos);
  else if (!starts (s, "<") || !ends (s, ">")) return s;
  else return s (1, N(s) - 1);
}

static int
large_size (string s) {
  int pos= search_backwards ("-", N(s), s);
  if (pos > 6) {
    if (s[pos-1] == '-') pos--;
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
  if (starts (s, "<left-.")) {
    rew= "";
    return 0;
  }
  if (starts (s, "<left-|") || starts (s, "<left-interleave-")) {
    int nr= large_size (s);
    if (nr <= 0) {
      string r= large_type (s);
      r= r (5, N(r));
      if (N(r) > 1) r= "<" * r * ">";
      rew= r;
      return 0;
    }
    else {
      rew= "<" * large_type (s) * "-" * as_string (nr + 9) * ">";
      return 16;
    }
  }
  if (starts (s, "<left-") && ends (s, "-0>")) {
    rew= s;
    return 0;
  }
  if (starts (s, "<left-sqrt-")) {
    string r= s (6, N(s) - 3);
    if (N(r) > 1) r= "<" * r * ">";
    rew= r;
    if (s == "<left-sqrt-1>") return 18;
    if (s == "<left-sqrt-2>") return 7;
    if (s == "<left-sqrt-3>") return 8;
    if (s == "<left-sqrt-4>") return 9;
    if (s == "<left-sqrt-5>") return 10;
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
      int nr= as_int (s (pos+1, N(s)-1));
      if (r == "(" || r == ")" ||
          r == "[" || r == "]") {
        rew= "<left-" * r * "-" * as_string (nr + 5) * ">";
        return 17;
      }
      if (r == "{" || r == "}") {
        rew= "<left-" * r * "-" * as_string (nr + 2) * ">";
        return 17;
      }
      if (r == "lfloor" || r == "rfloor" ||
          r == "lceil" || r == "rceil") {
        rew= "<left-" * r * "-" * as_string (nr + 9) * ">";
        return 17;
      }
      if (r == "sqrt") {
        rew= "<large-" * r * "-" * as_string (nr + 9) * ">";
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
  //cout << s << " -> " << nr << ", " << rew << ", " << ltype << LF;
  return nr;
}

font
rubber_stix_font_rep::search_font (string& s, SI& dy, string& ltype) {
  string rew;
  int nr= search_font_cached (s, rew, ltype);
  s= rew;
  if (nr < 3 || nr >= 7) dy= 0;
  else dy= (2 * base->yx) / 3;
  return get_font (nr);
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
rubber_stix_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_stix_font (base->magnify (zoomx, zoomy));
}

glyph
rubber_stix_font_rep::get_glyph (string s) {
  SI dy;
  string ltype;
  font fn= search_font (s, dy, ltype);
  return move (fn->get_glyph (s), 0, dy);
}

int
rubber_stix_font_rep::index_glyph (string s, font_metric& fnm,
                                             font_glyphs& fng) {
  SI dy;
  string ltype;
  font fn= search_font (s, dy, ltype);
  if (dy != 0) cout << "TeXmacs] warning: glyph offset ignored\n";
  return fn->index_glyph (s, fnm, fng);
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

SI
rubber_stix_font_rep::get_lsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
rubber_stix_font_rep::get_lsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
rubber_stix_font_rep::get_rsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
rubber_stix_font_rep::get_rsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
}

SI
rubber_stix_font_rep::get_wide_correction  (string s, int mode) {
  font fn= search_font (s);
  return fn->get_wide_correction (s, mode);
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
