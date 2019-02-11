
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
  bool big_flag;
  array<bool> initialized;
  array<font> larger;
  translator virt;

  poor_rubber_font_rep (string name, font base);
  font   get_font (int nr);
  int    search_font (string s, string& r);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
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

#define MAGNIFIED_NUMBER 4
#define HUGE_ADJUST      1

bool
supports_big_operators (string res_name) {
  if (occurs (" Math", res_name))
    return occurs ("TeX Gyre ", res_name);
  if (occurs ("mathitalic", res_name))
    return occurs ("bonum", res_name) ||
           occurs ("pagella", res_name) ||
           occurs ("schola", res_name) ||
           occurs ("termes", res_name);
  return false;
}

poor_rubber_font_rep::poor_rubber_font_rep (string name, font base2):
  font_rep (name, base2), base (base2),
  big_flag (supports_big_operators (base2->res_name))
{
  this->copy_math_pars (base);
  initialized << true;
  larger << base;
  for (int i=1; i<=2*MAGNIFIED_NUMBER+5; i++) {
    initialized << false;
    larger << base;
  }
  virt= load_translator ("emu-large");
}

font
poor_rubber_font_rep::get_font (int nr) {
  ASSERT (nr < N(larger), "wrong font number");
  if (initialized[nr]) return larger[nr];
  initialized[nr]= true;
  if (nr <= 2*MAGNIFIED_NUMBER + 1) {
    int hnr= nr / 2;
    double zoomy= pow (2.0, ((double) hnr) / 4.0);
    double zoomx= sqrt (zoomy);
    if ((nr & 1) == 1) zoomx= sqrt (zoomx);
    larger[nr]= poor_stretched_font (base, zoomx, zoomy);
    //larger[nr]= base->magnify (zoomx, zoomy);
  }
  else if (nr == 2*MAGNIFIED_NUMBER + 2 || nr == 2*MAGNIFIED_NUMBER + 3) {
    int hdpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
    int vdpi= (72 * base->hpt + (PIXEL/2)) / PIXEL;
    font vfn= virtual_font (base, "emu-large", base->size, hdpi, vdpi, false);
    double zoomy= pow (2.0, ((double) MAGNIFIED_NUMBER) / 4.0);
    double zoomx= sqrt (zoomy);
    if ((nr & 1) == 1) zoomx= sqrt (zoomx);
    larger[nr]= poor_stretched_font (vfn, zoomx, zoomy);
    //larger[nr]= vfn->magnify (zoomx, zoomy);
  }
  else if (nr == 2*MAGNIFIED_NUMBER + 5) {
    int hdpi= (72 * this->wpt + (PIXEL/2)) / PIXEL;
    int vdpi= (72 * this->hpt + (PIXEL/2)) / PIXEL;
    font vfn= virtual_font (this, "emu-large", this->size, hdpi, vdpi, false);
    larger[nr]= vfn;
  }
  else
    larger[nr]= rubber_unicode_font (base);
  return larger[nr];
}

static hashset<string> thin_delims;

static bool
is_thin (string s) {
  if (N(thin_delims) == 0)
    thin_delims << string ("|") << string ("||") << string ("interleave")
                << string ("[") << string ("]")
                << string ("lfloor") << string ("rfloor")
                << string ("lceil") << string ("rceil")
                << string ("llbracket") << string ("rrbracket")
                << string ("dlfloor") << string ("drfloor")
                << string ("dlceil") << string ("drceil")
                << string ("tlbracket") << string ("trbracket")
                << string ("tlfloor") << string ("trfloor")
                << string ("tlceil") << string ("trceil");
  return thin_delims->contains (s);
}

int
poor_rubber_font_rep::search_font (string s, string& r) {
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    r= s;
    if (starts (s, "<big-iint") || starts (s, "<big-iiint") ||
	starts (s, "<big-iiiint") || starts (s, "<big-oint") ||
        starts (s, "<big-oiint") || starts (s, "<big-oiiint") ||
	starts (s, "<big-upiint") || starts (s, "<big-upiiint") ||
	starts (s, "<big-upiiiint") || starts (s, "<big-upoint") ||
        starts (s, "<big-upoiint") || starts (s, "<big-upoiiint") ||
	starts (s, "<big-amalg") || starts (s, "<big-pluscup"))
      if (!big_flag) return 2*MAGNIFIED_NUMBER + 5;
    if (starts (s, "<big-idotsint") || starts (s, "<big-upidotsint") ||
        starts (s, "<big-triangleup") || starts (s, "<big-box") ||
        starts (s, "<big-parallel") || starts (s, "<big-interleave"))
      return 2*MAGNIFIED_NUMBER + 5;
    if (big_flag && ends (s, "-1>") && base->supports (s)) return 0;
    return 2*MAGNIFIED_NUMBER + 4;
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s), num;
    if (pos > 6) { r= s (6, pos); num= as_int (s (pos+1, N(s)-1)); }
    else { r= s (6, N(s)-1); num= 0; }
    //cout << "Search " << base->res_name << ", " << s
    //     << ", " << r << ", " << num << LF;
    int nr= max (num - 5, 0);
    int thin= (is_thin (r)? 1: 0);
    int code;
    if (num <= MAGNIFIED_NUMBER ||
        r == "/" || r == "\\" ||
        r == "langle" || r == "rangle" ||
        r == "llangle" || r == "rrangle") {
      num= min (num, MAGNIFIED_NUMBER);
      if (N(r) > 1) r= "<" * r * ">";
      if (N(r)>1 && !base->supports (r)) {
        if (r == "<||>") r= "<emu-dbar>";
        else if (r == "<interleave>") r= "<emu-tbar>";
        else if (r == "<llbracket>") r= "<emu-dlbracket>";
        else if (r == "<rrbracket>") r= "<emu-drbracket>";
        else r= "<emu-" * r (1, N(r)-1) * ">";
      }
      else if (r == "\\" && base->supports ("/")) {
        metric ex1, ex2;
        base -> get_extents ("/", ex1);
        base -> get_extents ("\\", ex2);
        double h1= ex1->y2 - ex1->y1;
        double h2= ex2->y2 - ex2->y1;
        if (fabs ((h2/h1) - 1.0) > 0.05) r= "<emu-backslash>";
      }
      return 2*num + thin;
    }
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
    else if (r == "|")
      code= virt->dict ["<rubber-bar-#>"];
    else if (r == "||") {
      if (base->supports ("<||>"))
        code= virt->dict ["<rubber-parallel-#>"];
      else code= virt->dict ["<rubber-parallel*-#>"];
    }
    else if (r == "interleave") {
      if (base->supports ("<interleave>"))
        code= virt->dict ["<rubber-interleave-#>"];
      else code= virt->dict ["<rubber-interleave*-#>"];
    }
    else if (r == "lfloor" || r == "rfloor" ||
             r == "lceil" || r == "rceil" ||
             r == "llbracket" || r == "rrbracket") {
      if (base->supports ("<" * r * ">"))
        code= virt->dict ["<rubber-" * r * "-#>"];
      else code= virt->dict ["<rubber-" * r * "*-#>"];
    }
    else if (r == "dlfloor" || r == "drfloor" ||
             r == "dlceil" || r == "drceil" ||
             r == "tlbracket" || r == "trbracket" ||
             r == "tlfloor" || r == "trfloor" ||
             r == "tlceil" || r == "trceil")
      code= virt->dict ["<rubber-" * r * "-#>"];
    else if (r == "sqrt") {
      code= virt->dict ["<rubber-sqrt-#>"];
      r= string ((char) code) * as_string (nr + HUGE_ADJUST) * ">";
      return 2*MAGNIFIED_NUMBER + 5;
    }
    else
      code= virt->dict ["<rubber-lparenthesis-#>"];
    
    r= string ((char) code) * as_string (nr + HUGE_ADJUST) * ">";
    return 2*MAGNIFIED_NUMBER + 2 + thin;
  }
  r= s;
  return 0;
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_rubber_font_rep::supports (string s) {
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    if (big_flag && ends (s, "-1>") && base->supports (s)) return true;
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
    string r;
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) r= s (6, pos);
    else r= s (6, N(s)-1);
    //cout << "Check " << base->res_name << ", " << s
    //     << ", " << r << LF;
    if (r == "(" || r == ")" ||
        r == "[" || r == "]" ||
        r == "{" || r == "}" ||
        r == "/" || r == "\\" || r == "|")
      return base->supports (r);
    if (r == "langle" || r == "rangle" ||
        r == "llangle" || r == "rrangle") {
      if (base->supports ("<" * r * ">")) return true;
      return base->supports ("/");
    }
    if (r == "||" || r == "interleave") {
      if (base->supports ("<" * r * ">")) return true;
      return base->supports ("|");
    }
    if (r == "llbracket" || r == "rrbracket" ||
        r == "lfloor" || r == "rfloor" ||
        r == "lceil" || r == "rceil" ||
        r == "dlfloor" || r == "drfloor" ||
        r == "dlceil" || r == "drceil" ||
        r == "tlbracket" || r == "trbracket" ||
        r == "tlfloor" || r == "trfloor" ||
        r == "tlceil" || r == "trceil") {
      if (base->supports ("<" * r * ">")) return true;
      return base->supports ("[") && base->supports ("]");
    }
    if (r == "sqrt")
      if (base->supports ("<#23B7>")) return true;      
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
poor_rubber_font_rep::magnify (double zoomx, double zoomy) {
  return poor_rubber_font (base->magnify (zoomx, zoomy));
}

glyph
poor_rubber_font_rep::get_glyph (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_glyph (name);
}

int
poor_rubber_font_rep::index_glyph (string s, font_metric& fnm,
                                             font_glyphs& fng) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> index_glyph (name, fnm, fng);
}

/******************************************************************************
* Italic correction
******************************************************************************/

double
poor_rubber_font_rep::get_left_slope (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_left_slope (name);
}

double
poor_rubber_font_rep::get_right_slope (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_right_slope (name);
}

SI
poor_rubber_font_rep::get_left_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_left_correction (name);
}

SI
poor_rubber_font_rep::get_right_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_right_correction (name);
}

SI
poor_rubber_font_rep::get_lsub_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_lsub_correction (name);
}

SI
poor_rubber_font_rep::get_lsup_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_lsup_correction (name);
}

SI
poor_rubber_font_rep::get_rsub_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_rsub_correction (name);
}

SI
poor_rubber_font_rep::get_rsup_correction (string s) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_rsup_correction (name);
}

SI
poor_rubber_font_rep::get_wide_correction (string s, int mode) {
  string name;
  int num= search_font (s, name);
  return get_font (num) -> get_wide_correction (name, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_rubber_font (font base) {
  string name= "poorrubber[" * base->res_name * "]";
  font enh= virtual_enhance_font (base, "emu-bracket");
  //int hdpi= (72 * base->wpt + (PIXEL/2)) / PIXEL;
  //int vdpi= (72 * base->hpt + (PIXEL/2)) / PIXEL;
  //font enh= virtual_font (base, "emu-bracket", base->size, hdpi, vdpi, true);
  return make (font, name, tm_new<poor_rubber_font_rep> (name, enh));
}
