
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Rubber unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "converter.hpp"
#include "Freetype/tt_face.hpp"
#include "Freetype/tt_tools.hpp"
#include "translator.hpp"
#include "convert.hpp" // string_to_scheme_tree

#ifdef USE_FREETYPE

bool supports_big_operators (string res_name); // from poor_rubber.cpp
font rubber_unicode_font (font base, tt_face face);

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_unicode_font_rep: font_rep {
  font base;
  bool big_flag;
  array<bool> initialized;
  array<font> subfn;
  bool big_sums;

  // for opentype math font
  translator virt;
  tt_face    math_face;

  hashmap<string,int> mapper;
  hashmap<string,string> rewriter;

  rubber_unicode_font_rep (string name, font base, tt_face face= nullptr);
  font   get_font (int nr);
  int    search_font_sub (string s, string& rew);
  int    search_font_sub_opentype (string s, string& rew);
  int    search_font_cached (string s, string& rew);
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

rubber_unicode_font_rep::rubber_unicode_font_rep (string name, font base2,  
                                                  tt_face face):
  font_rep (name, base2), base (base2),
  big_flag (supports_big_operators (base2->res_name)),
  math_face (face)
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
  // number of subfonts 7, see get_font(int) for details
  int nr_subfonts= 7;
  for (int i=0; i<nr_subfonts; i++) {
    initialized << false;
    subfn << base;
  }
  if (base->math_type == MATH_TYPE_OPENTYPE) {
    big_flag    = true;
    big_sums    = true;
    string vname= "opentype_virtual[" * base->res_name * "]";
    virt        = tm_new<translator_rep> (vname);
    // virt->virt_def= array<tree> ();
    // virt->virt_def << tree (); // fill out the 0 glyph
  }

}

font
rubber_unicode_font_rep::get_font (int nr) {
  ASSERT (nr < N(subfn), "wrong font number");
  if (initialized[nr]) return subfn[nr];
  initialized[nr]= true;
  switch (nr) {
  case 0:
    break;
  case 1:
    subfn[nr]= base->magnify (sqrt (0.5));
    break;
  case 2:
    subfn[nr]= base->magnify (sqrt (2.0));
    break;
  case 3:
    subfn[nr]= base->magnify (2.0);
    break;
  case 4:
    subfn[nr]= rubber_assemble_font (base);
    break;
  case 5:
    // if opentype math font fails, use default rubber font
    subfn[nr]= font_rep::make_rubber_font (base);
    break;
  case 6:
    int hdpi= (72 * base->wpt + (PIXEL / 2)) / PIXEL;
    int vdpi= (72 * base->hpt + (PIXEL / 2)) / PIXEL;
    subfn[nr]=
        virtual_font (base, virt->res_name, base->size, hdpi, vdpi, false);
    break;
  }
  return subfn[nr];
}

/******************************************************************************
* Find the font
******************************************************************************/

int
parse_variant (string s, string& head, string& root) {
  // cout << "parse_variant for " << s << LF;
  int var= 0;
  if (!starts (s, "<") || !ends (s, ">") || N (s) < 3) return 0;
  root = s (1, N(s) - 1);
  array<string> v= tokenize (root, "-");
  if (N (v) == 3 && is_int (v[2])) {
    var = as_int (v[2]);
    root= v[1];
    head= v[0];
  }
  return var;
}

// construct string for extender
static string
extend (string s, bool ver) {
  return ver ? "(ver-take " * s * " 0.5 # 0.25)"
             : "(hor-take " * s * " 0.5 # 0.25)";
}

// construct string (scheme tree) from a list of glyphs
static string
glue (array<string> glyphs, bool ver) {
  string gluer= ver ? "glue-above" : "glue*";
  int    g_N  = N (glyphs);
  if (g_N == 0) return "";
  string result= glyphs[g_N - 1];
  for (int i= g_N - 2; i >= 0; --i) {
    result= "(" * gluer * " " * glyphs[i] * " " * result * ")";
  }
  return result;
}

int
rubber_unicode_font_rep::search_font_sub_opentype (string s, string& rew) {
  string root, head;
  int    var           = 0;
  bool   using_vertical= true; // verizontal or vertical, default is vertical
  rew                  = s;

  var= parse_variant (s, head, root);

  // there is no <big-xxx-0>
  if (starts (s, "<big-")) {
    var= max (0, var - 1);
  }

  if (root == "") return search_font_sub (s, rew);

  string uu= N (root) > 1 ? strict_cork_to_utf8 ("<" * root * ">") : root;

  int          j      = 0;
  uint32_t     u      = decode_from_utf8 (uu, j);
  unsigned int glyphID= ft_get_char_index (math_face->ft_face, u);

  // cout << "unicode " << uu << " -> " << lolly::data::to_hex (u) << LF;
  // cout << "search_font_sub_opentype for " << s << " -> " << glyphID << LF;

  bool         has_variants= false;
  bool         has_assembly= false;
  ot_mathtable math_table  = math_face->math_table;

  // a glyph can not be both vertical and horizontal
  if (math_table->ver_glyph_variants->contains (glyphID)) {
    using_vertical= true;
    has_variants  = true;
    // if a glyph has assembly, it must be contained in the variants table
    has_assembly= math_table->ver_glyph_assembly->contains (glyphID);
  }
  else if (math_table->hor_glyph_variants->contains (glyphID)) {
    has_variants  = true;
    using_vertical= false;
    has_assembly  = math_table->hor_glyph_assembly->contains (glyphID);
  }

  auto glyph_variants= using_vertical
                           ? math_face->math_table->ver_glyph_variants
                           : math_face->math_table->hor_glyph_variants;

  auto glyph_assembly= using_vertical
                           ? math_face->math_table->ver_glyph_assembly
                           : math_face->math_table->hor_glyph_assembly;

  // turn a number to a 4-digit hexadecimal string "@XXXX"
  auto hex4= [] (int x) { return "@" * as_hexadecimal (x, 4); };

  if (has_variants) {
    auto& gv= glyph_variants (glyphID);
    if (var < N (gv)) {
      int res= gv[var];
      // use <@XXXX> for native glyph id
      rew= "<" * hex4 (res) * ">";
      // the unicode font itself has the variant glyph
      return 0;
    }
  }

  if (has_assembly) {
    string virt_glyph;
    // <xx-xx-#>, '#' can match any variant number
    string ss= "<" * head * "-" * root * "-#>";
    if (!virt->dict->contains (ss)) {
      auto& gass = glyph_assembly (glyphID);
      tree  glyph= tree ();

      array<string> glyphs;
      for (int i= 0; i < N (gass.partRecords); i++) {
        auto& pr= gass.partRecords[i];
        // whether the part is a extender
        if (pr.partFlags == 0x0001) {
          glyphs << extend (hex4 (pr.glyphID), using_vertical);
        }
        else {
          glyphs << hex4 (pr.glyphID);
        }
      }
      virt_glyph= glue (glyphs, using_vertical);

      glyph          = string_to_scheme_tree (virt_glyph);
      virt->dict (ss)= N (virt->virt_def);
      virt->virt_def << glyph;

      // subfn[6] is the virtual font for opentype math font
      // FIXME: can we only add the new glyph to the virtual font instead of
      // reset the whole virtual font?
      if (initialized[6]) {
        // fresh the virtual font, since new virtual glyph is added
        font::instances->reset (subfn[6]->res_name);
        initialized[6]= false;
      }
    }
    return 6;
  }
  // cout << "No opentype variant for " << uu << " -> " << glyphID << LF;

  // try to use subfont
  int nr= search_font_sub (s, rew);
  // if nr == 0, failed to find the sub font from subfn[1:4]
  // use default rubber font subfn[5]
  return nr == 0 ? 5 : nr;
}

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
    if (big_flag && base->supports (s)) {
      rew= s;
      return 0;
    }
    string r= s (5, N(s) - 3);
    if (ends (r, "lim")) r= r (0, N(r) - 3);
    if (starts (r, "up")) r= r (2, N(r));
    if (big_flag && base->supports ("<big-" * r * "-1>")) {
      rew= "<big-" * r * "-1>";
      return 2;
    }
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
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") { rew= ""; return 0; }
      if ((r == "(" && base->supports ("<#239C>")) ||
          (r == ")" && base->supports ("<#239F>")) ||
          (r == "[" && base->supports ("<#23A2>")) ||
          (r == "]" && base->supports ("<#23A5>")) ||
          ((r == "{" || r == "}") && base->supports ("<#23AA>")) ||
          (r == "sqrt" && base->supports ("<#23B7>"))) {
        rew= s;
        return 4;
      }
      rew= r;
      if (N(rew) > 1) rew= "<" * rew * ">";
      if (ends (s, "-0>")) return 0;
      return 0;
    }
  }
  rew= s;
  return 0;
}

int
rubber_unicode_font_rep::search_font_cached (string s, string& rew) {
  // cout << "search_font_cached for " << s << LF;
  if (mapper->contains (s)) {
    rew= rewriter[s];
    return mapper[s];
  }
  int nr= 0;
  if (!is_nil (math_face) && !is_nil (math_face->math_table)) {
    nr= search_font_sub_opentype (s, rew);
  }
  else {
    nr= search_font_sub (s, rew);
  }
  mapper(s)= nr;
  rewriter(s)= rew;
  //cout << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_unicode_font_rep::search_font (string& s) {
  string rew;
  int nr= search_font_cached (s, rew);
  s= rew;
  return get_font (nr);
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
      if (s[pos-1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") return true;
      if (r == "sqrt") return base->supports ("<#23B7>");
      if (N(r) > 1) r= "<" * r * ">";
      if (!base->supports (r)) return false;
      if (ends (s, "-0>")) return true;
      if (r == "(") return base->supports ("<#239C>");
      if (r == ")") return base->supports ("<#239F>");
      if (r == "[") return base->supports ("<#23A2>");
      if (r == "]") return base->supports ("<#23A5>");
      if (r == "{" || r == "}") return base->supports ("<#23AA>");
      return true;
    }
  }
  return base->supports (s);
}

void
rubber_unicode_font_rep::get_extents (string s, metric& ex) {
  font fn= search_font (s);
  fn->get_extents (s, ex);
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
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y);
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y, xk);
}

font
rubber_unicode_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_unicode_font (base->magnify (zoomx, zoomy), math_face);
}

glyph
rubber_unicode_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

int
rubber_unicode_font_rep::index_glyph (string s, font_metric& fnm,
                                                font_glyphs& fng) {
  font fn= search_font (s);
  return fn->index_glyph (s, fnm, fng);
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

SI
rubber_unicode_font_rep::get_lsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
rubber_unicode_font_rep::get_lsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
rubber_unicode_font_rep::get_rsub_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
rubber_unicode_font_rep::get_rsup_correction  (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
}

SI
rubber_unicode_font_rep::get_wide_correction  (string s, int mode) {
  font fn= search_font (s);
  return fn->get_wide_correction (s, mode);
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base));
}

font
rubber_unicode_font (font base, tt_face face) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base, face));
}

#else

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

font
rubber_unicode_font (font base, tt_face face) {
  return rubber_unicode_font (base);
}


#endif
