
/******************************************************************************
* MODULE     : font_translate.cpp
* DESCRIPTION: Compatibility between old and new font schemes
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "Freetype/tt_tools.hpp"
#include "analyze.hpp"

bool is_weight (string s);
bool is_category (string s);
bool is_glyphs (string s);
bool is_other (string s);

/******************************************************************************
* Translation into internal naming scheme
******************************************************************************/

string
get_family (array<string> v) {
  if (N(v) == 0) return "roman";
  return v[0];
}

string
get_variant (array<string> v) {
  array<string> r;
  for (int i=1; i<N(v); i++) {
    if (v[i] == "mono" && contains (string ("typewriter"), v));
    else if (v[i] == "mono" || v[i] == "typewriter")
      r << string ("tt");
    else if (v[i] == "sansserif")
      r << string ("ss");
    else if (v[i] == "digital" ||
	     v[i] == "pen" || v[i] == "artpen" ||
	     v[i] == "chalk" || v[i] == "marker")
      r << v[i];
    else if (is_category (v[i]))
      r << v[i];
    else if (is_glyphs (v[i]))
      r << v[i];
    else if (is_other (v[i]))
      r << v[i];
  }
  if (N(r) == 0) return "rm";
  return recompose (r, "-");
}

string
get_series (array<string> v) {
  for (int i=1; i<N(v); i++)
    if (is_weight (v[i]))
      return v[i];
  return "medium";
}

string
get_shape (array<string> v) {
  array<string> r;
  for (int i=1; i<N(v); i++)
    if (ends (v[i], "condensed") ||
        ends (v[i], "unextended") ||
        ends (v[i], "wide") ||
        v[i] == "proportional" ||
        (v[i] == "mono" && contains (string ("typewriter"), v)))
      r << v[i];
  for (int i=1; i<N(v); i++)
    if (v[i] == "upright") r << string ("right");
    else if (v[i] == "italic") r << string ("italic");
    else if (v[i] == "oblique") r << string ("slanted");
    else if (v[i] == "mathitalic") r << string ("mathitalic");
    else if (v[i] == "mathupright") r << string ("mathupright");
    else if (v[i] == "mathshape") r << string ("mathshape");
  for (int i=1; i<N(v); i++)
    if (v[i] == "smallcaps") r << string ("small-caps");
    else if (v[i] == "long") r << string ("long");
    else if (v[i] == "flat") r << string ("flat");
  if (N(r) == 0) return "right";
  return recompose (r, "-");
}

/******************************************************************************
* Upgrade old family names
******************************************************************************/

string
upgrade_family_name (string f) {
  static hashmap<string,string> t ("");
  if (N(t) == 0) {
    t ("luxi")= "Luxi";
    t ("ms-andalemo")= "Andale Mono";
    t ("ms-arial")= "Arial";
    t ("ms-comic")= "Comic Sans MS";
    t ("ms-courier")= "Courier New";
    t ("ms-georgia")= "Georgia";
    t ("ms-impact")= "Impact";
    t ("ms-lucida")= "Lucida Console";
    t ("ms-tahoma")= "Tahoma";
    t ("ms-times")= "Times New Roman";
    t ("ms-trebuchet")= "Trebuchet MS";
    t ("ms-verdana")= "Verdana";
    t ("apple-gothic")= "AppleGothic";
    t ("apple-lucida")= "Lucida Grande";
    t ("apple-mingliu")= "MingLiU";
    t ("apple-symbols")= "Apple Symbols";
    t ("apple-simsun")= "SimSun";
    t ("batang")= "Batang";
    t ("fireflysung")= "AR PL New Sung";
    t ("gulim")= "Baekmuk Gulim";
    t ("ipa")= "IPAMincho";
    t ("heiti")= "STHeiti";
    t ("kaku")= "Hiragino Kaku Gothic ProN";
    t ("kochi")= "Kochi Gothic";
    t ("lihei")= "LiHei Pro";
    t ("mingliu")= "MingLiU";
    t ("ms-gothic")= "MS Gothic";
    t ("ms-mincho")= "MS Mincho";
    t ("sazanami")= "Sazanami Mincho";
    t ("simfang")= "FangSong_GB2312";
    t ("simhei")= "SimHei";
    t ("simkai")= "KaiTi_GB2312";
    t ("simli")= "LiSu";
    t ("simsun")= "SimSun";
    t ("simyou")= "YouYuan";
    t ("ttf-japanese")= "TakaoPMincho";
    t ("ukai")= "AR PL ZenKai Uni";
    t ("uming")= "AR PL UMing CN";
    t ("unbatang")= "UnBatang";
    t ("wqy-microhei")= "WenQuanYi Micro Hei";
    t ("wqy-zenhei")= "WenQuanYi Zen Hei";
    t ("dejavu")= "DejaVu";
    t ("stix")= "Stix";
    t ("bonum")= "TeX Gyre Bonum";
    t ("chancery")= "TeX Gyre Chorus";
    t ("pagella")= "TeX Gyre Pagella";
    t ("schola")= "TeX Gyre Schola";
    t ("termes")= "TeX Gyre Termes";

    t ("adobe")= "Stix";
    t ("Duerer")= "duerer";
    t ("math-asana")= "Asana Math";
    t ("math-apple")= "Apple Symbols";
    t ("math-bonum")= "TeX Gyre Bonum";
    t ("math-dejavu")= "DejaVu";
    t ("math-lucida")= "Lucida Grande";
    t ("math-pagella")= "TeX Gyre Pagella";
    t ("math-schola")= "TeX Gyre Schola";
    t ("math-stix")= "Stix";
    t ("math-termes")= "TeX Gyre Termes";

    t ("modern")= "roman";
    t ("cyrillic")= "roman";
    t ("sys-chinese")= default_chinese_font_name ();
    t ("sys-japanese")= "roman";
    t ("sys-korean")= "roman";
  }
  if (t->contains (f)) return t[f];
  else return f;
}

/******************************************************************************
* Translation from internal naming scheme
******************************************************************************/

bool
is_other_internal (string s) {
  return
    is_other (s) &&
    s != "rm" &&
    s != "ss" &&
    s != "tt" &&
    s != "small-caps" &&
    s != "right" &&
    s != "slanted";
}

array<string>
variant_features (string s) {
  array<string> v= tokenize (s, "-");
  array<string> r;
  for (int i=0; i<N(v); i++)
    if (v[i] == "ss") r << string ("sansserif");
    else if (v[i] == "tt") r << string ("typewriter");
    else if (v[i] == "digital" ||
	     v[i] == "pen" || v[i] == "artpen" ||
	     v[i] == "chalk" || v[i] == "marker")
      r << v[i];
    else if (is_category (v[i]))
      r << v[i];
    else if (is_glyphs (v[i]))
      r << v[i];
    else if (is_other_internal (v[i]))
      r << v[i];
  return r;
}

array<string>
series_features (string s) {
  array<string> r;
  r << s;
  return r;
}

array<string>
shape_features (string s) {
  s= replace (s, "small-caps", "smallcaps");
  array<string> v= tokenize (s, "-");
  array<string> r;
  for (int i=0; i<N(v); i++)
    if (ends (v[i], "condensed") ||
        ends (v[i], "unextended") ||
        ends (v[i], "wide") ||
        v[i] == "mono" ||
        v[i] == "proportional" ||
        v[i] == "italic" ||
        v[i] == "mathitalic" ||
        v[i] == "mathupright" ||
        v[i] == "mathshape" ||
        v[i] == "smallcaps" ||
        v[i] == "long" ||
        v[i] == "flat")
      r << v[i];
    else if (v[i] == "right") r << string ("upright");
    else if (v[i] == "slanted") r << string ("oblique");
  return r;
}

array<string>
logical_font (string family, string variant, string series, string shape) {
  array<string> r;
  r << upgrade_family_name (family);
  r << variant_features (variant);
  r << series_features (series);
  r << shape_features (shape);
  array<string> v;
  for (int i=0; i<N(r); i++)
    if (r[i] != "medium" &&
        r[i] != "upright")
      v << r[i];
  //cout << family << ", " << variant << ", "
  //     << series << ", " << shape << " -> " << v << "\n";
  return v;
}

/******************************************************************************
* Find closest existing font
******************************************************************************/

bool
find_closest (string& family, string& variant, string& series, string& shape,
	      int attempt) {
  static hashmap<tree,tree> closest_cache (UNINIT);
  tree val= tuple (copy (family), variant, series, shape);
  tree key= tuple (copy (family), variant, series, shape, as_string (attempt));
  if (closest_cache->contains (key)) {
    tree t = closest_cache[key];
    family = t[0]->label;
    variant= t[1]->label;
    series = t[2]->label;
    shape  = t[3]->label;
    return t != val;
  }
  /*
  else if (attempt == FONT_ATTEMPTS && family != "modern") {
    family= "modern";
    find_closest (family, variant, series, shape, 0);
    tree t= tuple (family, variant, series, shape);
    closest_cache (key)= t;
    return t != val;
  }
  */
  else {
    //cout << "< " << family << ", " << variant
    //     << ", " << series << ", " << shape << "\n";
    array<string> lfn= logical_font (family, variant, series, shape);
    lfn= apply_substitutions (lfn);
    array<string> pfn= search_font (lfn, attempt);
    array<string> nfn= logical_font (pfn[0], pfn[1]);
    array<string> gfn= guessed_features (pfn[0], pfn[1]);
    //cout << lfn << " -> " << pfn << ", " << nfn << ", " << gfn << "\n";
    gfn << nfn;
    family= get_family (nfn);
    variant= get_variant (nfn);
    series= get_series (nfn);
    shape= get_shape (nfn);
    if ( contains (string ("outline"), lfn) &&
	!contains (string ("outline"), gfn))
      variant= variant * "-poorbbb";
    if ( contains (string ("bold"), lfn) &&
	!contains (string ("bold"), gfn))
      series= series * "-poorbf";
    if ( contains (string ("smallcaps"), lfn) &&
	!contains (string ("smallcaps"), gfn))
      shape= shape * "-poorsc";
    if ((contains (string ("italic"), lfn) ||
         contains (string ("oblique"), lfn)) &&
	!contains (string ("italic"), gfn) &&
        !contains (string ("oblique"), gfn))
      shape= shape * "-poorit";
    //cout << "> " << family << ", " << variant
    //     << ", " << series << ", " << shape << "\n";
    tree t= tuple (family, variant, series, shape);
    closest_cache (key)= t;
    return t != val;
  }
}

font
closest_font (string family, string variant, string series, string shape,
	      int sz, int dpi, int attempt) {
  string s=
    family * "-" * variant * "-" *
    series * "-" * shape * "-" *
    as_string (sz) * "-" * as_string (dpi) * "-" * as_string (attempt);
  if (font::instances->contains (s)) return font (s);
  find_closest (family, variant, series, shape, attempt);
  font fn= find_font (family, variant, series, shape, sz, dpi);
  //cout << "Found " << fn->res_name << "\n";
  font::instances (s)= (pointer) fn.rep;
  return fn;
}
