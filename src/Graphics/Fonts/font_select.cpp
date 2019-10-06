
/******************************************************************************
* MODULE     : font_select.cpp
* DESCRIPTION: New mechanisms for font selection
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "Freetype/tt_tools.hpp"
#include "analyze.hpp"

extern hashmap<tree,tree> font_features;
extern hashmap<tree,tree> font_variants;
array<string> remove_other (array<string> a, bool keep_glyphs= true);
bool same_kind (string s1, string s2);
bool is_glyphs (string s);
bool is_category (string s);
string upgrade_family_name (string f);

/******************************************************************************
* Basic subroutines
******************************************************************************/

array<string>
common (array<string> v1, array<string> v2) {
  array<string> r;
  for (int i=0; i<N(v1); i++)
    for (int j=0; j<N(v2); j++)
      if (v1[i] == v2[j]) {
        r << v1[i];
        break;
      }
  return r;
}

array<string>
exclude (array<string> a, array<string> b) {
  array<string> r;
  for (int i=0; i<N(a); i++)
    if (!contains (a[i], b))
      r << a[i];
  return r;
}

array<string>
exclude (array<string> a, string s) {
  array<string> r;
  for (int i=0; i<N(a); i++)
    if (a[i] != s) r << a[i];
  return r;
}

array<string>
remove_duplicates (array<string> a) {
  array<string> r;
  for (int i=0; i<N(a); i++)
    if (!contains (a[i], r))
      r << a[i];
  return r;
}

tree
array_as_tuple (array<string> a) {
  return tree (a);
}

array<string>
tuple_as_array (tree t) {
  ASSERT (is_func (t, TUPLE), "tuple expected");
  array<string> r;
  for (int i=0; i<N(t); i++) {
    ASSERT (is_atomic (t[i]), "string expected");
    r << t[i]->label;
  }
  return r;
}

/******************************************************************************
* Decoding and encoding of features
******************************************************************************/

string
decode_feature (string s) {
  s= replace (locase_all (s) , " ", "");
  if (s == "smallcapitals") s= "smallcaps";
  if (s == "monospaced") s= "mono";
  return s;
}

string
encode_feature (string s) {
  s= upcase_first (s);
  if (s == "Smallcaps") s= "SmallCaps";
  if (s == "Sansserif") s= "SansSerif";
  else if (ends (s, "bold"))
    s= s (0, N(s)-4) * upcase_first (s (N(s)-4, N(s)));
  else if (ends (s, "condensed"))
    s= s (0, N(s)-9) * upcase_first (s (N(s)-9, N(s)));
  return s;
}

/******************************************************************************
* Standardization of font features
******************************************************************************/

string
normalize_feature (string s) {
  s= locase_all (s);
  if (s == "ultralight") s= "thin";
  if (s == "nonextended") s= "unextended";
  if (s == "extended") s= "wide";
  if (s == "caption") s= "wide";
  return s;
}

array<string>
family_features (string f) { 
  font_database_load ();
  array<string> r;
  if (font_features->contains (tree (f))) {
    tree t= font_features [tree (f)];
    if (is_func (t, TUPLE) && N(t) >= 1 && is_atomic (t[0])) {
      for (int i=1; i<N(t); i++)
        if (is_atomic (t[i]))
          r << t[i]->label;
      for (int i=0; i<N(r); i++)
        r[i]= normalize_feature (r[i]);
      return r;
    }
  }
  if (occurs ("Mono", f) ||
      occurs ("Console", f) ||
      occurs ("Typewriter", f))
    r << string ("Mono");
  if (occurs ("ArtPen", f) || occurs ("Art Pen", f))
    r << string ("ArtPen");
  else if (occurs ("Pen ", f) || ends (f, "Pen"))
    r << string ("Pen");
  if (occurs ("Sans", f))
    r << string ("SansSerif");
  if (occurs ("DemiCondensed", f) ||
      occurs ("Demi Condensed", f))
    r << string ("DemiCondensed");
  else if (occurs ("Condensed", f) ||
           occurs ("Narrow", f))
    r << string ("Condensed");
  else if (occurs ("Caption", f))
    r << string ("Wide");
  for (int i=0; i<N(r); i++)
    r[i]= normalize_feature (r[i]);
  return remove_duplicates (r);
}

array<string>
style_features (string s) {
  s= replace (s, "-", " ");
  string r;
  for (int i=0; i<N(s); i++)
    if (is_upcase (s[i]) &&
        (i+1 < N(s)) &&
        is_locase (s[i+1]))
      r << ' ' << s[i];
    else r << s[i];
  array<string> v= tokenize (r, " ");
  if (N(v) == 0) return array<string> ();
  r= "";
  for (int i=0; i<N(v); i++) {
    if (i+1 < N(v) &&
        v[i] == "Regular" &&
        (v[i+1] == "Thin" ||
         v[i+1] == "Light" ||
         v[i+1] == "Heavy" ||
         v[i+1] == "Black"))
      continue;
    if (N(r) == 0 ||
        v[i-1] == "Demi" ||
        v[i-1] == "Extra" ||
        v[i-1] == "Semi" ||
        v[i-1] == "Ultra" ||
        v[i-1] == "Small");
    else r << ' ';
    if (v[i] == "Regular");
    else if (v[i] == "Medium");
    else if (v[i] == "Normal");
    else if (v[i] == "Roman");
    else if (v[i] == "Upright");
    else if (v[i] == "Nonextended");
    else if (v[i] == "Unextended");
    else if (v[i] == "Nonstretched");
    else if (v[i] == "Unstretched");
    else if (v[i] == "Book"); // for DejaVu fonts
    else if (v[i] == "Slanted") r << "Oblique";
    else if (v[i] == "Inclined") r << "Italic";
    else if (v[i] == "Versalitas") r << "SmallCaps";
    else r << v[i];
  }
  v= tokenize (r, " ");
  array<string> w;
  for (int i=0; i<N(v); i++)
    if (v[i] != "")
      w << normalize_feature (v[i]);
  return remove_duplicates (w);
}

array<string>
logical_font (string family, string style) {
  array<string> r;
  //cout << family << ", " << style
  //     << " -> " << family_to_master (family)
  //     << ", " << family_features (family)
  //     << ", " << family_strict_features (family)
  //     << ", " << style_features (style) << "\n";
  r << family_to_master (family);
  r << family_strict_features (family);
  r << style_features (style);
  //cout << family << ", " << style << " -> " << r << "\n";
  return remove_duplicates (r);
}

array<string>
glyph_features (string family, string style) {
  array<string> r;
  array<string> a= font_database_characteristics (family, style);
  for (int i=0; i<N(a); i++)
    if (is_glyphs (locase_all (a[i])))
      r << normalize_feature (a[i]);
  return r;
}

array<string>
logical_font_exact (string family, string style) {
  array<string> r;
  r << family_to_master (family);
  r << family_features (family);
  r << style_features (style);
  r << glyph_features (family, style);
  if (contains (string ("gothic"), r))
    if (contains (string ("cjk"), r) || contains (string ("hangul"), r)) {
      r << string ("sansserif");
      r= exclude (r, "gothic");
    }
  //cout << family << ", " << style << " -> " << r << "\n";
  return remove_duplicates (r);
}


array<string>
logical_font_enrich (array<string> v) {
  if (N(v) == 0) return v;
  string master= v[0];
  array<string> r;
  r << master << master_features (master);
  return patch_font (r, range (v, 1, N(v)), false);
}

/******************************************************************************
* Master font families
******************************************************************************/

string
Replace (string s, string w, string b) {
  if (N(s) == 0) return s;
  return s (0, 1) * replace (s (1, N(s)), w, b);
}

string
family_to_master (string f) {
  if (occurs (",", f) && occurs ("=", f)) f= main_family (f);
  f= upgrade_family_name (f);
  font_database_load ();
  if (!font_features->contains (tree (f)) &&
      f != "tcx" && f != "tc") {
    cout << "TeXmacs] missing '" << f << "' family\n";
    font_database_global_load ();
  }
  if (font_features->contains (tree (f))) {
    tree t= font_features [tree (f)];
    if (is_func (t, TUPLE) && N(t) >= 1 && is_atomic (t[0]))
      return t[0]->label;
  }
  f= replace (f, " Mono", "");
  f= Replace (f, "Mono", "");
  f= replace (f, " Console", "");
  f= Replace (f, "Console", "");
  f= replace (f, " Typewriter", "");
  f= Replace (f, "Typewriter", "");
  f= replace (f, " ArtPen", "");
  f= Replace (f, "ArtPen", "");
  f= replace (f, " Art", "");
  f= Replace (f, "Art", "");
  f= replace (f, " Pen", "");
  f= Replace (f, "Pen", "");
  f= replace (f, " Sans", "");
  f= Replace (f, "Sans", "");
  f= replace (f, " Serif", "");
  f= Replace (f, "Serif", "");
  f= replace (f, " Demi", "");
  f= Replace (f, "Demi", "");
  f= replace (f, " Condensed", "");
  f= Replace (f, "Condensed", "");
  f= replace (f, " Narrow", "");
  f= Replace (f, "Narrow", "");
  f= replace (f, " Caption", "");
  f= Replace (f, "Caption", "");
  f= replace (f, " Thin", "");
  f= Replace (f, "Thin", "");
  f= replace (f, " Light", "");
  f= Replace (f, "Light", "");
  f= replace (f, " Medium", "");
  f= Replace (f, "Medium", "");
  f= replace (f, " Bold", "");
  f= Replace (f, "Bold", "");
  f= replace (f, " Heavy", "");
  f= Replace (f, "Heavy", "");
  f= replace (f, " Black", "");
  f= Replace (f, "Black", "");
  return f;
}

array<string>
master_to_families (string m) {
  if (occurs (",", m) && occurs ("=", m)) m= main_family (m);
  m= upgrade_family_name (m);
  font_database_load ();
  if (!font_variants->contains (tree (m)) &&
      m != "tcx" && m != "tc") {
    cout << "TeXmacs] missing '" << m << "' master\n";
    font_database_global_load ();
  }
  array<string> r;
  if (font_variants->contains (tree (m))) {
    tree t= font_variants [tree (m)];
    for (int i=0; i<N(t); i++)
      if (is_atomic (t[i]))
        r << t[i]->label;
  }
  if (N(r) == 0) r << m;
  return r;
}

array<string>
master_features (string m) {
  array<string> fams= master_to_families (m);
  if (N(fams) == 0) return array<string> ();
  array<string> r= family_features (fams[0]);
  for (int i=1; i<N(fams); i++)
    r= common (r, family_features (fams[i]));
  return r;
}

array<string>
family_strict_features (string f) {
  string m= family_to_master (f);
  array<string> ff= family_features (f);
  array<string> mf= master_features (m);
  return exclude (ff, mf);
}

/******************************************************************************
* Predicates for font features
******************************************************************************/

bool
is_stretch (string s) {
  return
    ends (s, "condensed") ||
    ends (s, "unextended") ||
    ends (s, "wide") ||
    ends (s, "caption");
}

bool
is_weight (string s) {
  return 
    ends (s, "thin") ||
    ends (s, "light") ||
    s == "regular" ||
    s == "medium" ||
    ends (s, "bold") ||
    ends (s, "heavy") ||
    ends (s, "black");
}

bool
is_slant (string s) {
  return 
    s == "upright" ||
    s == "italic" ||
    s == "oblique" ||
    s == "mathitalic" ||
    s == "mathupright" ||
    s == "mathshape";
}

bool
is_capitalization (string s) {
  return 
    s == "mixed" ||
    s == "smallcaps";
}

bool
is_serif (string s) {
  return 
    s == "serif" ||
    s == "sansserif";
}

bool
is_spacing (string s) {
  return 
    s == "typewriter" ||
    s == "mono" ||
    s == "proportional";
}

bool
is_device (string s) {
  return 
    s == "print" ||
    s == "typewriter" ||
    s == "digital" ||
    s == "pen" ||
    s == "artpen" ||
    s == "chalk" ||
    s == "marker";
}

bool
is_category (string s) {
  return 
    s == "ancient" ||
    s == "attached" ||
    s == "calligraphic" ||
    s == "comic" ||
    s == "decorative" ||
    s == "distorted" ||
    s == "gothic" ||
    s == "handwritten" ||
    s == "initials" ||
    s == "medieval" ||
    s == "miscellaneous" ||
    s == "gothic" ||
    s == "outline" ||
    s == "retro" ||
    s == "scifi" ||
    s == "title";
}

bool
is_glyphs (string s) {
  return 
    s == "ascii" ||
    s == "latin" ||
    s == "greek" ||
    s == "cyrillic" ||
    s == "cjk" ||
    s == "hangul" ||
    s == "mathsymbols" ||
    s == "mathextra" ||
    s == "mathletters";
}

bool
is_other (string s) {
  return
    !is_stretch (s) &&
    !is_weight (s) &&
    !is_slant (s) &&
    !is_capitalization (s) &&
    !is_serif (s) &&
    !is_spacing (s) &&
    !is_device (s) &&
    !is_category (s) &&
    !is_glyphs (s) &&
    s != "long" &&
    s != "flat";
}

bool
same_kind (string s1, string s2) {
  return
    (is_stretch (s1) && is_stretch (s2)) ||
    (is_weight (s1) && is_weight (s2)) ||
    (is_slant (s1) && is_slant (s2)) ||
    (is_capitalization (s1) && is_capitalization (s2)) ||
    (is_serif (s1) && is_serif (s2)) ||
    (is_spacing (s1) && is_spacing (s2)) ||
    (is_device (s1) && is_device (s2)) ||
    (is_category (s1) && is_category (s2)) ||
    (is_glyphs (s1) && is_glyphs (s2));
}

/******************************************************************************
* Computing the distance between two fonts
******************************************************************************/

#define S_STRETCH         10
#define D_STRETCH         30
#define S_WEIGHT          100
#define S_SLANT           100
#define N_WEIGHT          200
#define Q_WEIGHT          300
#define D_WEIGHT          1000
#define D_SLANT           1000
#define D_CAPITALIZATION  3000
#define D_MASTER          10000
#define D_SERIF           100000
#define D_SPACING         100000
#define Q_DEVICE          300000
#define D_DEVICE          1000000
#define Q_CATEGORY        300000
#define D_CATEGORY        1000000
#define D_GLYPHS          3000000
#define D_HUGE            30000000
#define D_INFINITY        1000000000

int
distance (string s1, string s2, bool asym) {
  // NOTE: distances can be asymmetric.
  // For instance, 'bold' matches 'black' with distance Q_WEIGHT,
  // but 'black' does not match 'bold'.
  if (s1 == s2) return 0;
  if (is_stretch (s1) || is_stretch (s2)) {
    if (!is_stretch (s1) || !is_stretch (s2)) return D_HUGE;
    if (ends (s1, "condensed") && ends (s2, "condensed")) return S_STRETCH;
    if (ends (s1, "unextended") && ends (s2, "unextended")) return S_STRETCH;
    if (ends (s1, "wide") && ends (s2, "wide")) return S_STRETCH;
    if (ends (s1, "wide") && ends (s2, "caption")) return S_STRETCH;
    if (ends (s1, "caption") && ends (s2, "wide")) return S_STRETCH;
    return D_STRETCH;
  }
  if (is_weight (s1) || is_weight (s2)) {
    if (!is_weight (s1) || !is_weight (s2)) return D_HUGE;
    if (ends (s1, "light") && ends (s2, "light")) return S_WEIGHT;
    if (ends (s1, "bold") && ends (s2, "bold")) return S_WEIGHT;
    if (ends (s1, "black") && ends (s2, "black")) return S_WEIGHT;
    if (ends (s1, "light") && ends (s2, "thin")) return Q_WEIGHT;
    if (ends (s1, "thin") && ends (s2, "light") && !asym) return Q_WEIGHT;
    if (ends (s1, "bold") && ends (s2, "heavy")) return N_WEIGHT;
    if (ends (s1, "heavy") && ends (s2, "bold") && !asym) return N_WEIGHT;
    if (ends (s1, "heavy") && ends (s2, "black")) return N_WEIGHT;
    if (ends (s1, "black") && ends (s2, "heavy") && !asym) return N_WEIGHT;
    if (ends (s1, "bold") && ends (s2, "black")) return Q_WEIGHT;
    if (ends (s1, "black") && ends (s2, "bold") && !asym) return Q_WEIGHT;
    return D_WEIGHT;
  }
  if (is_slant (s1) || is_slant (s2)) {
    if (!is_slant (s1) || !is_slant (s2)) return D_HUGE;
    if (s1 == "italic" && s2 == "oblique") return S_SLANT;
    if (s1 == "oblique" && s2 == "italic") return S_SLANT;
    return D_SLANT;
  }
  if (is_capitalization (s1) || is_capitalization (s2)) {
    if (!is_capitalization (s1) || !is_capitalization (s2)) return D_HUGE;
    return D_CAPITALIZATION;
  }
  if (is_serif (s1) || is_serif (s2)) {
    if (!is_serif (s1) || !is_serif (s2)) return D_HUGE;
    return D_SERIF;
  }
  if (is_spacing (s1) || is_spacing (s2)) {
    if (!is_spacing (s1) || !is_spacing (s2)) return D_HUGE;
    if (s1 == "mono" && s2 == "typewriter") return 0;
    if (s1 == "typewriter" && s2 == "mono") return 0;
    return D_SPACING;
  }
  if (is_device (s1) || is_device (s2)) {
    if (!is_device (s1) || !is_device (s2)) return D_HUGE;
    if (s1 == "pen" && s2 == "artpen") return Q_DEVICE;
    if (s1 == "pen" && s2 == "marker") return Q_DEVICE;
    if (s1 == "pen" && s2 == "chalk") return Q_DEVICE;
    if (s1 == "artpen" && s2 == "pen" && !asym) return Q_DEVICE;
    if (s1 == "marker" && s2 == "pen" && !asym) return Q_DEVICE;
    if (s1 == "chalk" && s2 == "pen" && !asym) return Q_DEVICE;
    return D_DEVICE;
  }
  if (is_category (s1) || is_category (s2)) {
    if (!is_category (s1) || !is_category (s2)) return D_HUGE;
    if (s1 == "retro" && s2 == "medieval") return Q_CATEGORY;
    if (s1 == "medieval" && s2 == "retro") return Q_CATEGORY;
    return D_CATEGORY;
  }
  if (is_glyphs (s1) || is_glyphs (s2)) {
    if (!is_glyphs (s1) || !is_glyphs (s2)) return D_HUGE;
    return D_GLYPHS;
  }
  return D_HUGE;
}

bool
contains (array<string> a, bool (*pred) (string)) {
  for (int i=0; i<N(a); i++)
    if (pred (a[i])) return true;
  return false;
}

int
distance (string s, array<string> v, bool asym) {
  if (s == "unextended" && !contains (v, is_stretch)) return 0;
  if (s == "medium" && !contains (v, is_weight)) return 0;
  if (s == "normal" && !contains (v, is_slant)) return 0;
  if (s == "mixed" && !contains (v, is_capitalization)) return 0;
  if (s == "serif" && !contains (v, is_serif)) return 0;
  if (s == "proportional" && !contains (v, is_spacing)) return 0;
  if (s == "print" && !contains (v, is_device)) return 0;

  if (s == "mono" && contains (string ("proportional"), v)) return D_SPACING;
  if (s == "proportional" && contains (string ("mono"), v)) return D_SPACING;

  int m= D_HUGE;
  if (is_stretch (s)) m= D_STRETCH;
  else if (is_weight (s)) m= D_WEIGHT;
  else if (is_slant (s)) m= D_SLANT;
  else if (is_capitalization (s)) m= D_CAPITALIZATION;
  else if (is_serif (s)) m= D_SERIF;
  else if (is_spacing (s)) m= D_SPACING;
  else if (is_device (s)) m= D_DEVICE;
  else if (is_category (s)) m= D_CATEGORY;
  else if (is_glyphs (s)) m= D_GLYPHS;

  for (int i=1; i<N(v); i++)
    m= min (distance (s, v[i], asym), m);
  return m;
}

int
distance (array<string> v, array<string> vx,
          array<string> w, array<string> wx) {
  // NOTE: v typically contains required properties of a font,
  // vx the implicit superset of properties for v induced by the font,
  // wx the full set of properties of the candidate font and
  // w a subset of wx of those properties which are not common
  // between all styles in the same family.
  int d= 0;
  if (N(v) == 0 || N(w) == 0) return D_INFINITY;
  if (v[0] != w[0]) d= D_MASTER;
  for (int i=1; i<N(v); i++)
    d += distance (v[i], wx, false);
  for (int i=1; i<N(w); i++)
    d += distance (w[i], vx, false);
  if (v[0] != w[0] && !contains (string ("ascii"), wx)) d += D_GLYPHS;
  return d;
}

int
distance (array<string> v, array<string> w) {
  return distance (v, v, w, w);
}

/******************************************************************************
* Compute a best possible approximation for font
******************************************************************************/

array<string>
remove_other (array<string> a, bool keep_glyphs) {
  array<string> r;
  if (N(a)>0) r << a[0];
  for (int i=1; i<N(a); i++)
    if (!is_other (a[i]))
      if (keep_glyphs || !is_glyphs (a[i]))
        r << a[i];
  return r;
}

void
search_font_among (array<string> v, array<string> fams, array<string> avoid,
                   int& best_d1, array<string>& best_result, bool strict) {
  //if (N(fams) < 20)
  //  cout << "  Search among " << fams << ", " << strict << "\n";
  //else
  //  cout << "  Search among many " << strict << "\n";
  array<string> vx= logical_font_enrich (v);
  if (!strict) v = remove_other (v);
  if (!strict) vx= remove_other (vx);
  //cout << "    Searching " << v << ", " << vx << "\n";
  best_d1= D_INFINITY;
  int best_d2= D_INFINITY + 1;
  double best_d3= 1000000.0;
  best_result= array<string> (v[0], string ("Unknown"));
  for (int i=0; i<N(fams); i++)
    if (N (avoid) == 0 || !contains (family_to_master (fams[i]), avoid)) {
      array<string> stys= font_database_styles (fams[i]);
      for (int j=0; j<N(stys); j++) {
	array<string> w = logical_font (fams[i], stys[j]);
	array<string> wx= logical_font_exact (fams[i], stys[j]);
	if (!strict) w = remove_other (w);
	if (!strict) wx= remove_other (wx);
	int d1= distance (v, vx, w, wx);
        //cout << "  " << w << ", " << wx << " -> " << d1 << "\n";
	int d2= D_INFINITY + 1;
	if (d1 == best_d1 || best_d2 == D_INFINITY + 1)
	  d2= distance (remove_other (vx, false), remove_other (wx, false));
	double d3= 1000000.0;
	if ((d1 == best_d1 && d2 <= best_d2) || best_d3 == 1000000.0)
	  d3= guessed_distance (v[0], w[0]);
	if ((d1 <  best_d1) ||
	    (d1 == best_d1 && d2 <  best_d2) ||
	    (d1 == best_d1 && d2 == best_d2 && d3 < best_d3)) {
	  best_d1= d1;
	  best_d2= d2;
	  best_d3= d3;
	  best_result= array<string> (fams[i], stys[j]);
	  //cout << "  Better " << w << ", " << wx
	  //     << " -> " << d1 << ", " << d2 << ", " << d3 << "\n";
	}
      }
    }
  //cout << "  Best result: " << best_result
  //     << ", " << best_d1 << ", " << best_d2 << ", " << best_d3 << "\n";
}

array<string>
search_font (array<string> v, bool require_exact, array<string> avoid) {
  if (N(v) == 0)
    return array<string> (string ("TeXmacs Computer Modern"),
                          string ("Unknown"));
  //cout << "Searching " << v << ", " << logical_font_enrich (v)
  //     << (require_exact? string (" (exact)"): string ("")) << "\n";
  int best_distance;
  array<string> best_result;
  array<string> fams= master_to_families (v[0]);
  bool found= false;
  if (require_exact || N(remove_other(v)) <= 1) {
    search_font_among (v, fams, avoid, best_distance, best_result, true);
    if (best_distance == 0 || require_exact) found= true;
    if (best_distance > 0 && require_exact) {
      string s;
      for (int i=1; i<N(v); i++) {
        if (i>1) s << " ";
        s << encode_feature (v[i]);
      }
      best_result[1]= s;
    }
  }
  if (!found) {
    search_font_among (v, fams, avoid, best_distance, best_result, false);
    if (best_distance < D_MASTER)
      search_font_among (v, fams, avoid, best_distance, best_result, true);
    else {
      fams= font_database_families ();
      search_font_among (v, fams, avoid, best_distance, best_result, false);
      string master= family_to_master (best_result[0]);
      fams= master_to_families (master);
      search_font_among (v, fams, avoid, best_distance, best_result, true);
    }
  }
  //cout << "Found " << best_result << ", " << best_distance << "\n";
  return best_result;
}

array<string>
search_font (array<string> v, int attempt) {
  static hashmap<tree,tree> cache (UNINIT);
  tree key= array_as_tuple (v);
  key << as_string (attempt);
  if (cache->contains (key))
    return tuple_as_array (cache[key]);
  array<string> black_list;
  for (int i=1; i<attempt; i++) {
    array<string> a= search_font (v, i);
    black_list << family_to_master (a[0]);
  }
  array<string> r= search_font (v, false, black_list);
  cache (key)= array_as_tuple (r);
  return r;
}

array<string>
search_font_exact (array<string> v) {
  array<string> black_list;
  return search_font (v, true, black_list);
}

/******************************************************************************
* Searching font families by properties
******************************************************************************/

array<string>
search_font_styles (string family, array<string> v) {
  array<string> styles= font_database_styles (family);
  if (N(v) == 0) return styles;
  array<string> empty;
  v= copy (v);
  for (int i=0; i<N(v); i++)
    v[i]= decode_feature (v[i]);

  array<string> r;
  for (int i=0; i<N(styles); i++) {
    int j;
    array<string> w= logical_font_exact (family, styles[i]);
    for (j=0; j<N(v); j++) {
      string property= decode_feature (v[j]);
      int d= distance (v[j], w, true);
      //cout << "Test " << v  << ", " << w << " -> "
      //     << d << ", " << distance (v[j], empty) << "\n";
      if (d != 0 && d >= distance (v[j], empty, true)) break;
    }
    if (j == N(v)) r << styles[i];
  }
  return r;
}

array<string>
search_font_families (array<string> v) {
  array<string> families= font_database_families ();
  if (N(v) == 0) return families;
  array<string> r;
  for (int i=0; i<N(families); i++)
    if (N(search_font_styles (families[i], v)) != 0)
      r << families[i];
  return r;
}

/******************************************************************************
* Modifying font properties
******************************************************************************/

array<string>
patch_font (array<string> v, array<string> w, bool decode) {
  array<string> r= copy (v);
  for (int i=0; i<N(w); i++) {
    string s= w[i];
    if (decode) s= decode_feature (s);
    int j;
    for (j=1; j<N(r); j++)
      if (!same_kind (r[j], s) || (is_category (s) && r[j] != s));
      else if (r[j] == "proportional" && s == "typewriter");
      else if (r[j] == "mono" && s == "typewriter");
      else if (r[j] == "typewriter" && s == "proportional" && j >= N(v));
      else if (r[j] == "typewriter" && s == "mono" && j >= N(v));
      else {
        r[j]= s;
	break;
      }
    if (j == N(r)) r << s;
  }
  //cout << v << ", " << w << " -> " << r << "\n";
  return r;
}

/******************************************************************************
* Font substitution
******************************************************************************/

bool
match_properties (array<string> v, tree props) {
  for (int i=0; i<N(props); i++)
    if (is_atomic (props[i])) {
      bool found= false;
      for (int j=0; j<N(v); j++)
        if (v[j] == props[i]->label)
          found= true;
      if (!found) return false;
    }
  return true;
}

array<string>
remove_properties (array<string> v, tree props) {
  array<string> r;
  for (int j=0; j<N(v); j++) {
    bool found= false;
    for (int i=0; i<N(props); i++)
      if (is_atomic (props[i]))
        if (v[j] == props[i]->label)
          found= true;
    if (!found) r << v[j];
  }
  return r;
}

array<string>
add_properties (array<string> v, tree props) {
  array<string> r;
  for (int i=0; i<N(props); i++)
    if (is_atomic (props[i]))
      r << props[i]->label;
  r << v;
  return r;
}

array<string>
apply_substitutions (array<string> v) {
  if (N(v) <= 0) return v;
  tree t= font_database_substitutions (v[0]);
  for (int i=0; i<N(t); i++)
    if (match_properties (v, t[i][0])) {
      v= remove_properties (v, t[i][0]);
      v= add_properties (v, t[i][1]);
      return apply_substitutions (v);
    }
  return v;
}
