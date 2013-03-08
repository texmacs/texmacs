
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
#include "analyze.hpp"

extern hashmap<tree,tree> font_features;
extern hashmap<tree,tree> font_variants;

/******************************************************************************
* Decoding and encoding of features
******************************************************************************/

string
decode_feature (string s) {
  s= replace (locase_all (s) , " ", "");
  if (s == "unstretched") s= "unextended";
  if (s == "smallcapitals") s= "smallcaps";
  if (s == "monospaced") s= "mono";
  return s;
}

string
encode_feature (string s) {
  s= upcase_first (s);
  if (s == "Smallcaps") s= "SmallCaps";
  else if (ends (s, "bold"))
    s= s (0, N(s)-4) * upcase_first (s (N(s)-4, N(s)));
  else if (ends (s, "condensed"))
    s= s (0, N(s)-9) * upcase_first (s (N(s)-9, N(s)));
  return s;
}

/******************************************************************************
* Standardization of font features
******************************************************************************/

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
        r[i]= locase_all (r[i]);
      return r;
    }
  }
  if (occurs ("Mono", f) ||
      occurs ("Console", f) ||
      occurs ("Typewriter", f))
    r << string ("Mono");
  if (occurs ("Script", f))
    r << string ("Script");    
  if (occurs ("Sans", f))
    r << string ("SansSerif");
  if (occurs ("DemiCondensed", f) ||
      occurs ("Demi Condensed", f))
    r << string ("DemiCondensed");
  else if (occurs ("Condensed", f) ||
           occurs ("Narrow", f))
    r << string ("Condensed");
  for (int i=0; i<N(r); i++)
    r[i]= locase_all (r[i]);
  return r;
}

array<string>
style_features (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if ((s[i] >= 'A' && s[i] <= 'Z') &&
        (i+1 < N(s)) &&
        (s[i+1] >= 'a' && s[i+1] <= 'z'))
      r << ' ' << s[i];
    else r << s[i];
  array<string> v= tokenize (r, " ");
  if (N(v) == 0) return array<string> ();
  r= "";
  for (int i=0; i<N(v); i++) {
    if (i+1 < N(v) &&
        v[i] == "Regular" &&
        (v[i+1] == "Light" ||
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
    else if (v[i] == "Thin"); // Marker Felt
    else if (v[i] == "Upright");
    else if (v[i] == "Nonextended");
    else if (v[i] == "Unextended");
    else if (v[i] == "Wide") r << "Bold"; // Marker Felt
    else if (v[i] == "Slanted") r << "Oblique";
    else if (v[i] == "Inclined") r << "Italic";
    else r << v[i];
  }
  v= tokenize (r, " ");
  array<string> w;
  for (int i=0; i<N(v); i++)
    if (v[i] != "")
      w << locase_all (v[i]);
  return w;
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
  return r;
}

array<string>
logical_font_exact (string family, string style) {
  array<string> r;
  r << family_to_master (family);
  r << family_features (family);
  r << style_features (style);
  return r;
}

/******************************************************************************
* Master font families
******************************************************************************/

string
family_to_master (string f) {
  font_database_load ();
  if (font_features->contains (tree (f))) {
    tree t= font_features [tree (f)];
    if (is_func (t, TUPLE) && N(t) >= 1 && is_atomic (t[0]))
      return t[0]->label;
  }
  f= replace (f, " Mono", "");
  f= replace (f, "Mono", "");
  f= replace (f, " Console", "");
  f= replace (f, "Console", "");
  f= replace (f, " Typewriter", "");
  f= replace (f, "Typewriter", "");
  f= replace (f, " Script", "");
  f= replace (f, "Script", "");
  f= replace (f, " Sans", "");
  f= replace (f, "Sans", "");
  f= replace (f, " Serif", "");
  f= replace (f, "Serif", "");
  f= replace (f, " Demi", "");
  f= replace (f, "Demi", "");
  f= replace (f, " Condensed", "");
  f= replace (f, "Condensed", "");
  f= replace (f, " Narrow", "");
  f= replace (f, "Narrow", "");
  f= replace (f, " Light", "");
  f= replace (f, "Light", "");
  f= replace (f, " Medium", "");
  f= replace (f, "Medium", "");
  f= replace (f, " Bold", "");
  f= replace (f, "Bold", "");
  f= replace (f, " Black", "");
  f= replace (f, "Black", "");
  return f;
}

array<string>
master_to_families (string m) {
  font_database_load ();
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
exclude (array<string> v1, array<string> v2) {
  array<string> r;
  for (int i=0; i<N(v1); i++) {
    bool ok= true;
    for (int j=0; j<N(v2); j++)
      if (v1[i] == v2[j]) ok= false;
    if (ok) r << v1[i];
  }
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
    ends (s, "extended");
}

bool
is_weight (string s) {
  return 
    ends (s, "light") ||
    s == "regular" ||
    s == "medium" ||
    ends (s, "bold") ||
    ends (s, "black");
}

bool
is_slant (string s) {
  return 
    s == "upright" ||
    s == "italic" ||
    s == "oblique";
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
is_mono (string s) {
  return 
    s == "typewriter" ||
    s == "mono" ||
    s == "proportional";
}

bool
is_device (string s) {
  return 
    s == "printer" ||
    s == "typewriter" ||
    s == "script" ||
    s == "chalk" ||
    s == "marker";
}

/******************************************************************************
* Computing the distance between two fonts
******************************************************************************/

#define DISTANCE_FAR       1000000
#define DISTANCE_INFINITY  100000000

int
distance (string s1, string s2) {
  // NOTE: distances can be asymmetric.
  // For instance, 'bold' matches 'black' with distance 300,
  // but 'black' does not match 'bold'.
  if (s1 == s2) return 0;
  if (is_stretch (s1) || is_stretch (s2)) {
    if (!is_stretch (s1) || !is_stretch (s2)) return DISTANCE_FAR;
    if (ends (s1, "condensed") && ends (s2, "condensed")) return 10;
    if (ends (s1, "unextended") && !ends (s2, "unextended")) return 30;
    if (!ends (s1, "unextended") && ends (s2, "unextended")) return 30;
    if (ends (s1, "extended") && ends (s2, "extended")) return 10;
    return 30;
  }
  if (is_weight (s1) || is_weight (s2)) {
    if (!is_weight (s1) || !is_weight (s2)) return DISTANCE_FAR;
    if (ends (s1, "light") && ends (s2, "light")) return 100;
    if (ends (s1, "bold") && ends (s2, "bold")) return 100;
    if (ends (s1, "black") && ends (s2, "black")) return 100;
    if (ends (s1, "bold") && ends (s2, "black")) return 300;
    return 1000;
  }
  if (is_slant (s1) || is_slant (s2)) {
    if (!is_slant (s1) || !is_slant (s2)) return DISTANCE_FAR;
    if (s1 == "italic" && s2 == "oblique") return 100;
    if (s1 == "oblique" && s2 == "italic") return 100;
    return 1000;
  }
  if (is_capitalization (s1) || is_capitalization (s2)) {
    if (!is_capitalization (s1) || !is_capitalization (s2)) return DISTANCE_FAR;
    return 3000;
  }
  if (is_serif (s1) || is_serif (s2)) {
    if (!is_serif (s1) || !is_serif (s2)) return DISTANCE_FAR;
    return 10000;
  }
  if (is_mono (s1) || is_mono (s2)) {
    if (!is_mono (s1) || !is_mono (s2)) return DISTANCE_FAR;
    if (s1 == "mono" && s2 == "typewriter") return 0;
    if (s1 == "typewriter" && s2 == "mono") return 0;
    return 10000;
  }
  if (is_device (s1) || is_device (s2)) {
    if (!is_device (s1) || !is_device (s2)) return DISTANCE_FAR;
    if (s1 == "script" && s2 == "marker") return 10000;
    if (s1 == "script" && s2 == "chalk") return 10000;
    return 30000;
  }
  return DISTANCE_FAR;
}

bool
contains (array<string> a, bool (*pred) (string)) {
  for (int i=0; i<N(a); i++)
    if (pred (a[i])) return true;
  return false;
}

int
distance (string s, array<string> v) {
  if (s == "unextended" && !contains (v, is_stretch)) return 0;
  if (s == "medium" && !contains (v, is_weight)) return 0;
  if (s == "normal" && !contains (v, is_slant)) return 0;
  if (s == "mixed" && !contains (v, is_capitalization)) return 0;
  if (s == "serif" && !contains (v, is_serif)) return 0;
  if (s == "proportional" && !contains (v, is_mono)) return 0;
  if (s == "printed" && !contains (v, is_device)) return 0;

  if (s == "mono" && contains (string ("proportional"), v)) return 10000;
  if (s == "proportional" && contains (string ("mono"), v)) return 10000;

  int m= DISTANCE_FAR;
  if (is_stretch (s)) m= 30;
  else if (is_weight (s)) m= 1000;
  else if (is_slant (s)) m= 1000;
  else if (is_capitalization (s)) m= 3000;
  else if (is_serif (s)) m= 10000;
  else if (is_mono (s)) m= 10000;
  else if (is_device (s)) m= 30000;

  for (int i=1; i<N(v); i++)
    m= min (distance (s, v[i]), m);
  return m;
}

int
distance (array<string> v1, array<string> v2, array<string> v3) {
  // NOTE: v1 typically contains required properties of a font,
  // v3 the full set of properties of the candidate font and
  // v2 a subset of v3 of those properties which are not common
  // between all styles in the same family.
  int d= 0;
  if (N(v1) == 0 || N(v2) == 0) return DISTANCE_FAR;
  if (v1[0] != v2[0]) d= 100000;
  for (int i=1; i<N(v1); i++)
    d += distance (v1[i], v3);
  for (int i=1; i<N(v2); i++)
    d += distance (v2[i], v1);
  return d;
}

/******************************************************************************
* Compute a best possible approximation for font
******************************************************************************/

array<string>
search_font (array<string> v, bool require_exact) {
  if (N(v) == 0)
    return array<string> (string ("TeXmacs Computer Modern"),
                          string ("Unknown"));
  int best_distance= DISTANCE_INFINITY;
  array<string> best_result (v[0], string ("Unknown"));
  array<string> fams= master_to_families (v[0]);
  //cout << "Searching " << v << "\n";
  for (int i=0; i<N(fams); i++) {
    array<string> stys= font_database_styles (fams[i]);
    for (int j=0; j<N(stys); j++) {
      array<string> w= logical_font (fams[i], stys[j]);
      array<string> x= logical_font_exact (fams[i], stys[j]);
      int d= distance (v, w, x);
      //cout << "  " << w << ", " << x << " -> " << d << "\n";
      if (d < best_distance) {
        best_distance= d;
        best_result= array<string> (fams[i], stys[j]);
      }
    }
  }
  if (best_distance > 0 && require_exact) {
    string s;
    for (int i=1; i<N(v); i++) {
      if (i>1) s << " ";
      s << encode_feature (v[i]);
    }
    best_result[1]= s;
    //best_result[1]= string ("Unknown");
  }
  //cout << "Found " << best_result << "\n";
  return best_result;
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
      int d= distance (v[j], w);
      //cout << "Test " << v  << ", " << w << " -> "
      //     << d << ", " << distance (v[j], empty) << "\n";
      if (d != 0 && d >= distance (v[j], empty)) break;
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
  for (int i=1; i<N(v); i++)
    if (v[i] == "mono" || v[i] == "typewriter")
      r << string ("tt");
  for (int i=1; i<N(v); i++)
    if (v[i] == "sansserif")
      r << string ("ss");
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
        ends (v[i], "extended") ||
        v[i] == "proportional")
      r << v[i];
  for (int i=1; i<N(v); i++)
    if (v[i] == "upright") r << string ("right");
    else if (v[i] == "italic") r << string ("italic");
    else if (v[i] == "oblique") r << string ("slanted");
  for (int i=1; i<N(v); i++)
    if (v[i] == "smallcaps") r << string ("small-caps");
    else if (v[i] == "long") r << string ("long");
    else if (v[i] == "flat") r << string ("flat");
  if (N(r) == 0) return "right";
  return recompose (r, "-");
}

/******************************************************************************
* Translation from internal naming scheme
******************************************************************************/

array<string>
variant_features (string s) {
  array<string> v= tokenize (s, "-");
  array<string> r;
  for (int i=0; i<N(v); i++)
    if (v[i] == "ss") r << string ("sansserif");
    else if (v[i] == "tt") r << string ("typewriter");
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
        ends (v[i], "extended") ||
        v[i] == "proportional" ||
        v[i] == "italic" ||
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
  r << family;
  r << variant_features (variant);
  r << series_features (series);
  r << shape_features (shape);
  array<string> v;
  for (int i=0; i<N(r); i++)
    if (r[i] != "medium" &&
        r[i] != "upright")
      v << r[i];
  return v;
}
