
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
* Standardization of font features
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
subfamily_features (string s) {
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
  //     << ", " << subfamily_features (style) << "\n";
  r << family_to_master (family);
  r << family_features (family);
  r << subfamily_features (style);
  return r;
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

/******************************************************************************
* Computing the distance between two fonts
******************************************************************************/

int
distance (string s1, string s2) {
  if (s1 == s2) return 0;
  if (is_stretch (s1) || is_stretch (s2)) {
    if (!is_stretch (s1) || !is_stretch (s2)) return 1000000;
    if (ends (s1, "condensed") && ends (s2, "condensed")) return 10;
    if (ends (s1, "unextended") && !ends (s2, "unextended")) return 30;
    if (!ends (s1, "unextended") && ends (s2, "unextended")) return 30;
    if (ends (s1, "extended") && ends (s2, "extended")) return 10;
    return 30;
  }
  if (is_weight (s1) || is_weight (s2)) {
    if (!is_weight (s1) || !is_weight (s2)) return 1000000;
    if (ends (s1, "light") && ends (s2, "light")) return 100;
    if (ends (s1, "bold") && ends (s2, "bold")) return 100;
    if (ends (s1, "black") && ends (s2, "black")) return 100;
    if (ends (s1, "bold") && ends (s2, "black")) return 300;
    if (ends (s1, "black") && ends (s2, "bold")) return 300;
    return 1000;
  }
  if (is_slant (s1) || is_slant (s2)) {
    if (!is_slant (s1) || !is_slant (s2)) return 1000000;
    if (s1 == "italic" && s2 == "oblique") return 100;
    if (s1 == "oblique" && s2 == "italic") return 100;
    return 1000;
  }
  if (is_capitalization (s1) || is_capitalization (s2)) {
    if (!is_capitalization (s1) || !is_capitalization (s2)) return 1000000;
    return 3000;
  }
  if (is_serif (s1) || is_serif (s2)) {
    if (!is_serif (s1) || !is_serif (s2)) return 1000000;
    return 10000;
  }
  if (is_mono (s1) || is_mono (s2)) {
    if (!is_mono (s1) || !is_mono (s2)) return 1000000;
    if (s1 == "mono" && s2 == "typewriter") return 0;
    if (s1 == "typewriter" && s2 == "mono") return 0;
    return 10000;
  }
  return 1000000;
}

int
distance (string s, array<string> v) {
  int m= 1000000;
  if (is_stretch (s)) m= 30;
  else if (is_weight (s)) m= 1000;
  else if (is_slant (s)) m= 1000;
  else if (is_capitalization (s)) m= 3000;
  else if (is_serif (s)) m= 10000;
  else if (is_mono (s)) m= 10000;
  for (int i=1; i<N(v); i++)
    m= min (distance (s, v[i]), m);
  return m;
}

int
distance (array<string> v1, array<string> v2) {
  int d= 0;
  if (N(v1) == 0 || N(v2) == 0) return 1000000;
  if (v1[0] != v2[0]) d= 100000;
  for (int i=1; i<N(v1); i++)
    d += distance (v1[i], v2);
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
  int best_distance= 100000000;
  array<string> best_result (v[0], string ("Unknown"));
  array<string> fams= master_to_families (v[0]);
  //cout << "Searching " << v << "\n";
  for (int i=0; i<N(fams); i++) {
    array<string> stys= font_database_styles (fams[i]);
    for (int j=0; j<N(stys); j++) {
      array<string> w= logical_font (fams[i], stys[j]);
      int d= distance (v, w);
      //cout << "  " << w << " -> " << d << "\n";
      if (d < best_distance) {
        best_distance= d;
        best_result= array<string> (fams[i], stys[j]);
      }
    }
  }
  if (best_distance > 0 && require_exact)
    best_result[1]= string ("Unknown");
  return best_result;
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
