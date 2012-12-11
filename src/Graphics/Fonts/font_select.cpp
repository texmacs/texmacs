
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

/******************************************************************************
* Standardization of font features
******************************************************************************/

string
master_family (string f) {
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
family_features (string f) {
  array<string> r;
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
        v[i-1] == "Ultra");
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
  for (int i=0; i<N(v); i++)
    v[i]= locase_all (v[i]);
  return v;
}

/******************************************************************************
* Translation with respect to internal naming scheme
******************************************************************************/

string
get_variant (array<string> v) {
  array<string> r;
  for (int i=0; i<N(v); i++)
    if (v[i] == "mono" || v[i] == "typewriter")
      r << string ("tt");
  for (int i=0; i<N(v); i++)
    if (v[i] == "sansserif")
      r << string ("ss");
  if (N(r) == 0) return "rm";
  return recompose (r, "-");
}

string
get_series (array<string> v) {
  for (int i=0; i<N(v); i++)
    if (ends (v[i], "light") ||
        v[i] == "regular" ||
        v[i] == "medium" ||
        ends (v[i], "bold") ||
        ends (v[i], "black"))
      return v[i];
  return "medium";
}

string
get_shape (array<string> v) {
  array<string> r;
  for (int i=0; i<N(v); i++)
    if (ends (v[i], "condensed") ||
        ends (v[i], "extended"))
      r << v[i];
  for (int i=0; i<N(v); i++)
    if (v[i] == "upright") r << string ("right");
    else if (v[i] == "italic") r << string ("italic");
    else if (v[i] == "oblique") r << string ("slanted");
  for (int i=0; i<N(v); i++)
    if (v[i] == "smallcaps") r << string ("small-caps");
    else if (v[i] == "long") r << string ("long");
    else if (v[i] == "flat") r << string ("flat");
  if (N(r) == 0) return "right";
  return recompose (r, "-");
}

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
        v[i] == "italic" ||
        v[i] == "smallcaps" ||
        v[i] == "long" ||
        v[i] == "flat")
      r << v[i];
    else if (v[i] == "right") r << string ("upright");
    else if (v[i] == "slanted") r << string ("oblique");
  return r;
}
