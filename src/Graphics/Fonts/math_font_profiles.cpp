
/******************************************************************************
* MODULE     : math_font_profiles.cpp
* DESCRIPTION: hand-written knowledge about named OpenType math fonts:
*              their text companions, where math letters come from, menus
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"

// A profile is a tuple of (key value ...) tuples, keyed by the family name
// of the math font as the font database names it. The profiles are defined
// in TeXmacs/progs/fonts/fonts-opentype.scm and pushed here at boot; see
// doc/opentype-math-fonts-survey.md for the meaning of the keys.
// function-local statics: these are filled during the Scheme boot, before
// which global statics of other units may not have been initialized
static hashmap<string,tree>&
profiles () {
  static hashmap<string,tree> t= hashmap<string,tree> (tree (TUPLE));
  return t;
}

static hashmap<string,string>&
text_to_math () {
  static hashmap<string,string> t ("");
  return t;
}

void
math_font_profile_set (string family, tree profile) {
  profiles () (family)= profile;
  // Several math fonts may name the same text companion (Asana Math is a
  // Palladio design and so is TeX Gyre Pagella Math). The reverse map keeps
  // the first claimant, so the order of the profiles in
  // TeXmacs/progs/fonts/fonts-opentype.scm decides which math font a text
  // family pulls in; put the canonical pairing first.
  string text= math_font_profile_attr (family, "text");
  if (text != "" && !text_to_math ()->contains (text))
    text_to_math () (text)= family;
}

tree
math_font_profile (string family) {
  if (profiles ()->contains (family)) return profiles () [family];
  return tree (TUPLE);
}

array<string>
math_font_profile_families () {
  array<string> r;
  iterator<string> it= iterate (profiles ());
  while (it->busy ()) r << it->next ();
  return r;
}

// Scheme strings arrive as quoted labels, Scheme symbols unquoted
static string
unquoted (string s) {
  if (N(s) >= 2 && s[0] == '"' && s[N(s)-1] == '"') return s (1, N(s)-1);
  return s;
}

string
math_font_profile_attr (string family, string key) {
  tree p= math_font_profile (family);
  for (int i= 0; i < N(p); i++)
    if (is_tuple (p[i]) && N(p[i]) >= 2 &&
        is_atomic (p[i][0]) && is_atomic (p[i][1]) &&
        unquoted (p[i][0]->label) == key)
      return unquoted (p[i][1]->label);
  return "";
}

// A profile names its text companion by the master, the way the font
// environment variable names a font, but a document may well name one of
// the families of that master: the document font of kpfonts is KpRoman as
// often as Kepler, and Fira Sans as often as Fira. Both are the same design
// and want the same mathematics.
string
math_family_for_text (string text_family) {
  if (text_to_math ()->contains (text_family))
    return text_to_math () [text_family];
  string master= font_database_master (text_family);
  if (master == "" || master == text_family) return "";
  if (text_to_math ()->contains (master)) return text_to_math () [master];
  return "";
}

string
text_family_for_math (string math_family) {
  return math_font_profile_attr (math_family, "text");
}
