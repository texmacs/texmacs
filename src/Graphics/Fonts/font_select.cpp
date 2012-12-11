
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

array<string>
font_features (string s) {
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
