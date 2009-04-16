
/******************************************************************************
* MODULE     : compound_font.cpp
* DESCRIPTION: fonts which are agglomerated from several other fonts.
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CHARMAP_H
#define CHARMAP_H
#include "resource.hpp"

RESOURCE(charmap);

/******************************************************************************
* The charmap structure for finding the physical font
******************************************************************************/

struct charmap_rep: rep<charmap> {
  int                    fast_map[256];
  hashmap<string,int>    slow_map;
  hashmap<string,string> slow_subst;

public:
  charmap_rep (string name);

  inline virtual int arity () { return 1; }
  inline virtual charmap child (int i) {
    ASSERT (i == 0, "bad child");
    return this; }
  inline virtual void lookup (string s, int& ch, string& r) { ch= 0; r= s; }
  inline void cached_lookup (string s, int& ch, string& r) {
    if (!slow_map->contains (s)) lookup (s, slow_map (s), slow_subst (s));
    ch= slow_map   (s);
    r = slow_subst (s);
  }

  void advance (string s, int& pos, string& r, int& ch);
};

charmap load_charmap (tree def);

#endif // defined CHARMAP_H
