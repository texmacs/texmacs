
/******************************************************************************
* MODULE     : charmap.cpp
* DESCRIPTION: character maps for agglomerated fonts
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "charmap.hpp"
#include "translator.hpp"
#include "analyze.hpp"

RESOURCE_CODE(charmap);

/******************************************************************************
* The charmap structure for finding the physical font
******************************************************************************/

charmap_rep::charmap_rep (string name):
  rep<charmap> (name), slow_map (-1), slow_subst ("")
{
  int ch; string r;
  for (int i=0; i<256; i++)
    if (((char) i) == '<' || ((char) i) == '>') fast_map[i]= -1;
    else {
      string s ((unsigned char) i);
      lookup (s, ch, r);
      fast_map[i]= (r == s? ch: -1);
    }
}

void
charmap_rep::advance (string s, int& pos, string& r, int& ch) {
  int n= N(s), start= pos;
  if (s[pos] != '<') {
    ch= fast_map [(unsigned char) s[pos++]];
    while (pos<n && s[pos] != '<' &&
	   fast_map [(unsigned char) s[pos]] == ch) pos++;
    r= s (start, pos);
    if (pos == n || s[pos] != '<') return;
  }
  else {
    int start= pos;
    tm_char_forwards (s, pos);
    cached_lookup (s (start, pos), ch, r);
  }
  int ch2; string r2;
  while (pos<n) {
    int start= pos;
    tm_char_forwards (s, pos);
    cached_lookup (s (start, pos), ch2, r2);
    if (ch2 != ch) { pos= start; break; }
    else r << r2;
  }
}

charmap
any_charmap () {
  if (charmap::instances -> contains ("any"))
    return charmap ("any");
  return make (charmap, "any", tm_new<charmap_rep> ("any"));
}

/******************************************************************************
* Explicit charmaps
******************************************************************************/

struct ec_charmap_rep: public charmap_rep {
  ec_charmap_rep (): charmap_rep ("ec") {}
  void lookup (string s, int& ch, string& r) {
    if (N(s) == 1 || s == "<less>" || s == "<gtr>") { ch= 0; r= s; }
    else { ch= -1; r= ""; }
  }
};

charmap
ec_charmap () {
  if (charmap::instances -> contains ("ec"))
    return charmap ("ec");
  return make (charmap, "ec", tm_new<ec_charmap_rep> ());
}

struct range_charmap_rep: public charmap_rep {
  int start, end;
  range_charmap_rep (int start2, int end2):
    charmap_rep (as_hexadecimal (start2) * "--" * as_hexadecimal (end2)),
    start (start2), end (end2) {}
  inline bool between (int what, int begin, int end) {
    return what >= begin && what <= end;
  }
  void lookup (string s, int& ch, string& r) {
    if (N(s) >= 3 && s[0] == '<' && s[1] == '#' && s[N(s)-1] == '>' &&
	between (from_hexadecimal (s (2, N(s)-1)), start, end)) { ch=0; r=s; }
    else { ch= -1; r= ""; }
  }
};

charmap
range_charmap (int start, int end) {
  string name= as_hexadecimal (start) * "--" * as_hexadecimal (end);
  if (charmap::instances -> contains (name)) return charmap (name);
  return make (charmap, name, tm_new<range_charmap_rep> (start, end));
}

charmap
la_charmap () {
  return range_charmap (0x400, 0x4ff);
}

charmap
hangul_charmap () {
  return range_charmap (0xac00, 0xd7a3);
}

charmap
oriental_charmap () {
  return range_charmap (0x3000, 0xffff);
}

struct explicit_charmap_rep: public charmap_rep {
  translator trl;
  explicit_charmap_rep (string name):
    charmap_rep (name), trl (load_translator (name)) {}
  void lookup (string s, int& ch, string& r) {
    if (trl->dict->contains (s)) { ch= 0; r= string ((char) trl->dict[s]); }
    else { ch= -1; r= ""; }
  }
};

charmap
explicit_charmap (string name) {
  if (charmap::instances -> contains (name))
    return charmap (name);
  return make (charmap, name, tm_new<explicit_charmap_rep> (name));
}

/******************************************************************************
* Joined charmaps
******************************************************************************/
 
string
join_name (charmap* a, int n) {
  string acc= copy (a[0]->res_name);
  for (int i=1; i<n; i++)
    acc << ":" << a[i]->res_name;
  return acc;
}

struct join_charmap_rep: public charmap_rep {
  charmap* ja;
  int      jn;
  join_charmap_rep (charmap* a, int n):
    charmap_rep (join_name (a, n)), ja (a), jn (n) {}
  int arity () {
    int i, sum= 0;
    for (i=0; i<jn; i++)
      sum += ja[i] -> arity ();
    return sum;
  }
  charmap child (int ch) {
    int i, sum= 0;
    for (i=0; i<jn; i++) {
      int p= ja[i] -> arity ();
      if (ch >= sum && ch < sum+p) return ja[i] -> child (i-sum);
      sum += p;
    }
    FAILED ("bad child");
    return this;
  }
  void lookup (string s, int& ch, string& r) {
    int i, sum= 0;
    for (i=0; i<jn; i++) {
      ja[i]->lookup (s, ch, r);
      //cout << i << "\t" << s << " -> " << ch << ", " << r << "\n";
      if (ch >= 0) {
	ch += sum;
	return;
      }
      sum += ja[i] -> arity ();
    }
    ch= -1; r= "";
  }  
};

charmap
join_charmap (charmap* a, int n) {
  string name= join_name (a, n);
  if (charmap::instances -> contains (name))
    return charmap (name);
  return make (charmap, name, tm_new<join_charmap_rep> (a, n));
}

/******************************************************************************
* High level interface
******************************************************************************/

charmap
load_charmap (tree def) {
  int i, n= N (def);
  charmap* a= tm_new_array<charmap> (n);
  for (i=0; i<n; i++) {
    //cout << i << "\t" << def[i] << "\n";
    if (def[i] == "any") a[i]= any_charmap ();
    else if (def[i] == "ec") a[i]= ec_charmap ();
    else if (def[i] == "hangul") a[i]= hangul_charmap ();
    else if (def[i] == "la") a[i]= la_charmap ();
    else if (def[i] == "oriental") a[i]= oriental_charmap ();
    else a[i]= explicit_charmap (as_string (def[i]));
  }
  return join_charmap (a, n);
}
