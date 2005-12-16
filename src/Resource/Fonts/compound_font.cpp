
/******************************************************************************
* MODULE     : compound_font.cpp
* DESCRIPTION: fonts which are agglomerated from several other fonts.
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"
#include "translator.hpp"
#include "analyze.hpp"
#include "convert.hpp"
#include "file.hpp"

/******************************************************************************
* The charmap structure for finding the physical font
******************************************************************************/

RESOURCE(charmap);

struct charmap_rep: rep<charmap> {
  int                    fast_map[256];
  hashmap<string,int>    slow_map;
  hashmap<string,string> slow_subst;
  charmap_rep (string name):
    rep<charmap> (name), slow_map (-1), slow_subst ("")
  {
    for (int i=0; i<256; i++) fast_map[i]= -1;
  }
  virtual int arity () { return 1; }
  virtual charmap child (int i) {
    if (i != 0) fatal_error ("bad child", "charmap_rep::child");
    return this; }
  virtual int which (string s) { return 0; }
  virtual string replace (string s) { return s; }

  inline int fast_lookup (int c) {
    register int& r= fast_map[c];
    if (r < 0) r= which (string ((char) c));
    //cout << "lookup " << c << " in " << res_name << " -> " << r << "\n";
    return r;
  }
  inline int slow_lookup (string s) {
    register int& r= slow_map (s);
    if (r < 0) r= which (s);
    //cout << "lookup " << s << " in " << res_name << " -> " << r << "\n";
    return r;
  }
  inline string slow_replace (string s) {
    register string& r= slow_subst (s);
    if (N(r) == 0) r= replace (s);
    //cout << "lookup " << s << " in " << res_name << " -> " << r << "\n";
    return r;
  }
  void advance (string s, int& pos, string& r, int& ch) {
    int n= N(s), start= pos;
    if (s[pos] != '<') {
      ch= fast_lookup ((unsigned char) s[pos++]);
      while (pos<n && s[pos] != '<' &&
	     fast_lookup ((unsigned char) s[pos]) == ch) pos++;
      r= s (start, pos);
      if (pos == n || s[pos] != '<') return;
    }
    else {
      int start= pos;
      tm_char_forwards (s, pos);
      ch= slow_lookup (s (start, pos));
      r = slow_replace (s (start, pos));
    }
    while (pos<n) {
      int start= pos;
      tm_char_forwards (s, pos);
      if (slow_lookup (s (start, pos)) != ch) {
	pos= start;
	break;
      }
      else r << slow_replace (s (start, pos));
    }
  }
};

RESOURCE_CODE(charmap);

charmap
any_charmap () {
  if (charmap::instances -> contains ("any"))
    return charmap ("any");
  return make (charmap, "any", new charmap_rep ("any"));
}

struct ec_charmap_rep: public charmap_rep {
  ec_charmap_rep (): charmap_rep ("ec") {}
  int which (string s) {
    if (N(s) == 1 || s == "<less>" || s == "<gtr>") return 0;
    else return -1;
  }
  string replace (string s) {
    if (N(s) == 1 || s == "<less>" || s == "<gtr>") return s;
    else return "";
  }
};

charmap
ec_charmap () {
  if (charmap::instances -> contains ("ec"))
    return charmap ("ec");
  return make (charmap, "ec", new ec_charmap_rep ());
}

static inline bool
between (int what, int begin, int end) {
  return what >= begin && what <= end;
}

struct range_charmap_rep: public charmap_rep {
  int start, end;
  range_charmap_rep (int start2, int end2):
    charmap_rep (as_hexadecimal (start2) * "--" * as_hexadecimal (end2)),
    start (start2), end (end2) {}
  int which (string s) {
    if (N(s) >= 3 && s[0] == '<' && s[1] == '#' && s[N(s)-1] == '>' &&
	between (from_hexadecimal (s (2, N(s)-1)), start, end))
      return 0;
    else return -1;
  }
  string replace (string s) {
    if (N(s) >= 3 && s[0] == '<' && s[1] == '#' && s[N(s)-1] == '>' &&
	between (from_hexadecimal (s (2, N(s)-1)), start, end))
      return s;
    else return "";
  }
};

charmap
range_charmap (int start, int end) {
  string name= as_hexadecimal (start) * "--" * as_hexadecimal (end);
  if (charmap::instances -> contains (name)) return charmap (name);
  return make (charmap, name, new range_charmap_rep (start, end));
}

charmap
la_charmap () {
  return range_charmap (0x400, 0x4ff);
}

charmap
oriental_charmap () {
  return range_charmap (0x3000, 0xfaff);
}

struct explicit_charmap_rep: public charmap_rep {
  translator trl;
  explicit_charmap_rep (string name):
    charmap_rep (name), trl (load_translator (name)) {}
  int which (string s) { return trl->dict->contains (s)? 0: -1; }
  string replace (string s) {
    if (trl->dict->contains (s)) return string ((char) trl->dict[s]);
    else return "";
  }
};

charmap
explicit_charmap (string name) {
  if (charmap::instances -> contains (name))
    return charmap (name);
  return make (charmap, name, new explicit_charmap_rep (name));
}
 
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
    charmap_rep (join_name (a, n)), ja (a), jn (n)
  {
    int i, ch, nch= arity ();
    for (i=0; i<256; i++)
      for (ch=0; ch < nch; ch++)
	if (child (ch) -> fast_map[i] != -1) {
	  fast_map[i]= ch;
	  break;
	}
  }
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
    fatal_error ("bad child", "join_charmap_rep::child");
    return this;
  }
  int which (string s) {
    int i, sum= 0;
    for (i=0; i<jn; i++) {
      int p= ja[i] -> arity ();
      int w= ja[i] -> which (s);
      if (w >= 0) return sum+w;
      sum += p;
    }
    return -1;
  }  
  string replace (string s) {
    int i;
    for (i=0; i<jn; i++) {
      string r= ja[i] -> replace (s);
      //cout << i << "\t" << s << " -> " << r << "\n";
      if (N(r) != 0) return r;
    }
    return "";
  }  
};

charmap
join_charmap (charmap* a, int n) {
  string name= join_name (a, n);
  if (charmap::instances -> contains (name))
    return charmap (name);
  return make (charmap, name, new join_charmap_rep (a, n));
}

charmap
load_charmap (tree def) {
  int i, n= N (def);
  charmap* a= new charmap [n];
  for (i=0; i<n; i++) {
    //cout << i << "\t" << def[i] << "\n";
    if (def[i] == "any") a[i]= any_charmap ();
    else if (def[i] == "ec") a[i]= ec_charmap ();
    else if (def[i] == "la") a[i]= la_charmap ();
    else if (def[i] == "oriental") a[i]= oriental_charmap ();
    else a[i]= explicit_charmap (as_string (def[i]));
  }
  return join_charmap (a, n);
}

/******************************************************************************
* The compound font class
******************************************************************************/

static tree
map_car (tree t) {
  int i, n= N(t);
  tree r (TUPLE, n);
  for (i=0; i<n; i++)
    r[i]= t[i][0];
  return r;
}

struct compound_font_rep: font_rep {
  scheme_tree  def;
  array<font>  fn;
  charmap      cm;

  compound_font_rep (string name, scheme_tree def, array<font> fn);
  void   advance (string s, int& pos, string& r, int& ch);
  void   get_extents (string s, metric& ex);
  void   draw (ps_device dev, string s, SI x, SI y);
  glyph  get_glyph (string s);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

compound_font_rep::compound_font_rep (
  string name, scheme_tree def2, array<font> fn2):
    font_rep (fn2[0]->dis, name, fn2[0]),
    def (def2), fn (fn2), cm (load_charmap (map_car (def)))
{}

void
compound_font_rep::advance (string s, int& pos, string& r, int& ch) {
  cm->advance (s, pos, r, ch);
  //cout << "(r,ch)= (" << r << "," << ch << ")\n";
  if (ch>0 && nil (fn[ch])) {
    tree t= def[ch][1];
    if (is_tuple (t, "virtual", 3))
      fn[ch]= virtual_font (this, as_string(t[1]), as_int(t[2]), as_int(t[3]));
    else fn[ch]= find_font (dis, t);
    if (nil (fn[ch]))
      fatal_error ("Font not found", "compound_font_rep::advance");
    //fn[ch]->copy_math_pars (fn[0]);
  }
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

void
compound_font_rep::get_extents (string s, metric& ex) {
  int i=0, n= N(s);
  fn[0]->get_extents (empty_string, ex);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->get_extents (r, ey);
      ex->y1= min (ex->y1, ey->y1);
      ex->y2= max (ex->y2, ey->y2);
      ex->x3= min (ex->x3, ex->x2 + ey->x3);
      ex->y3= min (ex->y3, ey->y3);
      ex->x4= max (ex->x4, ex->x2 + ey->x4);
      ex->y4= max (ex->y4, ey->y4);
      ex->x2 += ey->x2;
    }
  }
}

void
compound_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw (dev, r, x, y);
      if (i < n) {
	fn[nr]->get_extents (r, ey);
	x += ey->x2;
      }
    }
  }
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

glyph
compound_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
}

double
compound_font_rep::get_left_slope  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_slope (r);
}

double
compound_font_rep::get_right_slope (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_slope (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_slope (r);
}

SI
compound_font_rep::get_left_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_correction (r);
}

SI
compound_font_rep::get_right_correction (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_correction (r);
}

/******************************************************************************
* User interface
******************************************************************************/

font
compound_font (display dis, scheme_tree def) {
  string name= tree_to_scheme (def);
  if (font::instances->contains (name))
    return font (name);
  array<font> fn (N(def));
  fn[0]= find_font (dis, def[0][1]);
  if (nil (fn[0])) return font ();
  return make (font, name, new compound_font_rep (name, def, fn));
}
