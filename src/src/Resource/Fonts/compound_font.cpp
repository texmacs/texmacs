
/******************************************************************************
* MODULE     : compound_font.cpp
* DESCRIPTION: fonts which are agglomerated from several other fonts.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
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
* The compound encoding structure
******************************************************************************/

RESOURCE(compound_encoding);

struct compound_encoding_rep: rep<compound_encoding> {
  int                    fast_map[256];
  hashmap<string,int>    char_map;
  hashmap<string,string> char_subst;
  compound_encoding_rep (string name, tree def);
};

RESOURCE_CODE(compound_encoding);

compound_encoding_rep::compound_encoding_rep (string name, tree def):
  rep<compound_encoding> (name), char_map (-1), char_subst ("x")
{
  int i;
  for (i=0; i<256; i++)
    fast_map[i]= -1;

  int nr, tot= N (def);
  for (nr=tot-1; nr>=0; nr--) {
    string s, fname= as_string (def[nr]) * ".scm";
    if (DEBUG_AUTO) cout << "TeXmacs] Loading " << fname << "\n";
    if (load_string (url ("$TEXMACS_PATH/fonts/enc", fname), s)) return;
    tree t= block_to_scheme_tree (s);
    if (!is_tuple (t)) return;
    
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_func (t[i], TUPLE, 2) &&
	  is_atomic (t[i][0]) && is_atomic (t[i][1]))
	{
	  string l= unquote (t[i][0]->label);
	  string r= unquote (t[i][1]->label);
	  if ((l == r) && (N(l) == 1))
	    fast_map[(unsigned char) l[0]]= nr;
	  else {
	    char_map   (l)= nr;
	    char_subst (l)= r;
	  }
	}
  }
}

static tree
map_car (tree t) {
  int i, n= N(t);
  tree r (TUPLE, n);
  for (i=0; i<n; i++)
    r[i]= t[i][0];
  return r;
}

compound_encoding
load_compound_encoding (tree def) {
  string name= tree_to_scheme (def);
  if (compound_encoding::instances -> contains (name))
    return compound_encoding (name);
  return make (compound_encoding, name,
	       new compound_encoding_rep (name, def));
}

/******************************************************************************
* The compound font class
******************************************************************************/

struct compound_font_rep: font_rep {
  scheme_tree       def;
  array<font>       fn;
  compound_encoding enc;

  compound_font_rep (string name, scheme_tree def, array<font> fn);
  void advance (string s, int& i, string& r, int& nr);
  void get_extents (string s, metric& ex);
  void draw (ps_device dev, string s, SI x, SI y);
  glyph get_glyph (string s);
};

compound_font_rep::compound_font_rep (
  string name, scheme_tree def2, array<font> fn2):
    font_rep (fn2[0]->dis, name, fn2[0]),
    def (def2), fn (fn2), enc (load_compound_encoding (map_car (def)))
{
}

void
compound_font_rep::advance (string s, int& i, string& r, int& nr) {
  // advance as much as possible while remaining in same subfont
  // return the corresponding transcoded substring in r and
  // the index of the subfont in nr. The string r is assumed to
  // be initialized with s for speeding things up when r==s at exit.
  // The fast_map tells which 1-byte characters are mapped to themselves.

  int n= N(s);
  nr= enc->fast_map[(unsigned char) s[i]];
  if (nr >= 0) {
    int start= i++;
    while ((i<n) && (enc->fast_map[(unsigned char) s[i]] == nr)) i++;
    if ((start>0) || (i<n)) r= r (start, i);
  }
  else {
    int start= i;
    skip_symbol (s, i);
    string ss= s (start, i);
    nr= enc->char_map [ss];
    r = copy (enc->char_subst (ss));
    while (i < n) {
      if (enc->fast_map[(unsigned char) s[i]] >= 0) break;
      start= i;
      skip_symbol (s, i);
      ss= s (start, i);
      int nr2= enc->char_map [ss];
      if (nr2 != nr) {
	i= start;
	break;
      }
      r << enc->char_subst (ss);
    }
  }
  if ((nr>0) && (nil (fn[nr])))
    fn[nr]= find_font (fn[0]->dis, def[nr][1]);
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

glyph
compound_font_rep::get_glyph (string s) {
  int i=0, n= N(s);
  if (n == 0) return fn[0]->get_glyph (s);
  int nr;
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
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
  return make (font, name, new compound_font_rep (name, def, fn));
}
