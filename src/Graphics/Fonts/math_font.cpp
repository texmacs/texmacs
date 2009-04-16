
/******************************************************************************
* MODULE     : math_font.cpp
* DESCRIPTION: the font used for mathematical typesetting is actually a list
*              of several fonts, which may be specified by the user.
*              If a symbol needs to printed, we search in the list
*              for a font which is capable of printing it.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "translator.hpp"
#include "convert.hpp"

typedef int SI;
bool operator == (font fn1, font fn2) { return fn1.rep == fn2.rep; }
bool operator != (font fn1, font fn2) { return fn1.rep == fn2.rep; }
font find_font (tree t);

/******************************************************************************
* The compound font class
******************************************************************************/

struct math_font_rep: font_rep {
  font         base_fn;
  font         error_fn;
  translator   math;
  translator   rubber;
  translator   italic;
  array<tree>  font_name;
  array<font>  font_table;
  array<tree>  rubber_name;
  array<font>  rubber_table;

  math_font_rep (string name, scheme_tree t, font base, font error);
  void init_font (int fn_nr, font& fn);
  void search_font (string& s, font& fn);
  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
  glyph get_glyph (string s);

  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

math_font_rep::math_font_rep (
  string name, scheme_tree t, font base, font error):
    font_rep (name, base), base_fn (base), error_fn (error),
    math (load_translator (as_string (t[1][0]))),
    rubber (load_translator (as_string (t[2][0]))),
    italic (load_translator ("italic")),
    font_name (N(t[1])-1), font_table (N(t[1])-1),
    rubber_name (N(t[2])-1), rubber_table (N(t[2])-1)
{
  int i;
  for (i=1; i<N(t[1]); i++)
    font_name[i-1]= t[1][i];
  for (i=1; i<N(t[2]); i++)
    rubber_name[i-1]= t[2][i];
}

/******************************************************************************
* Find the font and the corresponding character
******************************************************************************/

void
math_font_rep::init_font (int fn_nr, font& fn) {
  tree t= font_name [fn_nr];
  if (is_tuple (t, "virtual", 3))
    fn= virtual_font (this, as_string(t[1]), as_int(t[2]), as_int(t[3]));
  else {
    fn= find_font (t);
    ASSERT (!is_nil (fn), "font not found");
  }
  fn->copy_math_pars (base_fn);
  font_table [fn_nr]= fn;
}

void
math_font_rep::search_font (string& s, font& fn) {
  if ((N(s)>=9) && (s[N(s)-2]>='0') && (s[N(s)-2]<='9')) {
    int i;
    for (i=N(s)-1; i>0; i--)
      if (s[i]=='-') break;
    if (i>0) {
      string root= s(0,i) * ">";
      int c= rubber->dict [root];
      if ((c!=-1) && (s (N(s)-3, N(s)) != "-0>")) {
	int fn_nr= c/256;
	if (is_nil (rubber_table [fn_nr])) {
	  fn= find_font (rubber_name [fn_nr]);
	  ASSERT (!is_nil (fn), "font not found");
	  fn->yfrac= base_fn->yfrac;
	  // fn->copy_math_pars (base_fn);
	  rubber_table [fn_nr]= fn;
	}
	else fn= rubber_table [fn_nr];
	return;
      }
      c= math->dict [s(0,i) * "-#>"];
      if (c != -1) {
	int fn_nr= c/256;
	fn= font_table [fn_nr];
	if (is_nil (fn)) init_font (fn_nr, fn);
	s= string ((char) (c&255)) * s (i+1, N(s));
	return;
      }
    }
  }

  int c= math->dict [s];
  if (c != -1) { 
    int fn_nr= c/256;
    fn= font_table [fn_nr];
    if (is_nil (fn)) init_font (fn_nr, fn);
    s= string ((char) (c&255));
  }
  else {
    int i, n= N(s);
    for (i=0; i<n; i++)
      if (((s[i]<'0') || (s[i]>'9')) &&
	  ((s[i]<'a') || (s[i]>'z')) &&
	  ((s[i]<'A') || (s[i]>'Z')) &&
	  (s[i] != '.') && (s[i] != '\\') && (s[i] != '_'))
	{
	  fn= error_fn;
	  return;
	}
    fn= base_fn;
  }
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

void
math_font_rep::get_extents (string s, metric& ex) {
  font fn;
  search_font (s, fn);
  fn->get_extents (s, ex);
}

void
math_font_rep::get_xpositions (string s, SI* xpos) {
  if (s == "") return;
  font fn;
  string r= s;
  search_font (r, fn);
  if (r == s) fn->get_xpositions (s, xpos);
  else if (N(r) != 1) font_rep::get_xpositions (s, xpos);
  else {
    int i, n=N(s);
    for (i=1; i<n; i++) xpos[i]= 0;
    fn->get_xpositions (r, xpos+n-1);
  }
}

void
math_font_rep::draw (renderer ren, string s, SI x, SI y) {
  font fn;
  search_font (s, fn);
  fn->draw (ren, s, x, y);
}

glyph
math_font_rep::get_glyph (string s) {
  font fn;
  search_font (s, fn);
  return fn->get_glyph (s);
}

/******************************************************************************
* Metric properties
******************************************************************************/

double
math_font_rep::get_left_slope  (string s) {
  if (italic->dict->contains (s) || ((N(s) >= 2) && (s[0] != '<'))) {
    font fn;
    search_font (s, fn);
    return fn->get_left_slope (s);
  }
  else return 0;
}

double
math_font_rep::get_right_slope (string s) {
  if (italic->dict->contains (s) || ((N(s) >= 2) && (s[0] != '<'))) {
    font fn;
    search_font (s, fn);
    return fn->get_right_slope (s);
  }
  else return 0;
}

SI
math_font_rep::get_left_correction  (string s) {
  font fn;
  search_font (s, fn);
  return fn->get_left_correction (s);
}

SI
math_font_rep::get_right_correction (string s) {
  font fn;
  search_font (s, fn);
  return fn->get_right_correction (s);
}

/******************************************************************************
* User interface
******************************************************************************/

font
math_font (scheme_tree t, font base_fn, font error_fn) {
  string full_name= "compound-" * scheme_tree_to_string (t);
  return make (font, full_name,
    tm_new<math_font_rep> (full_name, t, base_fn, error_fn));
}
