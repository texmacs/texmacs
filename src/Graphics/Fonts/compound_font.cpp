
/******************************************************************************
* MODULE     : compound_font.cpp
* DESCRIPTION: fonts which are agglomerated from several other fonts.
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "charmap.hpp"
#include "convert.hpp"

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
  double       hzf;
  double       vzf;

  compound_font_rep (string name, scheme_tree def, array<font> fn,
                     double hzf, double vzf);
  bool   supports (string c);
  void   advance (string s, int& pos, string& r, int& ch);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos, bool ligf);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_wide_correction  (string s, int mode);
};

compound_font_rep::compound_font_rep (
  string name, scheme_tree def2, array<font> fn2, double hzf2, double vzf2):
    font_rep (name, fn2[0]),
    def (def2), fn (fn2), cm (load_charmap (map_car (def))),
    hzf (hzf2), vzf (vzf2)
{}

bool
compound_font_rep::supports (string c) {
  int pos= 0, ch;
  string r;
  advance (c, pos, r, ch);
  return pos > 0;
}

void
compound_font_rep::advance (string s, int& pos, string& r, int& ch) {
  cm->advance (s, pos, r, ch);
  //cout << "(r,ch)= (" << r << "," << ch << ")\n";
  if (ch>0 && is_nil (fn[ch])) {
    tree t= def[ch][1];
    if (is_tuple (t, "virtual", 3))
      fn[ch]= virtual_font (this, as_string (t[1]), as_int (t[2]),
			    (int) tm_round (as_int (t[3]) * hzf),
                            (int) tm_round (as_int (t[3]) * vzf), false);
    else
      fn[ch]= find_magnified_font (t, hzf, vzf);
    ASSERT (!is_nil (fn[ch]), "font not found");
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
compound_font_rep::get_xpositions (string s, SI* xpos) {
  SI x= 0;
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->get_xpositions (r, xpos+start);
      for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      x= xpos[i];
    }
    else
      for (int j=0; j<=N(r); j++) xpos[start+j]= x;
  }
}

void
compound_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI x= 0;
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->get_xpositions (r, xpos+start, xk);
      for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      x= xpos[i];
    }
    else
      for (int j=0; j<=N(r); j++) xpos[start+j]= x;
  }
}

void
compound_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y);
      if (i < n) {
	fn[nr]->get_extents (r, ey);
	x += ey->x2;
      }
    }
  }
}

void
compound_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y, xk);
      if (i < n) {
	fn[nr]->get_extents (r, ey, xk);
	x += ey->x2;
      }
    }
  }
}

font
compound_font_rep::magnify (double zoomx, double zoomy) {
  return compound_font (def, hzf * zoomx, vzf * zoomy);
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

void
compound_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  if (pos >= N(s)) return;
  int i= pos, nr;
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0) { tm_char_forwards (s, pos); return; }
  int pos2= 0;
  fn[nr]->advance_glyph (r, pos2, ligf);
  if (pos + pos2 <= N(s) && r (0, pos2) == s (pos, pos+pos2)) pos += pos2;
  else tm_char_forwards (s, pos);
}

glyph
compound_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
}

int
compound_font_rep::index_glyph (string s, font_metric& fnm, font_glyphs& fng) {
  int i=0, n= N(s), nr;
  if (n == 0) return -1;
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0) return -1;
  return fn[nr]->index_glyph (r, fnm, fng);
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
  int i=0, n= N(s), nr=0;
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
  int i=0, n= N(s), nr=0;
  if (n == 0) return fn[0]->get_right_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_correction (r);
}

SI
compound_font_rep::get_lsub_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_lsub_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_lsub_correction (r);
}

SI
compound_font_rep::get_lsup_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_lsup_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_lsup_correction (r);
}

SI
compound_font_rep::get_rsub_correction (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_rsub_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_rsub_correction (r);
}

SI
compound_font_rep::get_rsup_correction (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_rsup_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_rsup_correction (r);
}

SI
compound_font_rep::get_wide_correction (string s, int mode) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_wide_correction (s, mode);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_wide_correction (r, mode);
}

/******************************************************************************
* User interface
******************************************************************************/

font
compound_font (scheme_tree def, double hzf, double vzf) {
  string name= tree_to_scheme (def);
  if (hzf != 1.0) name= name * "-hzoom=" * as_string (hzf);
  if (vzf != hzf) name= name * "-vzoom=" * as_string (vzf);
  if (font::instances->contains (name)) return font (name);
  array<font> fn (N(def));
  fn[0]= find_magnified_font (def[0][1], hzf, vzf);
  if (is_nil (fn[0])) return font ();
  return make (font, name, tm_new<compound_font_rep> (name, def, fn, hzf, vzf));
}
