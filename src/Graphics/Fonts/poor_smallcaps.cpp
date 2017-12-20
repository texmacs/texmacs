
/******************************************************************************
* MODULE     : poor_smallcaps.cpp
* DESCRIPTION: fonts which are agglomerated from several other fonts.
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "universal.hpp"

/******************************************************************************
* The poor smallcaps font class
******************************************************************************/

struct poor_smallcaps_font_rep: font_rep {
  font         base;
  array<font>  subfn;

  poor_smallcaps_font_rep (string name, font base);
  bool   supports (string c);
  void   advance (string s, int& pos, string& r, int& ch);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
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

poor_smallcaps_font_rep::poor_smallcaps_font_rep (
  string name, font base2):
    font_rep (name, base2), base (base2), subfn (2)
{
  this->copy_math_pars (base);
  subfn[0]= base;
  if (base->supports ("x") && base->supports ("X")) {
    metric ex, eX;
    base->get_extents ("x", ex);
    base->get_extents ("X", eX);
    double hx= ex->y2 - ex->y1;
    double hX= eX->y2 - eX->y1;
    double sc= hx / hX;
    subfn[1]= base->magnify (sqrt (sc), sc / sqrt (sqrt (sc)));
  }
  else subfn[1]= base->magnify (0.75);
}

bool
poor_smallcaps_font_rep::supports (string c) {
  return base->supports (uni_upcase_all (c));
}

void
poor_smallcaps_font_rep::advance (string s, int& pos, string& r, int& nr) {
  int start= pos;
  nr= -1;
  while (pos < N(s)) {
    int end= pos;
    tm_char_forwards (s, end);
    string c1= s (pos, end);
    string c2= uni_upcase_char (c1);
    int next= ((c1 != c2)? 1: 0);
    if (next == nr) pos= end;
    else if (nr == -1) { pos= end; nr= next; }
    else break;
  }
  r= s (start, pos);
  if (nr == 1) r= uni_upcase_all (r);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

void
poor_smallcaps_font_rep::get_extents (string s, metric& ex) {
  int i=0, n= N(s);
  subfn[0]->get_extents (empty_string, ex);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      subfn[nr]->get_extents (r, ey);
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
poor_smallcaps_font_rep::get_xpositions (string s, SI* xpos) {
  SI x= 0;
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      subfn[nr]->get_xpositions (r, xpos+start);
      for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      x= xpos[i];
    }
    else
      for (int j=0; j<=N(r); j++) xpos[start+j]= x;
  }
}

void
poor_smallcaps_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI x= 0;
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    int start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      subfn[nr]->get_xpositions (r, xpos+start, xk);
      for (int j=0; j<=N(r); j++) xpos[start+j] += x;
      x= xpos[i];
    }
    else
      for (int j=0; j<=N(r); j++) xpos[start+j]= x;
  }
}

void
poor_smallcaps_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      subfn[nr]->draw_fixed (ren, r, x, y);
      if (i < n) {
	subfn[nr]->get_extents (r, ey);
	x += ey->x2;
      }
    }
  }
}

void
poor_smallcaps_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      subfn[nr]->draw_fixed (ren, r, x, y, xk);
      if (i < n) {
	subfn[nr]->get_extents (r, ey, xk);
	x += ey->x2;
      }
    }
  }
}

font
poor_smallcaps_font_rep::magnify (double zoomx, double zoomy) {
  return poor_smallcaps_font (base->magnify (zoomx, zoomy));
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

glyph
poor_smallcaps_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return subfn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return subfn[nr]->get_glyph (r);
}

int
poor_smallcaps_font_rep::index_glyph (string s, font_metric& fnm,
                                                font_glyphs& fng) {
  int i=0, n= N(s), nr;
  if (n == 0) return -1;
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0) return -1;
  return subfn[nr] -> index_glyph (r, fnm, fng);
}

double
poor_smallcaps_font_rep::get_left_slope  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return subfn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_left_slope (r);
}

double
poor_smallcaps_font_rep::get_right_slope (string s) {
  int i=0, n= N(s), nr=0;
  if (n == 0) return subfn[0]->get_right_slope (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_right_slope (r);
}

SI
poor_smallcaps_font_rep::get_left_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return subfn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_left_correction (r);
}

SI
poor_smallcaps_font_rep::get_right_correction (string s) {
  int i=0, n= N(s), nr=0;
  if (n == 0) return subfn[0]->get_right_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_right_correction (r);
}

SI
poor_smallcaps_font_rep::get_lsub_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return subfn[0]->get_lsub_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_lsub_correction (r);
}

SI
poor_smallcaps_font_rep::get_lsup_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return subfn[0]->get_lsup_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_lsup_correction (r);
}

SI
poor_smallcaps_font_rep::get_rsub_correction (string s) {
  int i=0, n= N(s), nr=0;
  if (n == 0) return subfn[0]->get_rsub_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_rsub_correction (r);
}

SI
poor_smallcaps_font_rep::get_rsup_correction (string s) {
  int i=0, n= N(s), nr=0;
  if (n == 0) return subfn[0]->get_rsup_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_rsup_correction (r);
}

SI
poor_smallcaps_font_rep::get_wide_correction (string s, int mode) {
  int i=0, n= N(s), nr=0;
  if (n == 0) return subfn[0]->get_wide_correction (s, mode);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return subfn[nr]->get_wide_correction (r, mode);
}

/******************************************************************************
* User interface
******************************************************************************/

font
poor_smallcaps_font (font base) {
  string name= base->res_name * "-smallcapsed";
  if (font::instances->contains (name)) return font (name);
  return make (font, name, tm_new<poor_smallcaps_font_rep> (name, base));
}
