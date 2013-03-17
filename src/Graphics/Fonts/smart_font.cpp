
/******************************************************************************
* MODULE     : smart_font.cpp
* DESCRIPTION: smart merging of several fonts for different unicode ranges
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "convert.hpp"
#include "converter.hpp"

typedef int int_vector[256];
typedef hashmap<string,int> int_table;

#define SUBFONT_MAIN           0
#define SUBFONT_ERROR          1
#define SUBFONT_ASCII          2
#define SUBFONT_LATIN          3
#define SUBFONT_LATINA         4
#define SUBFONT_GREEK          5
#define SUBFONT_CYRILLIC       6
#define SUBFONT_CJK            7
#define SUBFONT_HANGUL         8
#define SUBFONT_MATH           9
#define SUBFONT_MATH_EXTRA    10
#define SUBFONT_MATH_LETTERS  11
#define SUBFONT_TOTAL         12

/******************************************************************************
* The smart font class
******************************************************************************/

struct smart_font_rep: font_rep {
  string family;
  string variant;
  string series;
  string shape;
  int    sz;
  int    dpi;

  array<font> fn;
  int_vector  chv;
  int_table   cht;

  smart_font_rep (string name, font base_fn, font err_fn,
                  string family, string variant,
                  string series, string shape, int sz, int dpi);

  void   advance (string s, int& pos, string& r, int& ch);
  int    search_subfont (string c);
  int    resolve (string c);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  font   magnify (double zoom);
  glyph  get_glyph (string s);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

smart_font_rep::smart_font_rep (
  string name, font base_fn, font err_fn, string family2, string variant2,
  string series2, string shape2, int sz2, int dpi2):
  font_rep (name, base_fn), family (family2), variant (variant2),
    series (series2), shape (shape2), sz (sz2), dpi (dpi2),
    fn (SUBFONT_TOTAL), cht (-1)
{
  fn[0]= base_fn;
  fn[1]= err_fn;
  for (int i=0; i<256; i++) chv[i]= -1;
}

/******************************************************************************
* Smart font resolution
******************************************************************************/

void
smart_font_rep::advance (string s, int& pos, string& r, int& ch) {
  int start= pos;
  ch= -1;
  while (pos < N(s)) {
    if (s[pos] != '<') {
      int c= (int) (unsigned char) s[pos];
      int next= chv[c];
      if (chv[c] == -1) next= resolve (s (pos, pos+1));
      if (next == ch) pos++;
      else if (ch == -1) { pos++; ch= next; }
      else break;
    }
    else {
      int end= pos;
      tm_char_forwards (s, end);
      int next= cht[s (pos, end)];
      if (next == -1) next= resolve (s (pos, end));
      if (next == ch) pos= end;
      else if (ch == -1) { pos= end; ch= next; }
      else break;
    }
  }
  r= s (start, pos);
}

int
smart_font_rep::search_subfont (string c) {
  if (fn[SUBFONT_MAIN]->supports (c)) return SUBFONT_MAIN;
  string uc= cork_to_utf8 (c);
  int pos= 0;
  int code= decode_from_utf8 (uc, pos);
  if (pos != N(uc)) return SUBFONT_ERROR; // fall back to virtual fonts
  int nr= 1;
  if (code <= 0x7f) nr= SUBFONT_ASCII;
  else if (code >= 0x80 && code <= 0xff) nr= SUBFONT_LATIN;
  else if (code >= 0x100 && code <= 0x17f) nr= SUBFONT_LATINA;
  else if (code >= 0x380 && code <= 0x3ff) nr= SUBFONT_GREEK;
  else if (code >= 0x400 && code <= 0x4ff) nr= SUBFONT_CYRILLIC;
  else if (code >= 0x4e00 && code <= 0x9fcc) nr= SUBFONT_CJK;
  else if (code >= 0xac00 && code <= 0xd7af) nr= SUBFONT_HANGUL;
  else if (code >= 0x2000 && code <= 0x23ff) nr= SUBFONT_MATH;
  else if (code >= 0x2900 && code <= 0x2e7f) nr= SUBFONT_MATH_EXTRA;
  else if (code >= 0x1d400 && code <= 0x1d7ff) nr= SUBFONT_MATH_LETTERS;
  if (is_nil (fn[nr])) {
    string range= "";
    switch (nr) {
    case SUBFONT_ASCII: range= "+ascii"; break;
    case SUBFONT_LATIN: range= "+latin1basic"; break;
    case SUBFONT_LATINA: range= "+latina"; break;
    case SUBFONT_GREEK: range= "+greekbasic"; break;
    case SUBFONT_CYRILLIC: range= "+cyrillicbasic"; break;
    case SUBFONT_CJK: range= "+cjk"; break;
    case SUBFONT_HANGUL: range= "+hangul"; break;
    case SUBFONT_MATH: range= "+math"; break;
    case SUBFONT_MATH_EXTRA: range= "+mathextra"; break;
    case SUBFONT_MATH_LETTERS: range= "+mathletters"; break;
    default: nr= 1;
    }
    string v= variant;
    if (v == "rm") v= range;
    else v= v * "-" * range;
    fn[nr]= closest_font (family, v, series, shape, sz, dpi);
  }
  if (nr != SUBFONT_ERROR && !fn[nr]->supports (c)) nr= SUBFONT_ERROR;
  return nr;
}

int
smart_font_rep::resolve (string c) {
  int nr= search_subfont (c);
  if (starts (c, "<")) cht (c)= nr;
  else chv [(int) (unsigned char) c[0]]= nr;
  return nr;
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

bool
smart_font_rep::supports (string c) {
  (void) c;
  return true;
}

void
smart_font_rep::get_extents (string s, metric& ex) {
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
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
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

font
smart_font_rep::magnify (double zoom) {
  return smart_font (family, variant, series, shape, sz,
                     (int) tm_round (dpi * zoom));
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

glyph
smart_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
}

double
smart_font_rep::get_left_slope  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_slope (r);
}

double
smart_font_rep::get_right_slope (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_slope (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_slope (r);
}

SI
smart_font_rep::get_left_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_correction (r);
}

SI
smart_font_rep::get_right_correction (string s) {
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
smart_font (string family, string variant, string series, string shape,
            int sz, int dpi) {
  string name=
    family * "-" * variant * "-" *
    series * "-" * shape * "-" *
    as_string (sz) * "-" * as_string (dpi) * "-smart";
  if (font::instances->contains (name)) return font (name);
  font base_fn= closest_font (family, variant, series, shape, sz, dpi);
  if (is_nil (base_fn)) return font ();
  font err_fn= error_font (base_fn);
  return make (font, name,
               tm_new<smart_font_rep> (name, base_fn, err_fn, family, variant,
                                       series, shape, sz, dpi));
}
