
/******************************************************************************
* MODULE     : rubber_unicode_font.cpp
* DESCRIPTION: Assemble rubber characters from pieces in Unicode fonts
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "converter.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* True Type fonts
******************************************************************************/

struct rubber_assemble_font_rep: font_rep {
  font base;
  array<font> larger;

  rubber_assemble_font_rep (string name, font base);
  array<string> get_pieces (string s, int& num);

  bool supports (string c);
  void get_extents (string s, metric& ex);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  void draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font magnify (double zoom);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

rubber_assemble_font_rep::rubber_assemble_font_rep (string name, font base2):
  font_rep (name, base2), base (base2)
{
  this->copy_math_pars (base);
  larger << base;
  double m= sqrt (sqrt (2.0)), p= m;
  for (int i=1; i<=4; i++) {
    larger << base->magnify (p);
    p *= m;
  }
}

array<string>
rubber_assemble_font_rep::get_pieces (string s, int& num) {
  array<string> pieces;
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      num= as_int (s (pos+1, N(s)-1));
      int nr= num - 5;
      if (num <= 4)
        pieces << r;
      else if (r == "(") {
        pieces << string ("<#239B>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#239C>");
        pieces << string ("<#239D>");
      }
      else if (r == ")") {
        pieces << string ("<#239E>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#239F>");
        pieces << string ("<#23A0>");
      }
      else if (r == "[") {
        pieces << string ("<#23A1>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23A2>");
        pieces << string ("<#23A3>");
      }
      else if (r == "]") {
        pieces << string ("<#23A4>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23A5>");
        pieces << string ("<#23A6>");
      }
      else if (r == "{") {
        pieces << string ("<#23A7>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23AA>");
        pieces << string ("<#23A8>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23AA>");
        pieces << string ("<#23A9>");
      }
      else if (r == "}") {
        pieces << string ("<#23AB>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23AA>");
        pieces << string ("<#23AC>");
        for (int i=0; i<nr; i++)
          pieces << string ("<#23AA>");
        pieces << string ("<#23AD>");
      }
    }
  }
  return reverse (pieces);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
rubber_assemble_font_rep::supports (string s) {
  if (starts (s, "<mid-")) s= "<left-" * s (5, N(s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N(s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N(s), s);
    if (pos > 6) {
      string r= s (6, pos);
      return r == "(" || r == ")" ||
             r == "[" || r == "]" ||
             r == "{" || r == "}";
    }
  }
  return false;
}

void
rubber_assemble_font_rep::get_extents (string s, metric& ex) {
  int num;
  array<string> pieces= get_pieces (s, num);
  if (num <= 4)
    larger[num]->get_extents (s, ex);
  else {
    ex->x1= ex->x3= ex->y3= PLUS_INFINITY;
    ex->x2= ex->x4= ex->y4= MINUS_INFINITY;
    ex->y1= ex->y2= 0;
    SI y=0;
    for (int i=0; i<N(pieces); i++) {
      metric ey;
      base->get_extents (pieces[i], ey);
      ex->x1= min (ex->x1, ey->x1);
      ex->y1= min (ex->y1, ey->y1 + y);
      ex->x2= max (ex->x2, ey->x2);
      ex->y2= max (ex->y2, ey->y2 + y);
      ex->x3= min (ex->x3, ey->x3);
      ex->y3= min (ex->y3, ey->y3 + y);
      ex->x4= max (ex->x4, ey->x4);
      ex->y4= max (ex->y4, ey->y4 + y);
      y += (ey->y2 - ey->y1);
    }
  }
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int num;
  array<string> pieces= get_pieces (s, num);
  if (num <= 4)
    larger[num]->draw_fixed (ren, s, x, y);
  else {
    SI dy=0;
    for (int i=0; i<N(pieces); i++) {
      metric ey;
      base->get_extents (pieces[i], ey);
      base->draw_fixed (ren, pieces[i], x, y + dy);
      dy += (ey->y2 - ey->y1);
    }
  }
}

void
rubber_assemble_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int num;
  array<string> pieces= get_pieces (s, num);
  if (num <= 4)
    larger[num]->draw_fixed (ren, s, x, y, xk);
  else {
    SI dy=0;
    for (int i=0; i<N(pieces); i++) {
      metric ey;
      base->get_extents (pieces[i], ey);
      base->draw_fixed (ren, pieces[i], x, y + dy, xk);
      dy -= (ey->y2 - ey->y1);
    }
  }
}

font
rubber_assemble_font_rep::magnify (double zoom) {
  return rubber_assemble_font (base->magnify (zoom));
}

/******************************************************************************
* Interface
******************************************************************************/

font
rubber_assemble_font (font base) {
  string name= "rubberassemble[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_assemble_font_rep> (name, base));
}

#else

font
rubber_assemble_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
