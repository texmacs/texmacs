
/******************************************************************************
* MODULE     : font_protrusion.cpp
* DESCRIPTION: font protrusion
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"

/******************************************************************************
* Protrusion for western fonts
******************************************************************************/

void
add_upright_left_protrusion (hashmap<string,double>& t) {
  t ("`")= 0.7;
  t ("``")= 0.5;
  t ("(")= 0.05;
  t ("A")= 0.05;
  t ("J")= 0.05;
  t ("T")= 0.05;
  t ("V")= 0.05;
  t ("W")= 0.05;
  t ("X")= 0.05;
  t ("Y")= 0.05;
  t ("v")= 0.05;
  t ("w")= 0.05;
  t ("x")= 0.05;
  t ("y")= 0.05;
}

void
add_upright_right_protrusion (hashmap<string,double>& t) {
  t (".")= 0.7;
  t ("-")= 0.7;
  t (",")= 0.7;
  t ("'")= 0.7;
  t ("''")= 0.5;
  t (";")= 0.5;
  t (":")= 0.5;
  t ("--")= 0.3;
  t ("---")= 0.2;
  t ("!")= 0.2;
  t ("?")= 0.2;
  t (")")= 0.05;
  t ("A")= 0.05;
  t ("F")= 0.05;
  t ("K")= 0.05;
  t ("L")= 0.05;
  t ("T")= 0.05;
  t ("V")= 0.05;
  t ("W")= 0.05;
  t ("X")= 0.05;
  t ("Y")= 0.05;
  t ("k")= 0.05;
  t ("r")= 0.05;
  t ("s")= 0.05;
  t ("t")= 0.05;
  t ("v")= 0.05;
  t ("w")= 0.05;
  t ("x")= 0.05;
  t ("y")= 0.05;
}

/******************************************************************************
* Protrusion for CJK fonts
******************************************************************************/

void
add_cjk_left_protrusion (hashmap<string,double>& t) {
  t ("<#3008>")= 0.5;
  t ("<#300A>")= 0.5;
  t ("<#300C>")= 0.5;
  t ("<#300E>")= 0.5;
  t ("<#3016>")= 0.5;
  t ("<#3018>")= 0.5;
  t ("<#301A>")= 0.5;
  t ("<#301D>")= 0.5;
}

void
add_cjk_right_protrusion (hashmap<string,double>& t) {
  t ("<#3001>")= 0.5;
  t ("<#3002>")= 0.5;
  t ("<#3009>")= 0.5;
  t ("<#300B>")= 0.5;
  t ("<#300D>")= 0.5;
  t ("<#300F>")= 0.5;
  t ("<#3017>")= 0.5;
  t ("<#3019>")= 0.5;
  t ("<#301B>")= 0.5;
  t ("<#301E>")= 0.5;
  t ("<#301F>")= 0.5;
  t ("<#FF01>")= 0.5;
  t ("<#FF0C>")= 0.5;
  t ("<#FF0E>")= 0.5;
  t ("<#FF1A>")= 0.5;
  t ("<#FF1B>")= 0.5;
  t ("<#FF1F>")= 0.5;
}

/******************************************************************************
* Getting the protrusion tables
******************************************************************************/

static hashmap<string,double> no_protrusion (0.0);
static hashmap<string,double> cjk_left (0.0);
static hashmap<string,double> cjk_right (0.0);
static hashmap<string,double> cjk_inner_left (0.0);
static hashmap<string,double> cjk_inner_right (0.0);
static hashmap<string,double> western_left (0.0);
static hashmap<string,double> western_right (0.0);

hashmap<string,double>
get_left_protrusion_table (int mode) {
  switch (mode & (PROTRUSION_MASK + START_OF_LINE)) {
  case CJK_PROTRUSION:
    if (N(cjk_inner_left) == 0) add_cjk_left_protrusion (cjk_inner_left);
    return cjk_inner_left;
  case CJK_PROTRUSION + START_OF_LINE:
    if (N(cjk_left) == 0) add_cjk_left_protrusion (cjk_left);
    return cjk_left;
  case WESTERN_PROTRUSION:
  case WESTERN_PROTRUSION + START_OF_LINE:
    if (N(western_left) == 0) add_upright_left_protrusion (western_left);
    return western_left;
  default:
    return no_protrusion;
  }
}

hashmap<string,double>
get_right_protrusion_table (int mode) {
  switch (mode & (PROTRUSION_MASK + END_OF_LINE)) {
  case CJK_PROTRUSION:
    if (N(cjk_inner_right) == 0) {
      add_cjk_right_protrusion (cjk_inner_right);
      cjk_inner_right->reset ("<#3002>");
    }
    return cjk_inner_right;
  case CJK_PROTRUSION + END_OF_LINE:
    if (N(cjk_right) == 0) add_cjk_right_protrusion (cjk_right);
    return cjk_right;
  case WESTERN_PROTRUSION:
  case WESTERN_PROTRUSION + END_OF_LINE:
    if (N(western_right) == 0) add_upright_right_protrusion (western_right);
    return western_right;
  default:
    return no_protrusion;
  }
}

/******************************************************************************
* User interface
******************************************************************************/

SI
font_rep::get_left_protrusion (string s, int mode) {
  if (mode == 0 || N(s) == 0) return 0;
  int pos= 0;
  tm_char_forwards (s, pos);
  string first= s (0, pos);
  hashmap<string,double> t= get_left_protrusion_table (mode);
  if (t->contains (first)) {
    metric ex;
    get_extents (first, ex);
    double factor= t[first];
    return (SI) tm_round (factor * ex->x2);
  }
  return 0;
}

SI
font_rep::get_right_protrusion (string s, int mode) {
  if (mode == 0 || N(s) == 0) return 0;
  int pos= N(s);
  tm_char_backwards (s, pos);
  string last= s (pos, N(s));
  hashmap<string,double> t= get_right_protrusion_table (mode);
  if (t->contains (last)) {
    metric ex;
    get_extents (last, ex);
    double factor= t[last];
    return (SI) tm_round (factor * ex->x2);
  }
  return 0;
}
