
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
  t ("'")= 0.7;
  t (",")= 0.7;
  t ("<#2018>")= 0.7;
  t ("<#2019>")= 0.7;
  t ("\016")= 0.2; // <
  t ("\017")= 0.2; // >
  t ("\020")= 0.5; // ``
  t ("\021")= 0.5; // ''
  t ("\022")= 0.5; // ,,
  t ("\023")= 0.2; // <<
  t ("\024")= 0.2; // >>
  t ("\025")= 0.3; // --
  t ("\026")= 0.2; // ---
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
  t ("`")= 0.7;
  t ("'")= 0.7;
  t (",")= 0.7;
  t ("<#2018>")= 0.7;
  t ("<#2019>")= 0.7;
  t ("\016")= 0.2; // <
  t ("\017")= 0.2; // >
  t ("\020")= 0.5; // ``
  t ("\021")= 0.5; // ''
  t ("\022")= 0.5; // ,,
  t ("\023")= 0.2; // <<
  t ("\024")= 0.2; // >>
  t ("\025")= 0.3; // --
  t ("\026")= 0.2; // ---
  t (".")= 0.7;
  t ("-")= 0.7;
  t (";")= 0.5;
  t (":")= 0.5;
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

void
add_accented_protrusion (hashmap<string,double>& t, string c, string s) {
  if (t->contains (c)) {
    int pos= 0;
    while (pos < N(s)) {
      int start= pos;
      tm_char_forwards (s, pos);
      t (s (start, pos))= t[c];
    }
  }
}

void
add_accented_protrusion (hashmap<string,double>& t) {
  add_accented_protrusion (t, "A", "\300\301\302\303\304\305\200\201");
  add_accented_protrusion (t, "C", "\307\202\203");
  add_accented_protrusion (t, "E", "\310\311\312\313\205\206");
  add_accented_protrusion (t, "G", "\207");
  add_accented_protrusion (t, "I", "\314\315\316\317\235");
  add_accented_protrusion (t, "L", "\210\212");
  add_accented_protrusion (t, "N", "\321\213\214");
  add_accented_protrusion (t, "O", "\322\323\324\325\326\216");
  add_accented_protrusion (t, "R", "\217\220");
  add_accented_protrusion (t, "S", "\221\222\223");
  add_accented_protrusion (t, "T", "\224\225");
  add_accented_protrusion (t, "U", "\331\332\333\334\226\227");
  add_accented_protrusion (t, "Y", "\335\230");
  add_accented_protrusion (t, "Z", "\231\232\233");
  add_accented_protrusion (t, "a", "\340\341\342\343\344\345\240\241");
  add_accented_protrusion (t, "c", "\347\242\243");
  add_accented_protrusion (t, "e", "\350\351\352\353\245\246");
  add_accented_protrusion (t, "g", "\247");
  add_accented_protrusion (t, "i", "\354\355\356\357");
  add_accented_protrusion (t, "l", "\250\252");
  add_accented_protrusion (t, "n", "\361\253\254");
  add_accented_protrusion (t, "o", "\362\363\364\365\366\256");
  add_accented_protrusion (t, "r", "\257\260");
  add_accented_protrusion (t, "s", "\261\262\263");
  add_accented_protrusion (t, "t", "\265");
  add_accented_protrusion (t, "u", "\371\372\373\374\266\267");
  add_accented_protrusion (t, "y", "\375\270");
  add_accented_protrusion (t, "z", "\271\272\273");
}

void
add_western (hashmap<string,double>& t, string font_name, bool right) {
  (void) font_name;
  if (right) add_upright_right_protrusion (t);
  else add_upright_left_protrusion (t);
  add_accented_protrusion (t);
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

void
add_quanjiao (hashmap<string,double>& t, int mode, bool right) {
  // FIXME: successions of several punctuation symbols
  if (right) {
    if ((mode & END_OF_LINE) != 0)
      add_cjk_right_protrusion (t);
  }
  else {
    if ((mode & START_OF_LINE) != 0)
      add_cjk_left_protrusion (t);
  }
}

void
add_banjiao (hashmap<string,double>& t, int mode, bool right) {
  (void) mode;
  if (right) add_cjk_right_protrusion (t);
  else add_cjk_left_protrusion (t);
}

void
add_hangmobanjiao (hashmap<string,double>& t, int mode, bool right) {
  if (right) {
    if ((mode & END_OF_LINE) != 0)
      add_cjk_right_protrusion (t);
  }
  else {
    if ((mode & START_OF_LINE) != 0)
      add_cjk_left_protrusion (t);
  }
}

void
add_kaiming (hashmap<string,double>& t, int mode, bool right) {
  if (right) {
    add_cjk_right_protrusion (t);
    if ((mode & END_OF_LINE) == 0) {
      t->reset ("<#3002>");
    }
  }
  else add_cjk_left_protrusion (t);
}

/******************************************************************************
* Setting up the global protrusion tables
******************************************************************************/

static hashmap<string,int> protrusion_index_table (-1);
static array<hashmap<string,double> > protrusion_tables;

int
init_protrusion_table (string font_name, int mode, bool right) {
  int index= (mode<<1) + (right?1:0);
  string key= font_name * ":" * as_string (index);
  if (!protrusion_index_table->contains (key)) {
    int im= N(protrusion_tables);
    protrusion_index_table (key)= im;
    hashmap<string,double> t (0.0);
    if ((mode & WESTERN_PROTRUSION) != 0)
      add_western (t, font_name, right);
    switch (mode & CJK_PROTRUSION_MASK) {
    case QUANJIAO:
      add_quanjiao (t, mode, right);
      break;
    case BANJIAO:
      add_banjiao (t, mode, right);
      break;
    case HANGMOBANJIAO:
      add_hangmobanjiao (t, mode, right);
      break;
    case KAIMING:
      add_kaiming (t, mode, right);
      break;
    }
    protrusion_tables << t;
  }
  return protrusion_index_table [key];
}

/******************************************************************************
* User interface
******************************************************************************/

SI
font_rep::get_left_protrusion (string s, int mode) {
  /*
  static bool done= false;
  if (!done) {
    cout << "Font= " << res_name << "\n";
    glyph o= get_glyph ("o");
    for (int c=33; c<127; c++) {
      string s ((char) c);
      glyph g= get_glyph (s);
      cout << s << " -> " << left_protrusion (g, o) << "\n";
    }
    done= true;
  }
  */

  if (mode == 0 || N(s) == 0) return 0;
  int index= (mode<<1);
  if (!protrusion_maps->contains (index)) {
    int code= init_protrusion_table (res_name, mode, false);
    protrusion_maps (index)= code;
  }
  hashmap<string,double> t= protrusion_tables [protrusion_maps [index]];

  int pos= 0;
  tm_char_forwards (s, pos);
  string first= s (0, pos);
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
  int index= (mode<<1) + 1;
  if (!protrusion_maps->contains (index)) {
    int code= init_protrusion_table (res_name, mode, true);
    protrusion_maps (index)= code;
  }
  hashmap<string,double> t= protrusion_tables [protrusion_maps [index]];

  int pos= N(s);
  tm_char_backwards (s, pos);
  string last= s (pos, N(s));
  if (t->contains (last)) {
    metric ex;
    get_extents (last, ex);
    double factor= t[last];
    return (SI) tm_round (factor * ex->x2);
  }
  return 0;
}
