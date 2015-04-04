
/******************************************************************************
* MODULE     : indexation.cpp
* DESCRIPTION: Routines for the indexation of documents
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"
#include "file.hpp"

/******************************************************************************
* Computing scores for words in a file
******************************************************************************/

typedef bool char_selection[256];

struct score_computer {
  hashmap<string,int> score;
  array<string> keys;

  string fm;
  char_selection start_list;
  char_selection char_list;
  hashmap<string,int> multiplier;

  score_computer (string fm2);
  void initialize_acceptable (char_selection& sel, string accepted);
  void compute (string s, int m= 1);
  void compute (tree t, int m= 1);
  array<string> retrieve_keys ();
  scheme_tree retrieve_scores ();
};

score_computer::score_computer (string fm2):
  score (0), keys (0), fm (fm2), multiplier (1)
{
  string first= "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  string all= first * string ("_");
  if (fm == "texmacs") {
    multiplier ("explain") = 10;
  }
  if (fm == "scheme") {
    all << "-*/?";
    multiplier ("define") = 10;
    multiplier ("define-public") = 10;
    multiplier ("define-macro") = 10;
    multiplier ("define-public-macro") = 10;
    multiplier ("tm-define") = 10;
    multiplier ("tm-define-macro") = 10;
    multiplier ("define-menu") = 10;
    multiplier ("tm-menu") = 10;
    multiplier ("menu-bind") = 10;
    multiplier ("tm-widget") = 10;
    multiplier ("smart-table") = 10;
  }
  initialize_acceptable (start_list, first);
  initialize_acceptable (char_list, all);
}

void
score_computer::initialize_acceptable (char_selection& sel, string accepted) {
  for (int i=0; i<256; i++)
    sel[i]= false;
  for (int i=0; i<N(accepted); i++)
    sel[(int) (unsigned char) (accepted[i])]= true;
}

void
score_computer::compute (string s, int m) {
  for (int i=0; i<N(s); ) {
    if (char_list[(int) (unsigned char) s[i]]) {
      int start= i;
      while (i<N(s) && char_list[(int) (unsigned char) s[i]]) i++;
      if (start_list[(int) (unsigned char) s[start]]) {
        string ss= locase_all (s (start, i));
        if (!score->contains (ss)) keys << ss;
        score (ss)= min (score[ss] + m, 10000);
        m= multiplier[ss];
      }
      else m= 1;
    }
    else i++;
  }
}

void
score_computer::compute (tree t, int m) {
  if (is_atomic (t)) compute (t->label, m);
  else if (!is_func (t, RAW_DATA)) {
    for (int i=0; i<N(t); i++) {
      string tag= as_string (L(t));
      string qtag= "<" * tag * ">";
      if (i == 0) m= max (m, multiplier[tag]);
      else m= 1;
      score(qtag)= score[qtag] + 1;
      compute (t[i], m);
    }
  }
}

array<string>
score_computer::retrieve_keys () {
  return keys;
}

scheme_tree
score_computer::retrieve_scores () {
  array<string> r= retrieve_keys ();
  scheme_tree t (TUPLE);
  for (int i=0; i<N(r); i++)
    t << tree (TUPLE, r[i], as_string (score[r[i]]));
  return t;
}

/******************************************************************************
* Public interface
******************************************************************************/

array<string>
compute_keys (string s, string fm) {
  score_computer comp (fm);
  comp.compute (s);
  return comp.retrieve_keys ();
}

scheme_tree
compute_index (string s, string fm) {
  score_computer comp (fm);
  comp.compute (s);
  return comp.retrieve_scores ();
}

array<string>
compute_keys (tree t, string fm) {
  score_computer comp (fm);
  comp.compute (t);
  return comp.retrieve_keys ();
}

scheme_tree
compute_index (tree t, string fm) {
  score_computer comp (fm);
  comp.compute (t);
  return comp.retrieve_scores ();
}

array<string>
compute_keys (url u) {
  string s;
  load_string (u, s, false);
  string fm= get_format (s, suffix (u));
  if (fm != "texmacs") return compute_keys (s, fm);
  tree t= texmacs_document_to_tree (s);
  return compute_keys (t, fm);
}

scheme_tree
compute_index (url u) {
  string s;
  load_string (u, s, false);
  string fm= get_format (s, suffix (u));
  if (fm != "texmacs") return compute_index (s, fm);
  tree t= texmacs_document_to_tree (s);
  return compute_index (t, fm);
}
