/******************************************************************************
* MODULE     : impl_language.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "path.hpp"

extern tree the_et;

/*
 static bool
 is_line (tree t) {
 path p= obtain_ip (t);
 if (is_nil (p) || last_item (p) < 0) return false;
 tree pt= subtree (the_et, reverse (p->next));
 if (!is_func (pt, DOCUMENT)) return false;
 return true;
 }
 */

int
line_number (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return p->item;
}

int
number_of_lines (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return N(pt);
}

tree
line_inc (tree t, int i) {
  if (i == 0) return t;
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return tree (ERROR);
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return tree (ERROR);
  if ((p->item + i < 0) || (p->item + i >= N(pt))) return tree (ERROR);
  return pt[p->item + i];
}


bool
abstract_language_rep::belongs_to_identifier(char c) {
  return (is_digit (c) || is_alpha (c) || (c=='_'));
}

void
abstract_language_rep::parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (!t->contains (s (pos, i))) pos= i;
}

void
abstract_language_rep::parse_keyword (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}

void
abstract_language_rep::parse_type (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}

void
abstract_language_rep::parse_constant (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}
