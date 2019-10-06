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
abstract_language_rep::parse_alpha (string s, int& pos) {
  static hashmap<string,string> empty;
  parse_identifier (empty, s, pos);
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
