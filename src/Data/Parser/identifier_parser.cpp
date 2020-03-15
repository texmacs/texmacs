
/******************************************************************************
* MODULE     : identifier_parser.cpp
* DESCRIPTION: shared identifier parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "identifier_parser.hpp"
#include "analyze.hpp"

identifier_parser_rep::identifier_parser_rep () {
  start_with_alpha= true;
  start_chars << '_';
  extra_chars << '_';
}

void
identifier_parser_rep::set_start_chars (array<char> p_chars) {
  start_chars= p_chars;
}

void
identifier_parser_rep::set_extra_chars (array<char> p_chars) {
  extra_chars= p_chars;
}

bool
identifier_parser_rep::can_parse (string s, int pos) {
  if (!parser_rep::can_parse (s, pos)) return false;
  
  if (start_with_alpha && is_alpha (s[pos])) return true;
  if (contains (s[pos], start_chars)) return true;
  return false;
}

inline bool is_valid (char c, array<char> chars) {
  return is_digit (c) || is_alpha (c) || contains (c, chars);
}

void
identifier_parser_rep::do_parse (string s, int& pos) {
  while (pos<N(s) && is_valid (s[pos], extra_chars)) {
    pos= pos+1;
  }
}
