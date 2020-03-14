
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
  start_with_underline= true;
}

bool
identifier_parser_rep::can_parse (string s, int pos) {
  if (!parser_rep::can_parse (s, pos)) return false;
  
  if (start_with_alpha && is_alpha (s[pos])) return true;
  if (start_with_underline && s[pos] == '_') return true;
  return false;
}

bool is_valid (char c) {
  return is_digit (c) || is_alpha (c) ||  (c=='_');
}

void
identifier_parser_rep::do_parse (string s, int& pos) {
  while (pos<N(s) && is_valid (s[pos])) {
    pos= pos+1;
  }
}
