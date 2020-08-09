
/******************************************************************************
* MODULE     : operator_parser.cpp
* DESCRIPTION: shared operator parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "operator_parser.hpp"
#include "scheme.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

operator_parser_rep::operator_parser_rep () {
  operator_group= hashmap<string, string>();
}

bool
operator_parser_rep::can_parse (string s, int pos) {
  if (!parser_rep::can_parse (s, pos)) return false;

  iterator<string> iter= iterate (operator_group);
  while (iter->busy ()) {
    string oper= iter->next ();
    if (test (s, pos, oper)) {
      current_oper= oper;
      return true;
    }
  }
  return false;
}

void
operator_parser_rep::do_parse (string s, int& pos) {
  int current_oper_size= N(current_oper);
  if (current_oper_size <= 0) {
    debug_packrat << "current_oper is empty unexpectedly with "
                  << pos << ":" << s << LF;
    return;
  }

  // Always use the longer matched operator
  iterator<string> iter= iterate (operator_group);
  while (iter->busy ()) {
    string oper= iter->next ();
    int oper_size= N(oper);
    if (current_oper_size >= N(oper)) continue;
    if (starts (oper, current_oper) && test (s, pos, oper)) {
      current_oper= oper;
      current_oper_size= oper_size;
    }
  }

  pos+= current_oper_size;
}
