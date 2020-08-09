
/******************************************************************************
* MODULE     : operator_parser.hpp
* DESCRIPTION: shared operator parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OPERATOR_PARSER_H
#define OPERATOR_PARSER_H

#include "string.hpp"
#include "hashmap.hpp"
#include "parser.hpp"

class operator_parser_rep : public parser_rep {
public:
  operator_parser_rep ();

  string get_parser_name () { return "operator_parser"; }

  bool can_parse (string s, int pos);

  string get (string oper) {
    if (is_empty (oper)) return "";
    else return operator_group[oper];
  }

  void put (string oper, string group) {
    if (is_empty (oper)) return;

    operator_group(oper)= group;
  }


private:
  void do_parse (string s, int& pos);
  hashmap<string, string> operator_group;
  string current_oper;
};

#endif
