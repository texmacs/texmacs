
/******************************************************************************
* MODULE     : identifier_parser.hpp
* DESCRIPTION: shared identifier parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IDENTIFIER_PARSER_H
#define IDENTIFIER_PARSER_H

#include "parser.hpp"

class identifier_parser_rep : public parser_rep {
public:
  identifier_parser_rep ();

  string get_parser_name () { return "identifier_parser"; }

  bool can_parse (string s, int pos);

private:
  bool start_with_alpha;
  bool start_with_underline;
  void do_parse (string s, int& pos);
};

#endif
