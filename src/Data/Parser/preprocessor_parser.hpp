
/******************************************************************************
* MODULE     : preprocessor_parser.hpp
* DESCRIPTION: shared preprocessor parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PREPROCESSOR_PARSER_H
#define PREPROCESSOR_PARSER_H

#include "string.hpp"
#include "hashset.hpp"
#include "parser.hpp"

class preprocessor_parser_rep : public parser_rep {
public:
  preprocessor_parser_rep ();

  string get_parser_name () { return "preprocessor_parser"; }

  bool can_parse (string s, int pos);
  string to_string ();

  void set_start (string start);
  void set_directives (array<string> directives);

private:
  void do_parse (string s, int& pos);
  char m_start;
  hashset<string> m_directives;
};

#endif

