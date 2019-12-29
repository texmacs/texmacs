
/******************************************************************************
* MODULE     : inline_comment_parser.hpp
* DESCRIPTION: shared inline comment parsing routines
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef INLINE_COMMENT_PARSER_H
#define INLINE_COMMENT_PARSER_H

#include "parser.hpp"
#include "list.hpp"

class inline_comment_parser_rep : public parser_rep {
public:
  inline_comment_parser_rep();
  string get_parser_name () { return "inline_comment_parser"; }

  void set_starts(const list<string>& p_starts);
  bool can_parse (string s, int pos);

private:
  list<string> m_starts;
  void do_parse (string s, int& pos);
};

#endif