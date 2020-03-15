
/******************************************************************************
* MODULE     : keyword_parser.hpp
* DESCRIPTION: shared keyword parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef KEYWORD_PARSER_H
#define KEYWORD_PARSER_H

#include "parser.hpp"
#include "hashmap.hpp"

class keyword_parser_rep : public parser_rep {
public:
  keyword_parser_rep ();

  string get_parser_name () { return "keyword_parser"; }

  bool can_parse (string s, int pos);

  string get (string keyword) {
    if (is_empty (keyword)) return "";
    else return keyword_group[keyword];
  }

  void put (string keyword, string group) {
    if (is_empty (keyword)) return;

    keyword_group(keyword)= group;
  }

  void use_keywords_of_lang (string lang_code);

private:
  void do_parse (string s, int& pos);
  hashmap<string, string> keyword_group;
  string current_keyword;
};

#endif
