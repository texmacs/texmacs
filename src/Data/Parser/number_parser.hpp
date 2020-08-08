
/******************************************************************************
* MODULE     : number_parser.hpp
* DESCRIPTION: shared number parsing routines for various programming languages
* COPYRIGHT  : (C) 2019-2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NUMBER_PARSER_H
#define NUMBER_PARSER_H

#include "parser.hpp"
#include "keyword_parser.hpp"
#include "hashset.hpp"

class number_parser_rep : public parser_rep {
public:
  number_parser_rep ();

  bool can_parse (string s, int pos);
  string get_parser_name () { return "number_parser"; }

  inline keyword_parser_rep get_suffix_parser () { return suffix_parser; }

  string PREFIX_0B= "prefix_0b";
  string PREFIX_0O= "prefix_0o";
  string PREFIX_0X= "prefix_0x";
  string NO_SUFFIX_WITH_BOX= "no_suffix_with_box";

  string SCIENTIFIC_NOTATION= "sci_notation";


  inline void insert_bool_feature (string feature) {
    bool_features->insert (feature);
  }
  inline void remove_bool_feature (string feature) {
    bool_features->remove (feature);
  }

  inline bool prefix_0b () { return bool_features->contains (PREFIX_0B); }
  inline void support_prefix_0b (bool param) {
    if (param) insert_bool_feature (PREFIX_0B);
    else       remove_bool_feature (PREFIX_0B);
  }

  inline bool prefix_0o () { return bool_features->contains (PREFIX_0O); }
  inline void support_prefix_0o (bool param) {
    if (param) insert_bool_feature (PREFIX_0O);
    else       remove_bool_feature (PREFIX_0O);
  }

  inline bool prefix_0x () { return bool_features->contains (PREFIX_0X); }
  inline void support_prefix_0x (bool param) {
    if (param) insert_bool_feature (PREFIX_0X);
    else       remove_bool_feature (PREFIX_0X);
  }

  inline bool no_suffix_with_box () {
    return bool_features->contains (NO_SUFFIX_WITH_BOX);
  }
  inline void support_no_suffix_with_box (bool param) {
    if (param) insert_bool_feature (NO_SUFFIX_WITH_BOX);
    else       remove_bool_feature (NO_SUFFIX_WITH_BOX);
  }

  inline bool scientific_notation () { return bool_features->contains (SCIENTIFIC_NOTATION); }
  inline void support_scientific_notation (bool param) {
    if (param) insert_bool_feature (SCIENTIFIC_NOTATION);
    else       remove_bool_feature (SCIENTIFIC_NOTATION);
  }

  inline void support_separator (string param) {
    if (N(param) == 1) {
      separator= param[0];
    }
  }
  inline void support_separator (char param) {
    separator= param;
  }
  inline bool is_separator (char param) { return separator != '\0' && separator == param; }

  void use_fortran_style ();
  void use_r_style ();

private:
  char separator;
  hashset<string> bool_features;
  keyword_parser_rep suffix_parser;

  void do_parse (string s, int& pos);

  bool can_parse_prefix_0b (string s, int pos);
  bool can_parse_prefix_0o (string s, int pos);
  bool can_parse_prefix_0x (string s, int pos);

  void parse_binary (string s, int& pos);
  void parse_hex (string s, int& pos);
  void parse_octal (string s, int& pos);
  void parse_decimal (string s, int& pos);
};

#endif // defined NUMBER_PARSER_H
