
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

  string DOUBLE_SUFFIX= "double_suffix";
  string FLOAT_SUFFIX= "float_suffix";
  string I_SUFFIX= "i_suffix";
  string J_SUFFIX= "j_suffix";
  string LONG_SUFFIX= "long_suffix";
  string ULL_SUFFIX= "ull_suffix";
  string LOCASE_I_SUFFIX= "locase_i_suffix";

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

  inline bool double_suffix () { return bool_features->contains (DOUBLE_SUFFIX); }
  inline void support_double_suffix (bool param) {
    if (param) insert_bool_feature (DOUBLE_SUFFIX);
    else       remove_bool_feature (DOUBLE_SUFFIX);
  }

  inline bool float_suffix () { return bool_features->contains (FLOAT_SUFFIX); }
  inline void support_float_suffix (bool param) {
    if (param) insert_bool_feature (FLOAT_SUFFIX);
    else       remove_bool_feature (FLOAT_SUFFIX);
  }

  inline bool i_suffix () { return bool_features->contains (I_SUFFIX); }
  inline void support_i_suffix (bool param) {
    if (param) insert_bool_feature (I_SUFFIX);
    else       remove_bool_feature (I_SUFFIX);
  }

  inline bool j_suffix () { return bool_features->contains (J_SUFFIX); }
  inline void support_j_suffix (bool param) {
    if (param) insert_bool_feature (J_SUFFIX);
    else       remove_bool_feature (J_SUFFIX);
  }
  
  inline bool long_suffix () { return bool_features->contains (LONG_SUFFIX); }
  inline void support_long_suffix (bool param) { 
    if (param) insert_bool_feature (LONG_SUFFIX);
    else       remove_bool_feature (LONG_SUFFIX);
  }

  inline bool ull_suffix () { return bool_features->contains (ULL_SUFFIX); }
  inline void support_ull_suffix (bool param) {
    if (param) insert_bool_feature (ULL_SUFFIX);
    else       remove_bool_feature (ULL_SUFFIX);
  }

  inline bool locase_i_suffix () { return bool_features->contains (LOCASE_I_SUFFIX); }
  inline void support_locase_i_suffix (bool param) {
    if (param) insert_bool_feature (LOCASE_I_SUFFIX);
    else       remove_bool_feature (LOCASE_I_SUFFIX);
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

  void use_cpp_style ();
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
  void parse_suffix (string s, int& pos);
  void parse_decimal (string s, int& pos);
};

#endif // defined NUMBER_PARSER_H
