
/******************************************************************************
* MODULE     : escaped_char_parser.hpp
* DESCRIPTION: shared escaped characters parsing routines
* COPYRIGHT  : (C) 2019-2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ESCAPED_CHAR_PARSER_H
#define ESCAPED_CHAR_PARSER_H

#include "parser.hpp"
#include "array.hpp"
#include "hashset.hpp"

class escaped_char_parser_rep : public parser_rep {
public:
  escaped_char_parser_rep ();
  string get_parser_name () { return "escaped_char_parser"; }
  bool can_parse (string s, int pos);
  string to_string ();

  string HEX_WITH_8_BITS= "hex_with_8_bits";
  string HEX_WITH_16_BITS= "hex_with_16_bits";
  string HEX_WITH_32_BITS= "hex_with_32_bits";
  string OCTAL_UPTO_3_DIGITS= "octal_upto_3_digits";

  inline void insert_bool_feature (string feature) {
    bool_features->insert (feature);
  }
  inline void remove_bool_feature (string feature) {
    bool_features->remove (feature);
  }

  inline bool hex_with_8_bits () { return bool_features->contains (HEX_WITH_8_BITS); }
  inline void support_hex_with_8_bits (bool param) {
    if (param) insert_bool_feature (HEX_WITH_8_BITS);
    else       remove_bool_feature (HEX_WITH_8_BITS);
  }

  inline bool hex_with_16_bits () { return bool_features->contains (HEX_WITH_16_BITS); }
  inline void support_hex_with_16_bits (bool param) {
    if (param) insert_bool_feature (HEX_WITH_16_BITS);
    else       remove_bool_feature (HEX_WITH_16_BITS);
  }

  inline bool hex_with_32_bits () { return bool_features->contains (HEX_WITH_32_BITS); }
  inline void support_hex_with_32_bits (bool param) {
    if (param) insert_bool_feature (HEX_WITH_32_BITS);
    else       remove_bool_feature (HEX_WITH_32_BITS);
  }

  inline bool octal_upto_3_digits () { return bool_features->contains (OCTAL_UPTO_3_DIGITS); }
  inline void support_octal_upto_3_digits (bool param) {
    if (param) insert_bool_feature (OCTAL_UPTO_3_DIGITS);
    else       remove_bool_feature (OCTAL_UPTO_3_DIGITS);
  }

  void set_chars (array<char> p_chars);
  void set_escape (char p_escape);
  void set_sequences (array<string> p_strings);

private:
  array<char> m_chars;
  array<string> m_strings;
  char m_escape;

  hashset<string> bool_features;

  void do_parse (string s, int& pos);

  bool can_parse_hex_with_8_bits (string s, int pos);
  bool can_parse_hex_with_16_bits (string s, int pos);
  bool can_parse_hex_with_32_bits (string s, int pos);
  bool can_parse_octal_upto_3_digits (string s, int pos);
};

#endif // ESCAPED_CHAR_PARSER_H
