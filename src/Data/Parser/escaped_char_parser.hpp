
/******************************************************************************
* MODULE     : escaped_char_parser.hpp
* DESCRIPTION: shared escaped characters parsing routines
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ESCAPED_CHAR_PARSER_H
#define ESCAPED_CHAR_PARSER_H

#include "parser.hpp"
#include "list.hpp"

class escaped_char_parser_rep : public parser_rep {
public:
  escaped_char_parser_rep ();
  string get_parser_name () { return "blanks_parser"; }
  bool can_parse (string s, int pos);

  void set_chars (list<char> p_chars);
  void set_escape (char p_escape);
  void set_strings (list<string> p_strings);

  inline void support_hex_with_8_bits (bool param) { hex_with_8_bits= param; };
  inline void support_hex_with_16_bits (bool param) { hex_with_16_bits= param; };
  inline void support_hex_with_32_bits (bool param) { hex_with_32_bits= param; };
  inline void support_octal_upto_3_digits (bool param) { octal_upto_3_digits= param; };

private:
  list<char> m_chars;
  list<string> m_strings;
  char m_escape;

  bool hex_with_8_bits;
  bool hex_with_16_bits;
  bool hex_with_32_bits;
  bool octal_upto_3_digits;

  void do_parse (string s, int& pos);

  bool can_parse_hex_with_8_bits (string s, int pos);
  bool can_parse_hex_with_16_bits (string s, int pos);
  bool can_parse_hex_with_32_bits (string s, int pos);
  bool can_parse_octal_upto_3_digits (string s, int pos);
};

#endif // ESCAPED_CHAR_PARSER_H
