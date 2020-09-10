
/******************************************************************************
* MODULE     : escaped_char_parser.cpp
* DESCRIPTION: shared escaped characters parsing routines
* COPYRIGHT  : (C) 2019-2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "escaped_char_parser.hpp"
#include "analyze.hpp"

escaped_char_parser_rep::escaped_char_parser_rep ():
  HEX_WITH_8_BITS ("hex_with_8_bits"),
  HEX_WITH_16_BITS ("hex_with_16_bits"),
  HEX_WITH_32_BITS ("hex_with_32_bits"),
  OCTAL_UPTO_3_DIGITS ("octal_upto_3_digits")
{
  m_chars= array<char>();
  m_strings= array<string>();
  m_escape= '\\';
}

void
escaped_char_parser_rep::set_chars (array<char> p_chars) {
  m_chars= p_chars;
}

void
escaped_char_parser_rep::set_sequences (array<string> p_strings) {
  for (int i=0; i<N(p_strings); i++) {
    string str= p_strings[i];
    if (N(str) == 1) {
      m_chars << str[0];
    } else {
      m_strings << str;
    }
  }
}

void
escaped_char_parser_rep::set_escape(char p_escape) {
  m_escape= p_escape;
}

bool
escaped_char_parser_rep::can_parse_hex_with_8_bits (string s, int pos) {
  int remaining= N(s) - pos;
  return hex_with_8_bits()
    && s[pos] == 'x'
    && remaining >= 3
    && is_hex_digit (s[pos+1])
    && is_hex_digit (s[pos+2]);
}

bool
escaped_char_parser_rep::can_parse_hex_with_16_bits (string s, int pos) {
  int remaining= N(s) - pos;
  return hex_with_16_bits()
    && s[pos] == 'u'
    && remaining >= 5
    && is_hex_digit (s[pos+1])
    && is_hex_digit (s[pos+2])
    && is_hex_digit (s[pos+3])
    && is_hex_digit (s[pos+4]);
}

bool
escaped_char_parser_rep::can_parse_hex_with_32_bits (string s, int pos) {
  int remaining= N(s) - pos;
  return hex_with_32_bits()
    && s[pos] == 'U'
    && remaining >= 9
    && is_hex_digit (s[pos+1])
    && is_hex_digit (s[pos+2])
    && is_hex_digit (s[pos+3])
    && is_hex_digit (s[pos+4])
    && is_hex_digit (s[pos+5])
    && is_hex_digit (s[pos+6])
    && is_hex_digit (s[pos+7])
    && is_hex_digit (s[pos+8]);
}

bool
escaped_char_parser_rep::can_parse_octal_upto_3_digits (string s, int pos) {
  int remaining= N(s)-pos;
  return octal_upto_3_digits()
    && remaining >= 1
    && is_octal_digit (s[pos+1]);
}

bool
escaped_char_parser_rep::can_parse (string s, int pos) {
  int remaining= N(s) - pos;
  if (remaining <= 1) return false;
  if (s[pos] != m_escape) return false;

  if (can_parse_hex_with_8_bits (s, pos+1)) return true;
  if (can_parse_hex_with_16_bits (s, pos+1)) return true;
  if (can_parse_hex_with_32_bits (s, pos+1)) return true;

  for (int i=0; i<N(m_strings); i++) {
    string m_string= m_strings[i];
    if (test (s, pos+1, m_string)) return true;
  }
  return contains (s[pos+1], m_chars);
}

string
escaped_char_parser_rep::to_string () {
  string ret= parser_rep::to_string ();
  ret << "  escape_sequences:" << "\n";
  for (int i=0; i<N(m_chars); i++) {
    ret << "    - " << m_chars[i] << "\n";
  }
  for (int i=0; i<N(m_strings); i++) {
    ret << "    - " << m_strings[i] << "\n";
  }
  return ret;
}

void
escaped_char_parser_rep::do_parse (string s, int& pos) {
  int remaining= N(s) - pos;
  if (remaining <= 1) return;
  if (s[pos] != m_escape) return;

  if (can_parse_hex_with_8_bits (s, pos+1)) {
    pos= pos+4;
    return;
  }
  if (can_parse_hex_with_16_bits (s, pos+1)) {
    pos= pos+6;
    return;
  }
  if (can_parse_hex_with_32_bits (s, pos+1)) {
    pos= pos+10;
    return;
  }
  if (can_parse_octal_upto_3_digits (s, pos+1)) {
    pos= pos+1;
    int count= 3;
    while (pos<N(s) && count>0 && is_octal_digit (s[pos])) {
      count= count-1;
      pos= pos+1;
    }
    return;
  }

  for (int i=0; i<N(m_strings); i++) {
    string m_string= m_strings[i];
    if (test (s, pos+1, m_string)) {
      pos= pos+N(m_string);
      return;
    }
  }

  if (contains (s[pos+1], m_chars))
    pos= pos+2;
}
