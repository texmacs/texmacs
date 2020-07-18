
/******************************************************************************
* MODULE     : number_parser.cpp
* DESCRIPTION: shared number parsing routines for various programming languages
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "number_parser.hpp"
#include "analyze.hpp"

number_parser_rep::number_parser_rep () {
  separator= '\0';
}

void
number_parser_rep::parse_binary (string s, int& pos) {
  while (pos<N(s) && (is_binary_digit (s[pos]) || is_separator (s[pos])))
    pos++;
}

void
number_parser_rep::parse_octal (string s, int& pos) {
  while (pos<N(s) && (is_octal_digit (s[pos]) || is_separator (s[pos])))
    pos++;
}

void
number_parser_rep::parse_hex (string s, int& pos) {
  while (pos<N(s) && (is_hex_digit (s[pos]) || is_separator (s[pos])))
    pos++;
}

void
number_parser_rep::parse_decimal (string s, int& pos) {
  while (pos<N(s) &&
         (is_digit (s[pos]) || is_separator (s[pos]) || s[pos] == '.'))
    pos++;
}

bool
number_parser_rep::can_parse_prefix_0b (string s, int pos) {
  return prefix_0b()
    && pos+2 < N(s)
    && s[pos] == '0'
    && (s[pos+1] == 'b' || s[pos+1] == 'B');
}

bool
number_parser_rep::can_parse_prefix_0o (string s, int pos) {
  return prefix_0o()
    && pos+2 < N(s)
    && s[pos] == '0'
    && (s[pos+1] == 'o' || s[pos+1] == 'O');
}

bool
number_parser_rep::can_parse_prefix_0x (string s, int pos) {
  return prefix_0x()
    && pos+2 < N(s)
    && s[pos] == '0'
    && (s[pos+1] == 'x' || s[pos+1] == 'X');
}

bool
number_parser_rep::can_parse (string s, int pos) {
  // check on len >= 3
  if (pos+2 < N(s)) {
    if (can_parse_prefix_0b (s, pos)
        || can_parse_prefix_0x (s, pos)
        || can_parse_prefix_0o (s, pos))
      return true;
  }
  // check on len >= 2
  if (pos+1 < N(s)) {
    if (s[pos] == '.' && is_digit (s[pos+1])) return true;
  }
  // finally, check on len >= 1
  return pos<N(s) && is_digit (s[pos]);
}

void
number_parser_rep::do_parse (string s, int& pos) {
  if (pos>=N(s)) return;

  if (!is_digit (s[pos]) &&
      !(s[pos] == '.' && pos+1 < N(s) && is_digit (s[pos+1])))
    return;

  // Start with 0b, 0o, 0x
  if (can_parse_prefix_0b (s, pos)) {
      pos+= 2;
      parse_binary (s, pos);
      if (no_suffix_with_box()) return;
  }
  if (can_parse_prefix_0o (s, pos)) {
      pos+= 2;
      parse_octal (s, pos);
      if (no_suffix_with_box()) return;
  }
  if (can_parse_prefix_0x (s, pos)) {
      pos+= 2;
      parse_hex (s, pos);
      if (no_suffix_with_box()) return;
  }

  parse_decimal (s, pos);
  if (scientific_notation() && pos<N(s) && (s[pos] == 'e' || s[pos] == 'E')) {
    pos++;
    if (pos<N(s) && s[pos] == '-') pos++;
    parse_decimal (s, pos);
  }
  suffix_parser.parse (s, pos);
}

void
number_parser_rep::use_cpp_style () {
  support_prefix_0x (true);
  // support_ull_suffix (true);
  support_separator ('_');
  support_scientific_notation (true);
}

void
number_parser_rep::use_fortran_style () {
  support_scientific_notation (true);
}

void
number_parser_rep::use_r_style () {
  // support_long_suffix (true);
  // support_locase_i_suffix (true);
  support_scientific_notation (true);
  support_prefix_0x (true);
}
