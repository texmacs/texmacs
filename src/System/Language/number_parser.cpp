
/******************************************************************************
* MODULE     : number_parser.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "parser.hpp"
#include "analyze.hpp"

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

void
number_parser_rep::parse_suffix (string s, int& pos) {
  if (ull_suffix && pos<N(s)) {
    if (test (s, pos, "ull") || test (s, pos, "ULL")) { pos+= 3; return; }
    if (test (s, pos, "ll") || test (s, pos, "ul")
        || test(s, pos, "LL") || test (s, pos, "UL")) { pos+= 2; return; }
    if (s[pos] == 'l' || s[pos] == 'u'
        || s[pos] == 'L' || s[pos] == 'U') { pos+= 1; return; }
  }
  if (double_suffix && pos<N(s) && (s[pos] == 'd' || s[pos] == 'D')) {
    pos= pos+1;
    return;
  }
  if (float_suffix && pos<N(s) && (s[pos] == 'f' || s[pos] == 'F')) {
    pos= pos+1;
    return;
  }
  if (j_suffix && pos<N(s) && (s[pos] == 'j' || s[pos] == 'J')) {
    pos= pos+1;
    return;
  }
  if (long_suffix && pos<N(s) && (s[pos] == 'l' || s[pos] == 'L')) {
    pos= pos+1;
    return;
  }
  if (locase_i_suffix && pos<N(s) && s[pos] == 'i') {
    pos= pos+1;
    return;
  }
}

void
number_parser_rep::parse (string s, int& pos) {
  if (pos>=N(s)) return;

  if (!is_digit (s[pos]) &&
      !(s[pos] == '.' && pos+1 < N(s) && is_digit (s[pos+1])))
    return;

  // Start with 0b, 0o, 0x
  bool start_with_box = false;
  if (pos+2<N(s) && s[pos] == '0') {
    if (prefix_0x && (s[pos+1] == 'x' || s[pos+1] == 'X')) {
      pos+= 2;
      parse_hex (s, pos);
      if (no_suffix_with_box) return;
    }
    if (prefix_0o && (s[pos+1] == 'o' || s[pos+1] == 'O')) {
      pos+= 2;
      parse_octal (s, pos);
      if (no_suffix_with_box) return;
    }
    if (prefix_0b && (s[pos+1] == 'b' || s[pos+1] == 'B')) {
      pos+= 2;
      parse_binary (s, pos);
      if (no_suffix_with_box) return;
    }
  }

  parse_decimal (s, pos);
  if (scientific_notation && pos<N(s) && (s[pos] == 'e' || s[pos] == 'E')) {
    pos++;
    if (pos<N(s) && s[pos] == '-') pos++;
    parse_decimal (s, pos);
  }
  parse_suffix (s, pos);
}

void
number_parser_rep::use_cpp_style () {
  support_prefix_0x (true);
  support_ull_suffix (true);
  support_separator ('_');
  support_scientific_notation (true);
}

void
number_parser_rep::use_fortran_style () {
  support_scientific_notation (true);
}

void
number_parser_rep::use_java_style () {
  support_prefix_0x (true);
  support_prefix_0b (true);
  support_scientific_notation (true);
  support_separator ('_');
  support_long_suffix (true);
  support_double_suffix (true);
  support_float_suffix (true);
}

void
number_parser_rep::use_scala_style () {
  use_java_style ();
}

void
number_parser_rep::use_python_style () {
  support_j_suffix (true);
  support_scientific_notation (true);
  support_prefix_0b (true);
  support_prefix_0o (true);
  support_prefix_0x (true);
  support_no_suffix_with_box (true);
  support_separator ('_');
}

void
number_parser_rep::use_r_style () {
  support_long_suffix (true);
  support_locase_i_suffix (true);
  support_scientific_notation (true);
  support_prefix_0x (true);
}
