
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
      while (pos<N(s) && (is_hex_digit (s[pos]) || is_separator (s[pos])))
        pos++;
      return;
    }
    if (prefix_0o && (s[pos+1] == 'o' || s[pos+1] == 'O')) {
      pos+= 2;
      while (pos<N(s) && (is_octal_digit (s[pos]) || is_separator (s[pos])))
        pos++;
      return;
    }
    if (prefix_0b && (s[pos+1] == 'b' || s[pos+1] == 'B')) {
      pos+= 2;
      while (pos<N(s) && (is_binary_digit (s[pos]) || is_separator (s[pos]))) pos++;
      return;
    }
  }

  int i= pos;
  while (i<N(s) && (is_digit (s[i]) || is_separator (s[i]) || s[i] == '.'))
    i++;
  if (i == pos) return;
  if (scientific_notation && i<N(s) && (s[i] == 'e' || s[i] == 'E')) {
    i++;
    if (i<N(s) && s[i] == '-') i++;
    while (i<N(s) && (is_digit (s[i]) || is_separator (s[i]) || s[i] == '.')) i++;
  }
  pos= i;

  if (double_suffix && i<N(s) && (s[i] == 'd' || s[i] == 'D')) {
    pos= i+1;
    return;
  }
  if (float_suffix && i<N(s) && (s[i] == 'f' || s[i] == 'F')) {
    pos= i+1;
    return;
  }
  if (j_suffix && i<N(s) && (s[i] == 'j' || s[i] == 'J')) {
    pos= i+1;
    return;
  }
  if (long_suffix && i<N(s) && (s[i] == 'l' || s[i] == 'L')) {
    pos= i+1;
    return;
  }
  if (locase_i_suffix && i<N(s) && s[i] == 'i') {
    pos= i+1;
    return;
  }
}

void
number_parser_rep::use_cpp_style () {
  support_prefix_0x (true);
  support_scientific_notation (true);
}

void
number_parser_rep::use_fortran_style () {
  support_scientific_notation (true);
}

void
number_parser_rep::use_java_style () {
  support_prefix_0x (true);
  support_scientific_notation (true);
  support_long_suffix (true);
  support_double_suffix (true);
  support_float_suffix (true);
}

void
number_parser_rep::use_scala_style () {
  support_prefix_0x (true);
  support_scientific_notation (true);
  support_double_suffix (true);
  support_float_suffix (true);
  support_long_suffix (true);
}

void
number_parser_rep::use_python_style () {
  support_j_suffix (true);
  support_scientific_notation (true);
  support_prefix_0b (true);
  support_prefix_0o (true);
  support_prefix_0x (true);
  support_separator ('_');
}

void
number_parser_rep::use_r_style () {
  support_long_suffix (true);
  support_locase_i_suffix (true);
  support_scientific_notation (true);
  support_prefix_0x (true);
}
