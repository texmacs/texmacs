
/******************************************************************************
* MODULE     : java_language_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "number_parser_test.hpp"
#include "number_parser.hpp"

TEST (integer_literal, work) {
  number_parser_rep number_parser;
  number_parser.use_java_style ();

  // Hex Literals
  assert_numbers (number_parser, hex_literals * hex_literals_with_sep);

  // Binary Literals
  assert_numbers (number_parser, binary_literals * binary_literals_with_sep);
}
