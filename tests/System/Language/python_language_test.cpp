
/******************************************************************************
* MODULE     : python_language_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "number_parser_test.hpp"
#include "parser.hpp"

TEST (integer_literal, work) {
  number_parser_rep number_parser;
  number_parser.use_python_style ();

  // Decimal Literals
  assert_numbers (number_parser, list<string>()
    * string("10") * string("10j") * string("10J")
    * string("10_10") * string("105_10")
  );

  // Octal Literals
  assert_numbers (number_parser, list<string>()
    * string("0o10") * string("0O10")
    * string("0o10_10") * string("0O10_10")
  );

  // Hex Literals
  assert_numbers (number_parser, hex_literals * hex_literals_with_sep);

  // Binary Literals
  assert_numbers (number_parser, binary_literals * binary_literals_with_sep);

  // No Suffix with B O X
  int pos= 0;
  number_parser.parse ("0x10j", pos);
  EXPECT_EQ (pos, 4);
  pos= 0;
  number_parser.parse ("0o10j", pos);
  EXPECT_EQ (pos, 4);
  pos= 0;
  number_parser.parse ("0b10j", pos);
  EXPECT_EQ (pos, 4);
}

TEST (floating_point_literal, work) {
  number_parser_rep number_parser;
  number_parser.use_python_style ();

  assert_numbers (number_parser, list<string>()
    * string("3.14") * string("10.") * string(".001")
    * string("1e100") * string("3.14e-10") * string("0e0")
    * string("3.14_15_93")
  );
}

TEST (imag_literal , work) {
  number_parser_rep number_parser;
  number_parser.use_python_style ();

  assert_numbers (number_parser, list<string>()
    * string("3.14j") * string("10.j") * string("10j")
    * string(".001j") * string("1e100j") * string("3.14e-10j")
    * string("3.14_15_93j")
  );
}