
/******************************************************************************
* MODULE     : cpp_language_test.cpp
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
  number_parser.use_cpp_style ();

  // Decimal Literals
  assert_numbers (number_parser, list<string>()
    * string("10") * string("10_10") * string("105_10")
    * string("10l") * string("10ll")
    * string("10L") * string("10LL")
    * string("10u") * string("10ul") * string("10ull")
    * string("10U") * string("10UL") * string("10ULL")
  );
}
