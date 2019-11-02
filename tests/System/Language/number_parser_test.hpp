
/******************************************************************************
* MODULE     : python_language_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"
#include "parser.hpp"
#include "iterator.hpp"
#include "list.hpp"

#ifndef NUMBER_PARSER_TEST
#define NUMBER_PARSER_TEST

inline void assert_number_parser (number_parser_rep number_parser, string number) {
  int pos= 0;
  number_parser.parse (number, pos);
  EXPECT_EQ (pos, N(number));
}

inline void assert_numbers (number_parser_rep number_parser, list<string> numbers) {
  for (int i=0; i<N(numbers); i++) {
    assert_number_parser (number_parser, numbers[i]);
  }
}

#endif
