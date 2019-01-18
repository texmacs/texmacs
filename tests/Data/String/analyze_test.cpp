
/******************************************************************************
* MODULE     : analyze_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "analyze.hpp"

TEST (string, locase_all) {
  ASSERT_TRUE (locase_all (string ("true")) == string ("true"));
  ASSERT_TRUE (locase_all (string ("TRue")) == string ("true"));
  ASSERT_TRUE (locase_all (string ("TRUE")) == string ("true"));
  ASSERT_TRUE (locase_all (string ("123TRUE")) == string ("123true"));
}

TEST (string, upcase_all) {
  ASSERT_TRUE (upcase_all (string ("true")) == string ("TRUE"));
  ASSERT_TRUE (upcase_all (string ("TRue")) == string ("TRUE"));
  ASSERT_TRUE (upcase_all (string ("TRUE")) == string ("TRUE"));
  ASSERT_TRUE (upcase_all (string ("123true")) == string ("123TRUE"));
}
