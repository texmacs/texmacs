
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

TEST (string, is_alpha) {
  for (unsigned char c=0; c<255; c++) {
    if ((c>=65 && c<=90) || (c>=97 && c<=122)) {
      ASSERT_TRUE (is_alpha (c));
    } else {
      ASSERT_FALSE (is_alpha (c));
    }
  }
}

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

TEST (string, string_minus) {
  ASSERT_TRUE (string_minus ("Hello World", "eo") == string ("Hll Wrld"));
  ASSERT_TRUE (string_minus ("", "abc") == string (""));
  ASSERT_TRUE (string_minus ("abc", "") == string ("abc"));
}

TEST (string, string_union) {
  ASSERT_TRUE (string_union ("abc", "") == string("abc"));
  ASSERT_TRUE (string_union ("", "abc") == string("abc"));
  ASSERT_TRUE (string_union ("Hello World", "eo") == string ("Hll Wrldeo"));
}

TEST (string, starts) {
  ASSERT_TRUE (starts ("abc_def", "abc"));
  ASSERT_FALSE (starts ("abc_def", "def"));
  ASSERT_TRUE (starts ("abc", ""));
  ASSERT_TRUE (starts ("", ""));
}

TEST (string, ends) {
  ASSERT_TRUE (ends ("abc_def", "def"));
  ASSERT_TRUE (ends ("abc_def", ""));
  ASSERT_FALSE (ends ("abc_def", "de"));
}
