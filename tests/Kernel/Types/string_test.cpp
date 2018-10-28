
/******************************************************************************
* MODULE     : string_test.cpp
* DESCRIPTION: Fixed size strings with reference counting and
*              pointer copying. Zero-characters are allowed in strings.
* COPYRIGHT  : (C) 2018 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "string.hpp"

/******************************************************************************
* Tests on Common routines for strings
******************************************************************************/

TEST (string, equality) {
  EXPECT_EQ (string("abc") == "abc", true);
  EXPECT_EQ (string("abc") == "", false);

  EXPECT_EQ (string("abc") != "abc", false);
  EXPECT_EQ (string("abc") != "", true);

  EXPECT_EQ (string("abc") == string("abc"), true);
  EXPECT_EQ (string("abc") == string(), false);
  EXPECT_EQ (string("abc") != string("abc"), false);
  EXPECT_EQ (string("abc") != string(), true);
}

TEST (string, ltr) {

}

TEST (string, slice) {
  ASSERT_TRUE (string("abcde")(0, 0) == string());
  ASSERT_TRUE (string("abcde")(0, 1) == string("a"));
  ASSERT_TRUE (string("abcde")(0, 10) == string("abcde"));
  ASSERT_TRUE (string("abcde")(-1, 1) == string("a"));
  ASSERT_TRUE (string("abcde")(3, 2) == string());
  ASSERT_TRUE (string("abcde")(3, -2) == string());
  ASSERT_TRUE (string("abcde")(10, 11) == string());
  ASSERT_TRUE (string("abcde")(-3, -2) == string());
}

TEST (string, append) {
  ASSERT_TRUE (string("abc") * "de" == string("abcde"));
  ASSERT_TRUE (string("abc") * string("de") == string("abcde"));
  ASSERT_TRUE ("abc" * string("de") == string("abcde"));
}

/******************************************************************************
* Modifications
******************************************************************************/


/******************************************************************************
* Conversions
******************************************************************************/
TEST (string, to_lower) {
  ASSERT_TRUE (to_lower(string("true")) == string("true"));
  ASSERT_TRUE (to_lower(string("TRue")) == string("true"));
  ASSERT_TRUE (to_lower(string("TRUE")) == string("true"));
  ASSERT_TRUE (to_lower(string("123TRUE")) == string("123true"));
}

TEST (string, to_upper) {
  ASSERT_TRUE (to_upper(string("true")) == string("TRUE"));
  ASSERT_TRUE (to_upper(string("TRue")) == string("TRUE"));
  ASSERT_TRUE (to_upper(string("TRUE")) == string("TRUE"));
  ASSERT_TRUE (to_upper(string("123true")) == string("123TRUE"));
}

TEST (string, as_bool) {
  EXPECT_EQ (as_bool(string("true")), true);
  EXPECT_EQ (as_bool(string("#t")), true);
  EXPECT_EQ (as_bool(string("false")), false);

  // implicit conversion from char*
  ASSERT_TRUE (as_bool("true"));
  ASSERT_TRUE (as_bool("#t"));
  ASSERT_FALSE (as_bool("false"));
}

TEST (string, as_string_bool) {
  ASSERT_TRUE (as_string_bool(true) == string("true"));
  ASSERT_TRUE (as_string_bool(false) == string("false"));
}


/******************************************************************************
* Predicates
******************************************************************************/
TEST (string, is_bool) {
  ASSERT_TRUE (is_bool ("true"));
  ASSERT_TRUE (is_bool ("false"));
  ASSERT_TRUE (is_bool (string ("true")));
  ASSERT_TRUE (is_bool (string ("false")));

  ASSERT_FALSE (is_bool ("100"));
  ASSERT_FALSE (is_bool ("nil"));
}

TEST (string, is_quoted) {
  ASSERT_TRUE (is_quoted ("\"\""));
  ASSERT_TRUE (is_quoted ("\"Hello TeXmacs\""));
  // is_quoted only checks if a string starts with a double quote
  // and ends with another double quote, regardless the validity
  // of the raw string
  ASSERT_TRUE (is_quoted ("\"Hello\"TeXmacs\""));

  ASSERT_FALSE (is_quoted ("\""));
  ASSERT_FALSE (is_quoted ("A"));
  ASSERT_FALSE (is_quoted ("9"));
  ASSERT_FALSE (is_quoted ("Hello TeXmacs"));
  ASSERT_FALSE (is_quoted ("\"Hello TeXmac\"s"));
  ASSERT_FALSE (is_quoted ("H\"ello TeXmacs\""));
}
