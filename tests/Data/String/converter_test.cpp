/******************************************************************************
* MODULE     : converter_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "converter.hpp"

TEST (string, utf8_to_cork) {
  ASSERT_STREQ (as_charp (utf8_to_cork ("中")), "<#4E2D>");
  ASSERT_STREQ (as_charp (utf8_to_cork ("“")), "\x10");
  ASSERT_STREQ (as_charp (utf8_to_cork("”")), "\x11");
}