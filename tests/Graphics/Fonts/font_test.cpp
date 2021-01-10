/******************************************************************************
* MODULE     : font_test.cpp
* DESCRIPTION: tests on font
* COPYRIGHT  : (C) 2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"
#include "font.hpp"

TEST(font, default_chinese_font_name) {
  ASSERT_TRUE (default_chinese_font_name () == string ("Songti SC"));
}
