
/******************************************************************************
* MODULE     : url_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "file.hpp"

TEST (url, exist) {
  // two cases: root directory
  // TODO: Windows compatibility
  url root_tmp = url ("/tmp");
  url root_no_such_tmp = url ("/no_such_tmp");
  ASSERT_TRUE (exists (root_tmp));
  ASSERT_FALSE (exists (root_no_such_tmp));
}

TEST (url, suffix) {
  // empty suffix should work
  url no_suffix = url ("/a/b/c/d/no_suffix");
  EXPECT_EQ (suffix (no_suffix), string (""));
  url no_suffix2 = url ("/a/b.c/d/no_suffix");
  EXPECT_EQ (suffix (no_suffix2), string (""));

  // normal suffix should work
  url png = url ("/a/b/c/d.png");
  EXPECT_EQ (suffix (png), string ("png"));
  url png2 = url ("/a/b.c/d.png");
  EXPECT_EQ (suffix (png2), string ("png"));
}
