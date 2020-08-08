
/******************************************************************************
* MODULE     : hashset_test.cpp
* DESCRIPTION: test on hashset 
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"
#include "hashset.hpp"
#include "string.hpp"

/******************************************************************************
* tests on contains 
******************************************************************************/
TEST (hashset, contains) {
  auto set= hashset<string>();
  set->insert("Hello");
  EXPECT_EQ (set->contains("Hello"), true);
  EXPECT_EQ (set->contains("hello"), false);
}

TEST (hashset, init) {
  auto set= hashset<string>();
  set << string("hello") << string("world");
  EXPECT_EQ (set->contains("hello"), true);
  EXPECT_EQ (set->contains("world"), true);
  EXPECT_EQ (N(set), 2);
}
