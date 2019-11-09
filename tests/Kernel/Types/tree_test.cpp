
/******************************************************************************
* MODULE     : tree_test.cpp
* DESCRIPTION: Tests on tree
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "tree.hpp"

TEST (tree, is_atomic) {
  ASSERT_TRUE (is_atomic (tree ()));
}

TEST (tree, is_tuple) {
  ASSERT_TRUE (is_tuple (tuple ()));
  ASSERT_TRUE (is_tuple (tuple (tree ())));
  ASSERT_TRUE (is_tuple (tuple (tree (), tree ())));
  ASSERT_TRUE (is_tuple (tuple (tree (), tree (), tree ())));
  ASSERT_TRUE (is_tuple (tuple (tree (), tree (), tree (), tree ())));
  ASSERT_TRUE (is_tuple (tuple (tree (), tree (), tree (), tree (), tree ())));
}

TEST (tree, is_concat) {
  ASSERT_TRUE (is_concat (concat ()));
  ASSERT_TRUE (is_concat (concat (tree ())));
  ASSERT_TRUE (is_concat (concat (tree (), tree ())));
  ASSERT_TRUE (is_concat (concat (tree (), tree (), tree ())));
  ASSERT_TRUE (is_concat (concat (tree (), tree (), tree (), tree ())));
  ASSERT_TRUE (is_concat (concat (tree (), tree (), tree (), tree (), tree ())));
}
