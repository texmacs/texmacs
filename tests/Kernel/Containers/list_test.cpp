/******************************************************************************
* MODULE     : list_test.cpp
* DESCRIPTION: test on linked lists with reference counting
* COPYRIGHT  : (C) 2018 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "gtest/gtest.h"

#include "list.hpp"

static list<long> gen(int64_t n) {
  auto normal = list<long>();
  for (long i=0; i<n; i++)
    normal << i;
  return normal;
}

auto the_nil_list = list<string>();
auto the_atom_list = list<long>(1);
auto normal = list<long>(1, 2, 3, list<long>());

TEST (list, is_nil) {
  EXPECT_EQ (is_nil(the_nil_list), true);
  EXPECT_EQ (is_nil(the_atom_list), false);
}

TEST (list, is_atom) {
  EXPECT_EQ (is_atom(the_nil_list), false);
  EXPECT_EQ (is_atom(the_atom_list), true);
}

/******************************************************************************
* tests on output and convertion
******************************************************************************/

TEST (list, access) {
  EXPECT_ANY_THROW (the_nil_list[0]);

  EXPECT_EQ (the_atom_list[0], 1L);

  EXPECT_EQ (normal[0], 1);
  EXPECT_EQ (normal[1], 2);
  EXPECT_EQ (normal[2], 3);
}

/******************************************************************************
* tests on insertion and suppression
******************************************************************************/

TEST (list, operate_on_the_last) {
  EXPECT_ANY_THROW (access_last(the_nil_list));
  EXPECT_ANY_THROW (suppress_last(the_nil_list));
  EXPECT_ANY_THROW (last_item(the_nil_list));

  auto the_atom_list_copy = copy(the_atom_list);
  access_last(the_atom_list_copy) = 2L;
  EXPECT_EQ (the_atom_list_copy, list<long>(2L));
  suppress_last (the_atom_list_copy);
  EXPECT_EQ (the_atom_list_copy, list<long>());
  EXPECT_EQ (last_item(the_atom_list), 1);

  auto normal_copy = copy(normal);
  access_last (normal_copy) = 4;
  EXPECT_EQ (normal_copy, list<long>(1, 2, 4, list<long>()));
  suppress_last (normal_copy);
  EXPECT_EQ (normal_copy, list<long>(1, 2, list<long>()));
  EXPECT_EQ (last_item(normal), 3);
}

/******************************************************************************
* tests on computations with list<T> structures
******************************************************************************/
TEST (list, size) {
  EXPECT_EQ (N (the_nil_list), 0);
  EXPECT_EQ (N (the_atom_list), 1);
  EXPECT_EQ (N (normal), 3);
  for (auto i=4; i<=100; i++) {
    auto list = gen (i);
    EXPECT_EQ (N (list), i);
  }
}

TEST (list, copy) {
  EXPECT_EQ (copy(the_nil_list), the_nil_list);
  EXPECT_EQ (copy(the_atom_list), the_atom_list);
  EXPECT_EQ (copy(normal), normal);
}

TEST (list, append) {
  auto appended = the_nil_list * string("a");
  EXPECT_EQ (appended, list<string>(string("a")));

  auto to_append = list<string>("a");
  EXPECT_EQ (the_nil_list * to_append, to_append);

  auto nil_to_append = list<string>();
  EXPECT_EQ (the_nil_list * nil_to_append, nil_to_append);

  EXPECT_EQ (the_atom_list * list<long>(), the_atom_list);

  EXPECT_EQ (list<long>(1L, 2L, list<long>()) * list<long>(3L), normal);
  EXPECT_EQ (list<long>(1L) * list<long>(2L, 3L, list<long>()), normal);
}

TEST (list, head_and_tail) {
  EXPECT_ANY_THROW (head(the_nil_list));
  EXPECT_ANY_THROW (tail(the_nil_list));

  EXPECT_EQ (head(the_atom_list), the_atom_list);
  EXPECT_EQ (tail(the_atom_list), list<long>());

  EXPECT_EQ (head(normal), list<long>(1));
  EXPECT_EQ (tail(normal), list<long>(2, 3, list<long>()));
}

TEST (list, reverse) {
  EXPECT_EQ (reverse(the_nil_list), the_nil_list);
  EXPECT_EQ (reverse(the_atom_list), the_atom_list);
  EXPECT_EQ (reverse(normal), list<long>(3, 2, 1, list<long>()));
}

TEST (list, remove) {
  EXPECT_EQ (remove(the_nil_list, string("a")), the_nil_list);
  EXPECT_EQ (remove(the_atom_list, 1L), list<long>());
  EXPECT_EQ (remove (normal, 2L), list<long>(1, 3, list<long>()));
}

TEST (list, contains) {
  EXPECT_EQ (contains(the_nil_list, string("a")), false);
  EXPECT_EQ (contains(the_atom_list, 1L), true);
  EXPECT_EQ (contains(normal, 1L), true);
  EXPECT_EQ (contains(normal, 2L), true);
  EXPECT_EQ (contains(normal, 3L), true);
}