#include "gtest/gtest.h"

#include "list.hpp"

TEST (a_nil_list, is_a_nil_list) {
  auto the_nil_list = list<string>();

  EXPECT_EQ (is_nil(the_nil_list), true);
  EXPECT_EQ (is_atom(the_nil_list), false);
  EXPECT_ANY_THROW (head(the_nil_list));
  EXPECT_ANY_THROW (tail(the_nil_list));
  EXPECT_EQ (N(the_nil_list), 0);
  EXPECT_EQ (is_nil(copy(the_nil_list)), true);
  EXPECT_EQ (is_nil(reverse(the_nil_list)), true);

  auto appended = the_nil_list * string("a");
  EXPECT_EQ (appended, list<string>(string("a")));

  auto to_append = list<string>("a");
  EXPECT_EQ (the_nil_list * to_append, to_append);

  auto nil_to_append = list<string>();
  EXPECT_EQ (the_nil_list * nil_to_append, nil_to_append);
}

TEST (an_atom_list, is_an_atom_list) {
  auto the_atom_list = list<long>(1);

  EXPECT_EQ (is_nil(the_atom_list), false);
  EXPECT_EQ (is_atom(the_atom_list), true);
  EXPECT_EQ (head(the_atom_list), the_atom_list);
  EXPECT_EQ (tail(the_atom_list), list<long>());
  EXPECT_EQ (N(the_atom_list), 1);
  EXPECT_EQ (copy(the_atom_list), the_atom_list);
  EXPECT_EQ (reverse(the_atom_list), the_atom_list);
  EXPECT_EQ (the_atom_list * list<long>(), the_atom_list);
}