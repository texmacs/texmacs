#include "gtest/gtest.h"

#include "list.hpp"

TEST(N, the_lengh_of_the_list) {
  EXPECT_EQ(0, N(list<string>()));
  EXPECT_EQ(1, N(list<string>("a")));
}