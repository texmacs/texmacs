#include "gtest/gtest.h"

#include "string.hpp"

auto empty_str = string();

TEST (the_size_of_empty_string, eq_0) {
  EXPECT_EQ(N(empty_str), 0);
}