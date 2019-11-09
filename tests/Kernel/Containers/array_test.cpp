/******************************************************************************
* MODULE     : array_test.cpp
* DESCRIPTION: test on array
* COPYRIGHT  : (C) 2019  Yuhui Liu
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "gtest/gtest.h"

#include "array.hpp"
#include "string.hpp"

static array<int> gen_array(int n) {
  auto normal= array<int>();
  for (auto i=1;i<=n;i++){
      normal << i;
  }
  return normal;
}

auto zero_elem= array<int> ();
auto one_elem= append(1,zero_elem);
auto two_elem= array<int> (1,2);
auto three_elem= array<int> (1,2,3);
auto four_elem= array<int> (1,2,3,4);
auto five_elem= array<int> (1,2,3,4,5);

TEST (array, access) {
  EXPECT_EQ (five_elem[0], 1);
  EXPECT_EQ (five_elem[1], 2);
  EXPECT_EQ (five_elem[2], 3);
  EXPECT_EQ (five_elem[3], 4);
  EXPECT_EQ (five_elem[4], 5);
  EXPECT_EQ (one_elem[0], 1);
}

TEST (array, multiply) {
    auto mulu4= array<int> ();
    for (auto i=1; i<6; i++){
        mulu4 << (i * 4);
    }
    EXPECT_EQ (mulu4, five_elem * 4);

}

TEST (array, divide) {
    auto mulu4= array<int> ();
    for(auto i=1; i<6; i++){
        mulu4 << (i * 4);
    }
    EXPECT_EQ (mulu4 / 4, five_elem);
    
}

TEST (array, range) {
    EXPECT_EQ (range (gen_array (10),0,5), five_elem);
    EXPECT_EQ (range (five_elem,0,4), four_elem);
}

TEST (array, size) {
    EXPECT_EQ (N (zero_elem),0);
    EXPECT_EQ (N (one_elem),1);
    EXPECT_EQ (N (two_elem),2);
    EXPECT_EQ (N (three_elem),3);
    EXPECT_EQ (N (four_elem),4);
    EXPECT_EQ (N (five_elem),5);
    for (auto i=6; i<200; i++){
        auto array_test = gen_array (i);
        EXPECT_EQ ( N (array_test), i);
    }
}

TEST (array_test, copy) {
  EXPECT_EQ (copy (zero_elem), zero_elem);
  EXPECT_EQ (copy (one_elem), one_elem);
  EXPECT_EQ (copy (two_elem), two_elem);
  EXPECT_EQ (copy (three_elem), three_elem);
  EXPECT_EQ (copy (four_elem), four_elem);
  EXPECT_EQ (copy (five_elem),five_elem);
}

TEST (array, append) {
    auto zero_appended = append (1, zero_elem);
    EXPECT_EQ (zero_appended, one_elem);
    auto one_appended = append (one_elem, append (2, zero_elem));
    EXPECT_EQ (one_appended, two_elem);
    auto three_appended = append (three_elem, append (4, zero_elem));
    EXPECT_EQ (three_appended, four_elem);

    auto one2ten = gen_array (10);
    auto six2ten = array<int> (6,7,8,9,10);
    for (auto i=1; i<=10; i++){
        EXPECT_EQ (one2ten[i-1],i);
    }
    EXPECT_EQ (one2ten, append (five_elem, six2ten));
}

TEST (array, reverse) {
    EXPECT_EQ ( reverse (zero_elem), zero_elem);
    EXPECT_EQ ( reverse (one_elem), one_elem);
    auto rev_five = array<int> (5,4,3,2,1);
    EXPECT_EQ (reverse (five_elem), rev_five);

}

TEST (array, contains) {
  EXPECT_EQ (contains (1, zero_elem), false);
  EXPECT_EQ (contains (1, one_elem), true);
  EXPECT_EQ (contains (3, two_elem), false);
  EXPECT_EQ (contains (1, five_elem), true);
  EXPECT_EQ (contains (2, five_elem), true);
  EXPECT_EQ (contains (3, five_elem), true);
}
