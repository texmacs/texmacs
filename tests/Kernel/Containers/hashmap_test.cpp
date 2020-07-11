
/******************************************************************************
* MODULE     : hashmap_test.cpp
* DESCRIPTION: test on hashmap 
* COPYRIGHT  : (C) 2018 Xikai Wei 
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"
#include "hashmap.hpp"

/******************************************************************************
* tests on resize
******************************************************************************/
TEST (hashmap, resize) {
    auto hm = hashmap<int, int>(0, 10);
    hm(1) = 10;
    hm(2) = 20;

    hm->resize(1);
    EXPECT_EQ (hm[1] == 10, true);
    EXPECT_EQ (hm[2] == 20, true);

    hm->resize(20);
    EXPECT_EQ (hm[1] == 10, true);
    EXPECT_EQ (hm[2] == 20, true);
}

/******************************************************************************
* tests on reset 
******************************************************************************/
TEST (hashmap, reset) {
    auto hm = hashmap<int, int>(0, 10);
    hm(1) = 10;
    hm(11) = 20;
    hm->reset(1);

    EXPECT_EQ (hm->contains(1), false);
    EXPECT_EQ (hm->contains(11), true);
}


/******************************************************************************
* tests on generate 
******************************************************************************/
auto hm_generate = hashmap<int, int>(0, 10);
void routine(int key) {
    EXPECT_EQ (hm_generate->contains(key), true);
}

TEST (hashmap, generate) {
    hm_generate(1) = 10;
    hm_generate(2) = 20;
    hm_generate->generate(routine);
}

/******************************************************************************
* tests on contains 
******************************************************************************/
TEST (hashmap, contains) {
    auto hm = hashmap<int, void*>(nullptr, 2, 2);
    hm(1) = nullptr;
    EXPECT_EQ (hm->contains(1), true);
    EXPECT_EQ (hm->contains(3), false);
} 


/******************************************************************************
* tests on empty 
******************************************************************************/
TEST (hashmap, empty) {
    auto hm = hashmap<int, int>();
    EXPECT_EQ (hm->empty(), true);
    
    hm(1);
    EXPECT_EQ (hm->empty(), false);
}

/******************************************************************************
* tests on join 
******************************************************************************/
TEST (hashmap, join) {
    auto hm1 = hashmap<int, int>();
    auto hm2 = hashmap<int, int>();
    hm1(1) = 10;
    hm1(2) = 20;
    hm2(2) = -20;
    hm2(3) = -30;
    hm1->join(hm2);

    EXPECT_EQ (hm1[1] == 10, true);
    EXPECT_EQ (hm1[2] == -20, true);
    EXPECT_EQ (hm1[3] == -30, true);
}

/******************************************************************************
* tests on write_back 
******************************************************************************/
TEST (hashmap, write_back) {
    auto hm1 = hashmap<int, int>(0, 10);
    auto hm2 = hashmap<int, int>(0, 10);
    hm1(1) = 10;
    hm1(2) = 20;
    hm2(2) = -20;

    hm1->write_back(2, hm2);
    EXPECT_EQ (hm1[2] == 20, true);

    hm1->write_back(3, hm2);
    EXPECT_EQ (hm1[3] == 0, true);

    hm2(4) = -40;
    hm1->write_back(4, hm2);
    EXPECT_EQ (hm2[4] == -40, true);
}

/******************************************************************************
* tests on pre_patch 
******************************************************************************/
TEST (hashmap, pre_patch) {
    auto hm = hashmap<int, int>();
    auto hm_patch = hashmap<int, int>();
    auto hm_base = hashmap<int, int>();

    hm(1) = 10;
    hm_patch(1);
    hm->pre_patch(hm_patch, hm_base);
    EXPECT_EQ (hm[1] == 10, true);

    hm(2) = 20;
    hm_patch(2) = -20;
    hm_base(2) = 20;
    hm->pre_patch(hm_patch, hm_base);
    EXPECT_EQ (hm[2] == 0, true);

    hm_patch(3) = -30;
    hm->pre_patch(hm_patch, hm_base);
    EXPECT_EQ (hm[3] == -30, true);
}

/******************************************************************************
* tests on post_patch 
******************************************************************************/
TEST (hashmap, post_patch) {
    auto hm = hashmap<int, int>();
    auto hm_patch = hashmap<int, int>();
    auto hm_base = hashmap<int, int>();

    hm(1) = 10;
    hm_patch(1);
    hm->post_patch(hm_patch, hm_base);
    EXPECT_EQ (hm[1] == 0, true);

    hm_patch(2) = -20;
    hm->pre_patch(hm_patch, hm_base);
    EXPECT_EQ (hm[2] == -20, true);
}

/******************************************************************************
* tests on copy 
******************************************************************************/
TEST (hashmap, copy) {
    auto hm = hashmap<int, int>();
    auto hm_c = hashmap<int, int>(0, 10, 2);
    hm_c(1) = 10;
    hm_c(11) = 110;
    hm_c(2) = 20;

    auto res_hm = copy(hm_c);
    EXPECT_EQ (res_hm[1] == 10, true);
    EXPECT_EQ (res_hm[11] == 110, true);
    EXPECT_EQ (res_hm[2] == 20, true);
}

/******************************************************************************
* tests on equality 
******************************************************************************/
TEST (hashmap, equality) {
    auto hm1 = hashmap<int, int>(0, 10, 3);
    auto hm2 = hashmap<int, int>(0, 100, 30);
    hm1(1) = 10;
    hm2(1) = 10;
    EXPECT_EQ (hm1 == hm2, true);

    hm2(2) = 20;
    EXPECT_EQ (hm1 != hm2, true);
}

/******************************************************************************
* tests on changes 
******************************************************************************/
TEST (hashmap, changes) {
  auto base_m = hashmap<int, int>();
  auto patch_m = hashmap<int, int>();
  base_m(1) = 10;
  base_m(2) = 20;
  patch_m(2) = -20;
  patch_m(3) = -30;
  auto res = changes(patch_m, base_m);
  EXPECT_EQ (N(res) == 2, true);
  EXPECT_EQ (res[2] == -20, true);
  EXPECT_EQ (res[3] == -30, true);
}


/******************************************************************************
* tests on invert 
******************************************************************************/
TEST (hashmap, invert) {
  auto base_m = hashmap<int, int>();
  auto patch_m = hashmap<int, int>();
  base_m(1) = 10;
  base_m(2) = 20;
  patch_m(2) = -20;
  patch_m(3) = -30;
  auto res = invert(patch_m, base_m);
  EXPECT_EQ (N(res) == 2, true);
  EXPECT_EQ (res[2] == 20, true);
  EXPECT_EQ (res[3] == 0, true);
}


/******************************************************************************
* tests on N 
******************************************************************************/
TEST (hashmap, N) {
  auto empty_hm = hashmap<int, void*>();
  EXPECT_EQ (N(empty_hm) == 0, true);

  auto non_empty_hm = hashmap<int, void*>();
  non_empty_hm(1) = nullptr;
  EXPECT_EQ (N(non_empty_hm) == 1, true);
}
