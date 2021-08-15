
/******************************************************************************
* MODULE     : hashmap_test.cpp
* DESCRIPTION: test on hashmap 
* COPYRIGHT  : (C) 2018-2021  Xikai Wei, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "hashmap.hpp"

class TestHashmap: public QObject {
  Q_OBJECT

private slots:
  void test_resize ();
  void test_reset ();
  void test_generate ();
  void test_contains ();
  void test_empty ();
  void test_join ();
  void test_write_back ();
  void test_pre_patch ();
  void test_post_patch ();
  void test_copy ();
  void test_equality ();
  void test_changes ();
  void test_invert ();
  void test_size ();
};


/******************************************************************************
* tests on resize
******************************************************************************/
void
TestHashmap::test_resize () {
  auto hm = hashmap<int, int>(0, 10);
  hm(1) = 10;
  hm(2) = 20;

  hm->resize(1);
  QCOMPARE (hm[1] == 10, true);
  QCOMPARE (hm[2] == 20, true);

  hm->resize(20);
  QCOMPARE (hm[1] == 10, true);
  QCOMPARE (hm[2] == 20, true);
}

/******************************************************************************
* tests on reset 
******************************************************************************/
void
TestHashmap::test_reset () {
  auto hm = hashmap<int, int>(0, 10);
  hm(1) = 10;
  hm(11) = 20;
  hm->reset(1);

  QCOMPARE (hm->contains(1), false);
  QCOMPARE (hm->contains(11), true);
}

/******************************************************************************
* tests on generate 
******************************************************************************/
auto hm_generate = hashmap<int, int>(0, 10);
void routine(int key) {
    QCOMPARE (hm_generate->contains(key), true);
}

void
TestHashmap::test_generate () {
    hm_generate(1) = 10;
    hm_generate(2) = 20;
    hm_generate->generate(routine);
}

/******************************************************************************
* tests on contains 
******************************************************************************/
void
TestHashmap::test_contains () {
  auto hm = hashmap<int, void*>(nullptr, 2, 2);
  hm(1) = nullptr;
  QCOMPARE (hm->contains(1), true);
  QCOMPARE (hm->contains(3), false);
} 


/******************************************************************************
* tests on empty 
******************************************************************************/
void
TestHashmap::test_empty () {
  auto hm = hashmap<int, int>();
  QCOMPARE (hm->empty(), true);
  
  hm(1);
  QCOMPARE (hm->empty(), false);
}

/******************************************************************************
* tests on join 
******************************************************************************/
void
TestHashmap::test_join () {
  auto hm1 = hashmap<int, int>();
  auto hm2 = hashmap<int, int>();
  hm1(1) = 10;
  hm1(2) = 20;
  hm2(2) = -20;
  hm2(3) = -30;
  hm1->join(hm2);

  QCOMPARE (hm1[1] == 10, true);
  QCOMPARE (hm1[2] == -20, true);
  QCOMPARE (hm1[3] == -30, true);
}

/******************************************************************************
* tests on write_back 
******************************************************************************/
void
TestHashmap::test_write_back () {
  auto hm1 = hashmap<int, int>(0, 10);
  auto hm2 = hashmap<int, int>(0, 10);
  hm1(1) = 10;
  hm1(2) = 20;
  hm2(2) = -20;

  hm1->write_back(2, hm2);
  QCOMPARE (hm1[2] == 20, true);

  hm1->write_back(3, hm2);
  QCOMPARE (hm1[3] == 0, true);

  hm2(4) = -40;
  hm1->write_back(4, hm2);
  QCOMPARE (hm2[4] == -40, true);
}

/******************************************************************************
* tests on pre_patch 
******************************************************************************/
void
TestHashmap::test_pre_patch () {
  auto hm = hashmap<int, int>();
  auto hm_patch = hashmap<int, int>();
  auto hm_base = hashmap<int, int>();

  hm(1) = 10;
  hm_patch(1);
  hm->pre_patch(hm_patch, hm_base);
  QCOMPARE (hm[1] == 10, true);

  hm(2) = 20;
  hm_patch(2) = -20;
  hm_base(2) = 20;
  hm->pre_patch(hm_patch, hm_base);
  QCOMPARE (hm[2] == 0, true);

  hm_patch(3) = -30;
  hm->pre_patch(hm_patch, hm_base);
  QCOMPARE (hm[3] == -30, true);
}

/******************************************************************************
* tests on post_patch 
******************************************************************************/
void
TestHashmap::test_post_patch () {
  auto hm = hashmap<int, int>();
  auto hm_patch = hashmap<int, int>();
  auto hm_base = hashmap<int, int>();

  hm(1) = 10;
  hm_patch(1);
  hm->post_patch(hm_patch, hm_base);
  QCOMPARE (hm[1] == 0, true);

  hm_patch(2) = -20;
  hm->pre_patch(hm_patch, hm_base);
  QCOMPARE (hm[2] == -20, true);
}

/******************************************************************************
* tests on copy 
******************************************************************************/
void
TestHashmap::test_copy () {
  auto hm = hashmap<int, int>();
  auto hm_c = hashmap<int, int>(0, 10, 2);
  hm_c(1) = 10;
  hm_c(11) = 110;
  hm_c(2) = 20;

  auto res_hm = copy(hm_c);
  QCOMPARE (res_hm[1] == 10, true);
  QCOMPARE (res_hm[11] == 110, true);
  QCOMPARE (res_hm[2] == 20, true);
}

/******************************************************************************
* tests on equality 
******************************************************************************/
void
TestHashmap::test_equality () {
    auto hm1 = hashmap<int, int>(0, 10, 3);
    auto hm2 = hashmap<int, int>(0, 100, 30);
    hm1(1) = 10;
    hm2(1) = 10;
    QCOMPARE (hm1 == hm2, true);

    hm2(2) = 20;
    QCOMPARE (hm1 != hm2, true);
}

/******************************************************************************
* tests on changes 
******************************************************************************/
void
TestHashmap::test_changes() {
  auto base_m = hashmap<int, int>();
  auto patch_m = hashmap<int, int>();
  base_m(1) = 10;
  base_m(2) = 20;
  patch_m(2) = -20;
  patch_m(3) = -30;
  auto res = changes(patch_m, base_m);
  QCOMPARE (N(res) == 2, true);
  QCOMPARE (res[2] == -20, true);
  QCOMPARE (res[3] == -30, true);
}


/******************************************************************************
* tests on invert 
******************************************************************************/
void
TestHashmap::test_invert() {
  auto base_m= hashmap<int, int>();
  auto patch_m= hashmap<int, int>();
  base_m(1) = 10;
  base_m(2) = 20;
  patch_m(2) = -20;
  patch_m(3) = -30;
  auto res= invert(patch_m, base_m);
  QCOMPARE (N(res) == 2, true);
  QCOMPARE (res[2] == 20, true);
  QCOMPARE (res[3] == 0, true);
}


/******************************************************************************
* tests on N 
******************************************************************************/
void
TestHashmap::test_size () {
  auto empty_hm = hashmap<int, void*>();
  QCOMPARE (N(empty_hm) == 0, true);

  auto non_empty_hm = hashmap<int, void*>();
  non_empty_hm(1) = nullptr;
  QCOMPARE (N(non_empty_hm) == 1, true);
}

QTEST_MAIN(TestHashmap)
#include "hashmap_test.moc"
