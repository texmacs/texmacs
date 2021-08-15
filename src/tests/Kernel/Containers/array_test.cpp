/******************************************************************************
* MODULE     : array_test.cpp
* DESCRIPTION: test on array
* COPYRIGHT  : (C) 2019-2021  Yuhui Liu, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "array.hpp"
#include "string.hpp"

static array<int> gen_array(int n) {
  auto normal= array<int>();
  for (auto i=1;i<=n;i++) {
    normal << i;
  }
  return normal;
}

auto zero_elem= array<int> ();
auto one_elem= append(1, zero_elem);
auto two_elem= array<int> (1,2);
auto three_elem= array<int> (1,2,3);
auto four_elem= array<int> (1,2,3,4);
auto five_elem= array<int> (1,2,3,4,5);

class TestArray: public QObject {
  Q_OBJECT

private slots:
  void test_access ();
  void test_multiply ();
  void test_divide ();
  void test_range ();
  void test_size ();
  void test_copy ();
  void test_append ();
  void test_reverse ();
  void test_contains ();
};

void
TestArray::test_access () {
  QCOMPARE (five_elem[0], 1);
  QCOMPARE (five_elem[1], 2);
  QCOMPARE (five_elem[2], 3);
  QCOMPARE (five_elem[3], 4);
  QCOMPARE (five_elem[4], 5);
  QCOMPARE (one_elem[0], 1);
}

void
TestArray::test_multiply () {
  auto mulu4= array<int> ();
  for (auto i=1; i<6; i++){
      mulu4 << (i * 4);
  }
  QCOMPARE (mulu4, five_elem * 4);
}

void
TestArray::test_divide () {
  auto mulu4= array<int> ();
  for (auto i=1; i<6; i++){
    mulu4 << (i * 4);
  }
  QCOMPARE (mulu4 / 4, five_elem);
}

void
TestArray::test_range () {
  QCOMPARE (range (gen_array (10), 0, 5), five_elem);
  QCOMPARE (range (five_elem, 0, 4), four_elem);
}

void
TestArray::test_size () {
  QCOMPARE (N (zero_elem), 0);
  QCOMPARE (N (one_elem), 1);
  QCOMPARE (N (two_elem), 2);
  QCOMPARE (N (three_elem), 3);
  QCOMPARE (N (four_elem), 4);
  QCOMPARE (N (five_elem), 5);
  for (auto i=6; i<200; i++){
    auto array_test= gen_array (i);
    QCOMPARE ( N (array_test), i);
  }
}

void
TestArray::test_copy () {
  QCOMPARE (copy (zero_elem), zero_elem);
  QCOMPARE (copy (one_elem), one_elem);
  QCOMPARE (copy (two_elem), two_elem);
  QCOMPARE (copy (three_elem), three_elem);
  QCOMPARE (copy (four_elem), four_elem);
  QCOMPARE (copy (five_elem), five_elem);
}

void
TestArray::test_append () {
  auto zero_appended= append (1, zero_elem);
  QCOMPARE (zero_appended, one_elem);
  auto one_appended= append (one_elem, append (2, zero_elem));
  QCOMPARE (one_appended, two_elem);
  auto three_appended= append (three_elem, append (4, zero_elem));
  QCOMPARE (three_appended, four_elem);

  auto one2ten= gen_array (10);
  auto six2ten= array<int> (6,7,8,9,10);
  for (auto i=1; i<=10; i++){
    QCOMPARE (one2ten[i-1], i);
  }
  QCOMPARE (one2ten, append (five_elem, six2ten));
}

void
TestArray::test_reverse () {
  QCOMPARE (reverse (zero_elem), zero_elem);
  QCOMPARE (reverse (one_elem), one_elem);
  auto rev_five= array<int> (5,4,3,2,1);
  QCOMPARE (reverse (five_elem), rev_five);
}

void
TestArray::test_contains() {
  QCOMPARE (contains (1, zero_elem), false);
  QCOMPARE (contains (1, one_elem), true);
  QCOMPARE (contains (3, two_elem), false);
  QCOMPARE (contains (1, five_elem), true);
  QCOMPARE (contains (2, five_elem), true);
  QCOMPARE (contains (3, five_elem), true);
}

QTEST_MAIN(TestArray)
#include "array_test.moc"
