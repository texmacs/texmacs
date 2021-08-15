
/******************************************************************************
* MODULE     : hashset_test.cpp
* DESCRIPTION: test on hashset 
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "hashset.hpp"
#include "string.hpp"

class TestHashset: public QObject {
  Q_OBJECT

private slots:
  void test_contains();
  void test_init();
};

/******************************************************************************
* tests on contains 
******************************************************************************/
void
TestHashset::test_contains () {
  auto set= hashset<string>();
  set->insert("Hello");
  QCOMPARE (set->contains("Hello"), true);
  QCOMPARE (set->contains("hello"), false);
}

void
TestHashset::test_init () {
  auto set= hashset<string>();
  set << string("hello") << string("world");
  QCOMPARE (set->contains("hello"), true);
  QCOMPARE (set->contains("world"), true);
  QCOMPARE (N(set), 2);
}

QTEST_MAIN(TestHashset)
#include "hashset_test.moc"
