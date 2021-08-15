
/******************************************************************************
* MODULE     : tree_test.cpp
* DESCRIPTION: Tests on tree
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#include <QtTest/QtTest>
#include "tree.hpp"

class TestTree: public QObject {
  Q_OBJECT

private slots:
  void test_is_atomic ();
  void test_is_tuple ();
  void test_is_concat ();
};


void
TestTree::test_is_atomic() {
  QVERIFY (is_atomic (tree ()));
}

void
TestTree::test_is_tuple() {
  QVERIFY (is_tuple (tuple ()));
  QVERIFY (is_tuple (tuple (tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree (), tree (), tree ())));
}

void
TestTree::test_is_concat() {
  QVERIFY (is_concat (concat ()));
  QVERIFY (is_concat (concat (tree ())));
  QVERIFY (is_concat (concat (tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree (), tree (), tree ())));
}

QTEST_MAIN(TestTree)
#include "tree_test.moc"
