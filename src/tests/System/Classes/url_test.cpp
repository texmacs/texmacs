
/******************************************************************************
* MODULE     : url_test.cpp
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"

#include <QtTest/QtTest>

class TestURL: public QObject {
  Q_OBJECT

private slots:
  void test_exists ();
  void test_suffix ();
};

void TestURL::test_exists () {
  // two cases: root directory
  // TODO: Windows compatibility
  url root_tmp = url ("/tmp");
  url root_no_such_tmp = url ("/no_such_tmp");
  QVERIFY (exists (root_tmp));
  QVERIFY (!exists (root_no_such_tmp));
}

void TestURL::test_suffix () {
  // empty suffix should work
  url no_suffix = url ("/a/b/c/d/no_suffix");
  QCOMPARE (suffix (no_suffix), string (""));
  url no_suffix2 = url ("/a/b.c/d/no_suffix");
  QCOMPARE (suffix (no_suffix2), string (""));

  // normal suffix should work
  url png = url ("/a/b/c/d.png");
  QCOMPARE (suffix (png), string ("png"));
  url png2 = url ("/a/b.c/d.png");
  QCOMPARE (suffix (png2), string ("png"));
}

QTEST_MAIN(TestURL)
#include "url_test.moc"
