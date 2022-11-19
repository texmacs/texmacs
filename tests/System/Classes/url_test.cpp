
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

public:
  url tmfs_1= url_system ("tmfs://git/help");
  url http_1= url_system ("http://texmacs.org");
  url https_1= url_system ("https://ustc.edu.cn");
  url root_tmp= url ("/tmp");
  url root_no_such_tmp= url ("/no_such_tmp");

private slots:
  void test_exists ();
  void test_suffix ();

  // predicates
  void test_is_rooted_tmfs ();
  void test_is_rooted_web ();

  // operations
  void test_descends();
};

void TestURL::test_exists () {
  // two cases: root directory
  // TODO: Windows compatibility
  QVERIFY (exists (root_tmp));
  QVERIFY (!exists (root_no_such_tmp));
}

void TestURL::test_suffix () {
  // empty suffix should work
  url no_suffix= url ("/a/b/c/d/no_suffix");
  QCOMPARE (suffix (no_suffix), string (""));
  url no_suffix2= url ("/a/b.c/d/no_suffix");
  QCOMPARE (suffix (no_suffix2), string (""));

  // normal suffix should work
  url png= url ("/a/b/c/d.png");
  QCOMPARE (suffix (png), string ("png"));
  url png2= url ("/a/b.c/d.png");
  QCOMPARE (suffix (png2), string ("png"));
}


void TestURL::test_is_rooted_tmfs () {
  QVERIFY (is_rooted_tmfs (tmfs_1));
  QVERIFY (!is_rooted_tmfs (http_1));
  QVERIFY (!is_rooted_tmfs (https_1));
  QVERIFY (!is_rooted_tmfs (root_tmp));
}

void TestURL::test_is_rooted_web () {
  QVERIFY (is_rooted_web (http_1));
  QVERIFY (is_rooted_web (https_1));
  QVERIFY (!is_rooted_web (root_tmp));
  QVERIFY (!is_rooted_web (tmfs_1));
}


void TestURL::test_descends () {
  QVERIFY (descends (url_system ("/tmp/a.txt"), root_tmp));
  QVERIFY (descends (url_system ("$TEXMACS_PATH/doc/main/man-manual.en.tm"),
                     url_system ("$TEXMACS_PATH")));
  QVERIFY (!descends (root_no_such_tmp, root_tmp));
}

QTEST_MAIN(TestURL)
#include "url_test.moc"
