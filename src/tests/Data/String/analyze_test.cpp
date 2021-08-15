
/******************************************************************************
* MODULE     : analyze_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#include <QtTest/QtTest>
#include "analyze.hpp"

class TestAnalyze: public QObject {
  Q_OBJECT

private slots:
  void test_is_alpha ();
  void test_locase_all ();
  void test_upcase_all ();
  void test_string_minus ();
  void test_string_union ();
  void test_scm_quote ();
  void test_scm_unquote ();
  void test_raw_quote ();
  void test_raw_unquote ();
  void test_unescape_guile ();
  void test_starts ();
  void test_ends ();
  void test_read_word ();
};

void
TestAnalyze::test_is_alpha () {
  for (unsigned char c=0; c<255; c++) {
    if ((c>=65 && c<=90) || (c>=97 && c<=122)) {
      QVERIFY (is_alpha (c));
    } else {
      QVERIFY (!is_alpha (c));
    }
  }
}

void
TestAnalyze::test_locase_all () {
  QVERIFY (locase_all (string ("true")) == string ("true"));
  QVERIFY (locase_all (string ("TRue")) == string ("true"));
  QVERIFY (locase_all (string ("TRUE")) == string ("true"));
  QVERIFY (locase_all (string ("123TRUE")) == string ("123true"));
}

void
TestAnalyze::test_upcase_all () {
  QVERIFY (upcase_all (string ("true")) == string ("TRUE"));
  QVERIFY (upcase_all (string ("TRue")) == string ("TRUE"));
  QVERIFY (upcase_all (string ("TRUE")) == string ("TRUE"));
  QVERIFY (upcase_all (string ("123true")) == string ("123TRUE"));
}

void
TestAnalyze::test_string_minus () {
  QVERIFY (string_minus ("Hello World", "eo") == string ("Hll Wrld"));
  QVERIFY (string_minus ("", "abc") == string (""));
  QVERIFY (string_minus ("abc", "") == string ("abc"));
}

void
TestAnalyze::test_string_union () {
  QVERIFY (string_union ("abc", "") == string("abc"));
  QVERIFY (string_union ("", "abc") == string("abc"));
  QVERIFY (string_union ("Hello World", "eo") == string ("Hll Wrldeo"));
}

void
TestAnalyze::test_scm_quote () {
  QCOMPARE (as_charp (scm_quote ("a")), "\"a\"");
  QCOMPARE (as_charp (scm_quote ("")), "\"\"");
  QCOMPARE (as_charp (scm_quote ("\\")), "\"\\\\\"");
}

void
TestAnalyze::test_scm_unquote() {
  QCOMPARE (as_charp (scm_unquote("\"\"")), "");
  QCOMPARE (as_charp (scm_unquote("\"abc\"")), "abc");
  QCOMPARE (as_charp (scm_unquote("abc")), "abc");
  QCOMPARE (as_charp (scm_unquote("")), "");
  QCOMPARE (as_charp (scm_unquote("\"\\\\\"")), "\\");
}

void
TestAnalyze::test_raw_quote () {
  QCOMPARE (as_charp (raw_quote ("a")), "\"a\"");
  QCOMPARE (as_charp (raw_quote ("")), "\"\"");
}

void
TestAnalyze::test_raw_unquote () {
  QCOMPARE (as_charp (raw_unquote ("\"a\"")), "a");
  QCOMPARE (as_charp (raw_unquote ("\"a")), "\"a");
  QCOMPARE (as_charp (raw_unquote ("a\"")), "a\"");
  QCOMPARE (as_charp (raw_unquote ("")), "");
  QCOMPARE (as_charp (raw_unquote ("a")), "a");
}

void
TestAnalyze::test_unescape_guile () {
  QCOMPARE (as_charp (unescape_guile ("\\\\")), "\\\\\\\\");
}

void
TestAnalyze::test_starts () {
  QVERIFY (starts ("abc_def", "abc"));
  QVERIFY (!starts ("abc_def", "def"));
  QVERIFY (starts ("abc", ""));
  QVERIFY (starts ("", ""));
}

void
TestAnalyze::test_ends () {
  QVERIFY (ends ("abc_def", "def"));
  QVERIFY (ends ("abc_def", ""));
  QVERIFY (!ends ("abc_def", "de"));
}

void
TestAnalyze::test_read_word () {
  string word;
  int i=0;
  QVERIFY (read_word ("hello123", i, word));
  QCOMPARE (as_charp (word), "hello");
  QCOMPARE (i, 5);

  i=0;
  word= "";
  QVERIFY (!read_word ("123", i, word));
  QVERIFY (is_empty (word));
  QCOMPARE (i, 0);
}

QTEST_MAIN(TestAnalyze)
#include "analyze_test.moc"
