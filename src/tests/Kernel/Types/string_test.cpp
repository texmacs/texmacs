
/******************************************************************************
* MODULE     : string_test.cpp
* DESCRIPTION: Fixed size strings with reference counting and
*              pointer copying. Zero-characters are allowed in strings.
* COPYRIGHT  : (C) 2018-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "string.hpp"

class TestString: public QObject {
  Q_OBJECT

private slots:
  void equality ();
  void compare ();
  void slice ();
  void concat ();
  void append ();

  void test_as_bool ();
  void test_as_string_bool ();

  void test_is_empty ();
  void test_is_bool ();
  void test_is_int ();
  void test_is_quoted ();
};



/******************************************************************************
* Tests on Common routines for strings
******************************************************************************/

void
TestString::equality() {
  QCOMPARE (string("abc") == "abc", true);
  QCOMPARE (string("abc") == "", false);

  QCOMPARE (string("abc") != "abc", false);
  QCOMPARE (string("abc") != "", true);

  QCOMPARE (string("abc") == string("abc"), true);
  QCOMPARE (string("abc") == string(), false);
  QCOMPARE (string("abc") != string("abc"), false);
  QCOMPARE (string("abc") != string(), true);

  QCOMPARE (string() == string(), true);
}

void
TestString::compare () {
  QVERIFY (string("ab") < string("b"));
  QVERIFY (string() < string("0"));

  QVERIFY (string("a") <= string("a"));
  QVERIFY (string("ab") <= string("b"));
  QVERIFY (string() <= string());
  QVERIFY (string() <= string("0"));
}

void
TestString::slice () {
  QVERIFY (string("abcde")(0, 0) == string());
  QVERIFY (string("abcde")(0, 1) == string("a"));
  QVERIFY (string("abcde")(0, 10) == string("abcde"));
  QVERIFY (string("abcde")(-1, 1) == string("a"));
  QVERIFY (string("abcde")(3, 2) == string());
  QVERIFY (string("abcde")(3, -2) == string());
  QVERIFY (string("abcde")(10, 11) == string());
  QVERIFY (string("abcde")(-3, -2) == string());
}

void
TestString::concat () {
  QVERIFY (string("abc") * "de" == string("abcde"));
  QVERIFY (string("abc") * string("de") == string("abcde"));
  QVERIFY ("abc" * string("de") == string("abcde"));
}

/******************************************************************************
* Modifications
******************************************************************************/
void
TestString::append () {
  auto str = string();
  str << 'x';
  QVERIFY (str == string("x"));
  str << string("yz");
  QVERIFY (str == string("xyz"));
}

/******************************************************************************
* Conversions
******************************************************************************/
void
TestString::test_as_bool () {
  QCOMPARE (as_bool(string("true")), true);
  QCOMPARE (as_bool(string("#t")), true);
  QCOMPARE (as_bool(string("false")), false);

  // implicit conversion from char*
  QVERIFY (as_bool("true"));
  QVERIFY (as_bool("#t"));
  QVERIFY (!as_bool("false"));
}

void
TestString::test_as_string_bool () {
  QVERIFY (as_string_bool(true) == string("true"));
  QVERIFY (as_string_bool(false) == string("false"));
}


/******************************************************************************
* Predicates
******************************************************************************/
void
TestString::test_is_empty () {
  QVERIFY (is_empty (""));
  QVERIFY (!is_empty (" "));
  QVERIFY (!is_empty ("nonempty"));
}

void
TestString::test_is_bool () {
  QVERIFY (is_bool ("true"));
  QVERIFY (is_bool ("false"));
  QVERIFY (is_bool (string ("true")));
  QVERIFY (is_bool (string ("false")));

  QVERIFY (!is_bool ("100"));
  QVERIFY (!is_bool ("nil"));
}

void
TestString::test_is_int () {
  // Empty string is not an int
  QVERIFY (!is_int (""));

  // Only 0-9 in chars are int
  for (auto i= 0; i<256; i++) {
    char iter= (char) i;
    if (iter >= '0' && iter <= '9')
      QVERIFY (is_int (iter));
    else
      QVERIFY (!is_int (iter));
  }

  // Random tests
  QVERIFY (is_int ("-100"));
  QVERIFY (is_int ("+100"));
  QVERIFY (is_int ("100"));

  QVERIFY (!is_int(".0"));
  QVERIFY (!is_int("0x09"));
}

void
TestString::test_is_quoted () {
  QVERIFY (is_quoted ("\"\""));
  QVERIFY (is_quoted ("\"Hello TeXmacs\""));
  // is_quoted only checks if a string starts with a double quote
  // and ends with another double quote, regardless the validity
  // of the raw string
  QVERIFY (is_quoted ("\"Hello\"TeXmacs\""));

  QVERIFY (!is_quoted ("\""));
  QVERIFY (!is_quoted ("A"));
  QVERIFY (!is_quoted ("9"));
  QVERIFY (!is_quoted ("Hello TeXmacs"));
  QVERIFY (!is_quoted ("\"Hello TeXmac\"s"));
  QVERIFY (!is_quoted ("H\"ello TeXmacs\""));
}

QTEST_MAIN(TestString)
#include "string_test.moc"
