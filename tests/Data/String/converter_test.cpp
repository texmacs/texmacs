/******************************************************************************
* MODULE     : converter_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>

#include "converter.hpp"

class TestConverter: public QObject {
  Q_OBJECT

private slots:
  void test_utf8_to_cork();
};

void TestConverter::test_utf8_to_cork() {
  QCOMPARE (as_charp (utf8_to_cork ("中")), "<#4E2D>");
  QCOMPARE (as_charp (utf8_to_cork ("“")), "\x10");
  QCOMPARE (as_charp (utf8_to_cork("”")), "\x11");
}

QTEST_MAIN(TestConverter)
#include "converter_test.moc"
