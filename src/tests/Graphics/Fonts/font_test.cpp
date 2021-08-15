
/******************************************************************************
* MODULE     : font_test.cpp
* DESCRIPTION: tests on font
* COPYRIGHT  : (C) 2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "font.hpp"

class TestFont: public QObject {
  Q_OBJECT

private slots:
  void test_default_chinese_font_name();
};

void TestFont::test_default_chinese_font_name() {
#ifdef OS_MACOS
  QCOMPARE (default_chinese_font_name (), string ("Songti SC"));
#endif
}

QTEST_MAIN(TestFont)
#include "font_test.moc"
