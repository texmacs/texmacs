
/******************************************************************************
* MODULE     : keyword_parser_test.cpp
* DESCRIPTION: Properties of Keyword Parser
* COPYRIGHT  : (C) 2020-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "keyword_parser.hpp"

#include <QtTest/QtTest>
#include "converter.hpp"

class TestKeywordParser: public QObject {
  Q_OBJECT

private slots:
  void test_can_parse();
};

void TestKeywordParser::test_can_parse () {
  keyword_parser_rep keyword_parser= keyword_parser_rep ();
  int pos;
  keyword_parser.put ("key", "group");

  pos= 0;
  QVERIFY (keyword_parser.can_parse ("key group", pos));
}

QTEST_MAIN(TestKeywordParser)
#include "keyword_parser_test.moc"
