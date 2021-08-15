
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

#include "convert.hpp"
#include "drd_std.hpp"

class TestParseXML: public QObject {
  Q_OBJECT

private slots:
  void expand_xml_default_entity();
};


void TestParseXML::expand_xml_default_entity () {
  // init_std_drd ();
  // print_tree (parse_xml ("&quot;"));
  QVERIFY (parse_xml ("&amp;") == tuple(tree("*TOP*"), tree("\"&\"")));
  QVERIFY (parse_xml ("&lt;") == tuple(tree("*TOP*"), tree("\"<\"")));
  QVERIFY (parse_xml ("&gt;") == tuple(tree("*TOP*"), tree("\">\"")));
  QVERIFY (parse_xml ("&apos;") == tuple(tree("*TOP*"), tree("\"'\"")));
  QVERIFY (parse_xml ("&quot;") == tuple(tree("*TOP*"), tree("\"\\\"\"")));
}

QTEST_MAIN(TestParseXML)
#include "parsexml_test.moc"
