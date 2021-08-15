
/******************************************************************************
* MODULE     : xml_test.cpp
* DESCRIPTION: Tests on TMML
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>

#include "convert.hpp"
#include "Xml/xml.cpp"

class TestXML: public QObject {
  Q_OBJECT

private:
  void assert_tm_and_xml_name (string tm_name, string xml_name) {
    QCOMPARE (as_charp (xml_name_to_tm (xml_name)), as_charp (tm_name));
    QCOMPARE (as_charp (tm_to_xml_name (tm_name)), as_charp (xml_name));
  }
private slots:
  void test_is_xml_name();
  void tm_and_xml_name();
};

void
TestXML::test_is_xml_name () {
  QVERIFY (is_xml_name (':', 0));
  QVERIFY (is_xml_name ('_', 0));
  for (unsigned char c=0; c<128; c++) {
    if (is_alpha (c)) {
      QVERIFY (is_xml_name (c, 0));
    }
  }
  QVERIFY (!is_xml_name ('-', 0));
  QVERIFY (!is_xml_name ('.', 0));
  QVERIFY (is_xml_name ('-', 1));
  QVERIFY (is_xml_name ('.', 1));
  for (unsigned char c=0; c<128; c++) {
    if (is_digit (c)) {
      QVERIFY (is_xml_name (c, 1));
      QVERIFY (!is_xml_name (c, 0));
    }
  }
}

void
TestXML::tm_and_xml_name() {
  // Normal characters
  assert_tm_and_xml_name ("hello", "hello");
  // The '_' escape itself
  assert_tm_and_xml_name ("_f", "_95_f");
  assert_tm_and_xml_name ("_38_f", "_95_38_95_f");
  // Valid start char for XML
  assert_tm_and_xml_name (":f", ":f");
  // Invalid start char for XML
  assert_tm_and_xml_name ("&f", "_38_f");
  assert_tm_and_xml_name (".f", "_46_f");
  assert_tm_and_xml_name ("-f", "_45_f");
  // Invalid char for XML
  assert_tm_and_xml_name ("f!", "f_33_");
}

QTEST_MAIN(TestXML)
#include "xml_test.moc"
