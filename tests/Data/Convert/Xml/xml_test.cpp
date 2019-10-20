
/******************************************************************************
* MODULE     : xml_test.cpp
* DESCRIPTION: Tests on TMML
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "convert.hpp"
#include "Xml/xml.cpp"

TEST (xml, is_xml_name) {
  ASSERT_TRUE (is_xml_name (':', 0));
  ASSERT_TRUE (is_xml_name ('_', 0));
  for (unsigned char c=0; c<128; c++) {
    if (is_alpha (c)) {
      ASSERT_TRUE (is_xml_name (c, 0));
    }
  }
  ASSERT_FALSE (is_xml_name ('-', 0));
  ASSERT_FALSE (is_xml_name ('.', 0));
  ASSERT_TRUE (is_xml_name ('-', 1));
  ASSERT_TRUE (is_xml_name ('.', 1));
  for (unsigned char c=0; c<128; c++) {
    if (is_digit (c)) {
      ASSERT_TRUE (is_xml_name (c, 1));
      ASSERT_FALSE (is_xml_name (c, 0));
    }
  }
}


void
assert_tm_and_xml_name (string tm_name, string xml_name) {
  ASSERT_STREQ (as_charp (xml_name_to_tm (xml_name)), as_charp (tm_name));
  ASSERT_STREQ (as_charp (tm_to_xml_name (tm_name)), as_charp (xml_name));
}

TEST (xml, tm_and_xml_name) {
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
