
/******************************************************************************
* MODULE     : analyze_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "convert.hpp"
#include "drd_std.hpp"

TEST (xml_html_parser, expand_xml_default_entity) {
  ASSERT_TRUE (parse_xml ("&amp;") == tuple(tree("*TOP*"), tree("\"&\"")));
  ASSERT_TRUE (parse_xml ("&lt;") == tuple(tree("*TOP*"), tree("\"<\"")));
  ASSERT_TRUE (parse_xml ("&gt;") == tuple(tree("*TOP*"), tree("\">\"")));
  ASSERT_TRUE (parse_xml ("&apos;") == tuple(tree("*TOP*"), tree("\"'\"")));
  // init_std_drd ();
  // print_tree (parse_xml ("&quot;"));
  ASSERT_TRUE (parse_xml ("&quot;") == tuple(tree("*TOP*"), tree("\"\"\"")));
}
