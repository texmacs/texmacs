
/******************************************************************************
* MODULE     : queryxml_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gtest/gtest.h"

#include "convert.hpp"
#include "drd_std.hpp"

TEST (xml, find_first_element_by_name) {
  init_std_drd ();
  string svg_content= "<svg xmlns=\"http://www.w3.org/2000/svg\"\
class=\"railroad-diagram\" width=\"521\" height=\"110\" viewBox=\"0 0 521 110\">\
</svg>";
  tree t= parse_xml (svg_content);
  // print_tree (t);
  tree result= find_first_element_by_name (t, "svg");
  // print_tree (result);
  string width= get_attr_from_element (result, "width", "");
  string height= get_attr_from_element (result, "height", "");
  ASSERT_TRUE (width == "521");
  ASSERT_TRUE (height == "110");
}

TEST (xml, parse_xml_length) {
  ASSERT_EQ (parse_xml_length ("10"), 8);
  ASSERT_EQ (parse_xml_length ("10px"), 8);
  ASSERT_EQ (parse_xml_length ("10pt"), 10);
}
