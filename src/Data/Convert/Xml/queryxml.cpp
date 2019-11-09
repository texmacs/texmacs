
/******************************************************************************
* MODULE     : queryxml.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "xml.hpp"
#include "analyze.hpp"

tree find_first_element_by_name (tree t, string name) {
  if (!is_tuple (t)) return tree();

  string element_name= t[0]->label;
  if (element_name == name) return t;
  for (int i=1; i<N(t); i++) {
    if (is_tuple (t[i])) {
      tree ret= find_first_element_by_name (t[i], name);
      if (is_tuple (ret)) return ret;
    }
  }

  return tree();
}

string get_attr_from_element (tree t, string name, string default_value) {
  for (int i=1; i<N(t); i++)
    if (is_tuple (t[i]) && t[i][0]->label == "@") {
      for (int j=1; j<N(t[i]); j++) 
        if (is_tuple (t[i][j]) && t[i][j][0]->label == name) {
          return raw_unquote (t[i][j][1]->label);
        }
    }
  return default_value;
}

static int px2pt (double px) {
  // TODO: assume dpi is 600
  return tm_round (72 * px / 96.0);
}

int parse_xml_length (string length) {
  if (ends (length, "px") && is_double (length (0, N(length) - 2))) {
    return px2pt (as_double (length (0, N(length)-2)));
  }
  if (ends (length, "pt") && is_double (length (0, N(length) - 2))) {
    return tm_round (as_double (length (0, N(length)-2)));
  }
  // No length unit is specified, default to "px"
  if (is_double (length)) {
    return px2pt (as_double (length));
  }
  return 0;
}
