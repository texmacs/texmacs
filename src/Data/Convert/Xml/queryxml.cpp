
/******************************************************************************
* MODULE     : queryxml.cpp
* DESCRIPTION: extra utility functions for XML format
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
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

// See https://www.w3.org/TR/css-values-3/#absolute-lengths
static int cm2pt (double cm) {
  return (int) tm_round (28.34645 * cm);
}

static int mm2pt (double mm) {
  return (int) tm_round (2.834645 * mm);
}

static int Q2pt (double Q) {
  return (int) tm_round (28.34645 * Q / 40.0);
}

static int in2pt (double in) {
  return (int) tm_round (72 * in);
}

static int pc2pt (double pc) {
  return (int) tm_round (72 * pc / 6.0);
}

static int px2pt (double px) {
  // TODO: assume dpi is 600
  return (int) tm_round (72 * px / 96.0);
}

int parse_xml_length (string length) {
  double len;
  string unit;
  parse_length (length, len, unit);

  // default unit is px
  if (unit == "px" || is_empty (unit)) {
    return px2pt (len);
  } else if (unit == "cm") {
    return cm2pt (len);
  } else if (unit == "mm") {
    return mm2pt (len);
  } else if (unit == "Q") {
    return Q2pt (len);
  } else if (unit == "in") {
    return in2pt (len);
  } else if (unit == "pc") {
    return pc2pt (len);
  } else if (unit == "pt") {
    return (int) tm_round (len);
  } else {
    return 0;
  }
}
