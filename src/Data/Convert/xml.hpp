
/******************************************************************************
* MODULE     : xml.hpp
* DESCRIPTION: XML headers
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef XML_H
#define XML_H

#include "tree.hpp"

tree find_first_element_by_name (tree t, string name);
string get_attr_from_element (tree t, string name, string default_value);
int parse_xml_length (string length);

#endif // defined XML_H
