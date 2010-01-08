
/******************************************************************************
* MODULE     : parse_string.hpp
* DESCRIPTION: strings from which it is both easy to read and write characters
*              they are used for entity replacement in the XML parser
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PARSE_STRING_H
#define PARSE_STRING_H
#include "string.hpp"
#include "list.hpp"

class parse_string;
class parse_string_rep: concrete_struct {
  list<string> l;   // strings left to parse
  list<int>    p;   // positions in each string

public:
  inline parse_string_rep (): l (), p () {}
  inline parse_string_rep (string s): l (s), p (0) {}
  inline ~parse_string_rep () {}

  void advance (int n);
  string read (int n);
  void write (string s);
  char get_char (int n);
  string get_string (int n);
  bool test (string s);

  friend class parse_string;
  friend tm_ostream& operator << (tm_ostream& out, parse_string s);
  friend bool test (parse_string s, string what);
};

class parse_string {
  CONCRETE(parse_string);
  inline parse_string (): rep (tm_new<parse_string_rep> ()) {}
  inline parse_string (string s): rep (tm_new<parse_string_rep> (s)) {}
  inline char operator [] (int i) { return rep->get_char (i); }
  inline operator bool () { return !is_nil (rep->l); }
  inline void operator += (int i) { rep->advance (i); }
};
CONCRETE_CODE(parse_string);

bool test (parse_string s, string what);

#endif // defined PARSE_STRING_H
