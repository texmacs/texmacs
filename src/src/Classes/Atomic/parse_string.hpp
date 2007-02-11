
/******************************************************************************
* MODULE     : parse_string.hpp
* DESCRIPTION: strings from which it is both easy to read and write characters
*              they are used for entity replacement in the XML parser
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  friend ostream& operator << (ostream& out, parse_string s);
  friend bool test (parse_string s, string what);
};

class parse_string {
  CONCRETE(parse_string);
  inline parse_string (): rep (new parse_string_rep ()) {}
  inline parse_string (string s): rep (new parse_string_rep (s)) {}
  inline char operator [] (int i) { return rep->get_char (i); }
  inline operator bool () { return !nil (rep->l); }
  inline void operator += (int i) { rep->advance (i); }
};
CONCRETE_CODE(parse_string);

bool test (parse_string s, string what);

#endif // defined PARSE_STRING_H
