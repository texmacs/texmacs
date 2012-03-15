
/******************************************************************************
* MODULE     : new_buffer.hpp
* DESCRIPTION: File related information for buffers
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_BUFFER_H
#define NEW_BUFFER_H
#include "tree.hpp"
#include "hashmap.hpp"
#include "url.hpp"

class new_buffer;
class new_buffer_rep: public concrete_struct {
public:
  url name;               // full name
  url extra;              // for special buffers, like help buffer
  string fm;              // buffer format
  string abbr;            // abbreviated name
  bool read_only;         // buffer is read only?
  bool secure;            // is the buffer secure?
  bool in_menu;           // should the buffer be listed in the menus?

  inline new_buffer_rep (url name2):
    name (name2), extra (url_none ()),
    fm ("texmacs"), abbr (as_string (tail (name))),
    read_only (false), secure (is_secure (name2)), in_menu (true) {}
};

class new_buffer {
CONCRETE(new_buffer);
  inline new_buffer (url name): rep (tm_new<new_buffer_rep> (name)) {}
};
CONCRETE_CODE(new_buffer);

#endif // NEW_BUFFER_H
