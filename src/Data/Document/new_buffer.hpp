
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
#include "timer.hpp"

class new_buffer;
class new_buffer_rep: public concrete_struct {
public:
  url name;               // full name
  url master;             // base name for linking and navigation
  string fm;              // buffer format
  string title;           // buffer title (for menus)
  bool read_only;         // buffer is read only?
  bool secure;            // is the buffer secure?
  time_t last_visit;      // time that the buffer was visited last

  inline new_buffer_rep (url name2):
    name (name2), master (name2),
    fm ("texmacs"), title (as_string (tail (name))),
    read_only (false), secure (is_secure (name2)),
    last_visit (texmacs_time ()) {}
};

class new_buffer;
class new_buffer {
CONCRETE(new_buffer);
  inline new_buffer (url name): rep (tm_new<new_buffer_rep> (name)) {}
};
//CONCRETE_CODE(new_buffer);

inline new_buffer::new_buffer (const new_buffer& x):
  rep(x.rep) { INC_COUNT (this->rep); }
inline new_buffer::~new_buffer () { DEC_COUNT (this->rep); }
inline new_buffer_rep* new_buffer::operator -> () {
  return rep; }
inline new_buffer& new_buffer::operator = (new_buffer x) {
  INC_COUNT (x.rep); DEC_COUNT (this->rep);
  this->rep=x.rep; return *this; }

#endif // NEW_BUFFER_H
