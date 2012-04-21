
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

/******************************************************************************
* The buffer class
******************************************************************************/

class new_buffer;
class new_buffer_rep: public concrete_struct {
public:
  url name;               // full name
  url master;             // base name for linking and navigation
  string fm;              // buffer format
  string title;           // buffer title (for menus)
  bool read_only;         // buffer is read only?
  bool secure;            // is the buffer secure?
  int last_save;          // last time that the buffer was saved
  time_t last_visit;      // time that the buffer was visited last

  inline new_buffer_rep (url name2):
    name (name2), master (name2),
    fm ("texmacs"), title (as_string (tail (name))),
    read_only (false), secure (is_secure (name2)),
    last_save (- (int) (((unsigned int) (-1)) >> 1)),
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

/******************************************************************************
* Routines
******************************************************************************/

array<url> get_all_buffers ();
url  make_new_buffer ();
void remove_buffer (url name);
int  number_buffers ();
url  get_this_buffer ();
url  get_name_buffer (path p);
void rename_buffer (url name, url new_name);
url get_master_buffer (url name);
void set_master_buffer (url name, url master);
void set_title_buffer (url name, string title);
string get_title_buffer (url name);
void set_buffer_tree (url name, tree doc);
tree get_buffer_tree (url name);
void set_buffer_body (url name, tree body);
tree get_buffer_body (url name);
void new_buffer_in_new_window (url name, tree t, tree geom= "");
int  get_last_save_buffer (url name);
void set_last_save_buffer (url name, int t);
double last_visited (url name);
bool buffer_modified (url name);
bool buffer_modified_since_autosave (url name);
void pretend_buffer_modified (url name);
void pretend_buffer_saved (url name);
void pretend_buffer_autosaved (url name);
bool buffer_has_name (url name);
bool buffer_import (url name, url src, string fm);
bool buffer_load (url name);
bool buffer_export (url name, url dest, string fm);
bool buffer_save (url name);
tree import_tree (url u, string fm);
bool export_tree (tree doc, url u, string fm);

#endif // NEW_BUFFER_H
