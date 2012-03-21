
/******************************************************************************
* MODULE     : tm_buffer.hpp
* DESCRIPTION: TeXmacs main data structures (buffers, views and windows)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_BUFFER_H
#define TM_BUFFER_H
#include "new_data.hpp"
#include "new_buffer.hpp"

class tm_buffer_rep;
class tm_view_rep;
typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;

extern tree the_et;
path new_document ();
void delete_document (path rp);
void set_document (path rp, tree t);
int  create_window_id ();
void destroy_window_id (int);

class tm_buffer_rep {
public:
  new_buffer buf;         // file related information
  new_data data;          // data associated to document
  array<tm_view> vws;     // views attached to buffer
  tm_buffer prj;          // buffer which corresponds to the project
  path rp;                // path to the document's root in the_et

  inline tm_buffer_rep (url name):
    buf (name), data (),
    vws (0), prj (NULL), rp (new_document ()) {}

  inline ~tm_buffer_rep () {
    delete_document (rp); }

  bool needs_to_be_saved ();
  bool needs_to_be_autosaved ();
};

inline tm_buffer nil_buffer () { return (tm_buffer) NULL; }
inline bool is_nil (tm_buffer buf) { return buf == NULL; }

#endif // defined TM_BUFFER_H
