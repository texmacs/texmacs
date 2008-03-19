
/******************************************************************************
* MODULE     : tm_buffer.hpp
* DESCRIPTION: TeXmacs main data structures (buffers, views and windows)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_BUFFER_H
#define TM_BUFFER_H
#include "server.hpp"

extern tree the_et;
path new_document ();
void delete_document (path rp);
void set_document (path rp, tree t);
int  create_window_id ();
void destroy_window_id (int);

class tm_buffer_rep {
public:
  url name;               // full name
  string abbr;            // abbreviated name
  string fm;              // buffer format
  url extra;              // for special buffers, like help buffer
  array<tm_view> vws;     // views attached to buffer
  bool need_save;         // (non textual) modification since last save?
  bool need_autosave;     // (non textual) modification since last autosave?
  bool read_only;         // buffer is read only?
  bool secure;            // is the buffer secure?
  tm_buffer prj;          // buffer which corresponds to the project
  bool in_menu;           // should the buffer be listed in the menus?

  tree undo;              // for undoing changes
  tree redo;              // for redoing changes
  tree exdo;              // for undoing redone changes
  int  undo_depth;        // number of changes
  int  redo_depth;        // number of undone changes
  int  last_save;         // how many changes at last save
  int  last_autosave;     // how many changes at last autosave

  path rp;                    // path to the document's root in the_et
  tree project;               // a project the document belongs to
  tree style;                 // the style of the buffer
  hashmap<string,tree> init;  // initial values of environment variables
  hashmap<string,tree> fin;   // final values of environment variables
  hashmap<string,tree> ref;   // all labels with references
  hashmap<string,tree> aux;   // auxiliary output: toc, bib, etc.

  inline tm_buffer_rep (url name2):
    name (name2), abbr (as_string (tail (name))),
    fm ("texmacs"), extra (url_none ()), vws (0),
    need_save (false), need_autosave (false),
    read_only (false), secure (is_secure (name2)),
    prj (NULL), in_menu (true), undo ("nil"), redo ("nil"), exdo ("nil"),
    undo_depth (0), redo_depth (0), last_save (0), last_autosave (0),
    rp (new_document ()), project (""), style ("style"),
    init ("?"), fin ("?"), ref ("?"), aux ("?") {}

  inline ~tm_buffer_rep () {
    delete_document (rp); }

  void mark_undo_block ();
  void mark_redo_block ();
  void unmark_undo_block ();
  void unmark_redo_block ();
  void redo_to_undo ();
  void truncate_undos (int nr);
  bool needs_to_be_saved ();
  bool needs_to_be_autosaved ();
};

#endif // defined TM_BUFFER_H
