
/******************************************************************************
* MODULE     : archiver.hpp
* DESCRIPTION: manage undo/redo history
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ARCHIVER_H
#define ARCHIVER_H
#include "patch.hpp"

void global_clear_history ();
void global_confirm ();

class archiver_rep: public concrete_struct {
  patch    archive;        // undo and redo archive
  patch    current;        // current sequence of modifications
  int      depth;          // archive depth
  int      last_save;      // archive depth at last save
  int      last_autosave;  // archive depth at last autosave
  double   the_author;     // the author corresponding to the archiver
  double   the_owner;      // author of current modifications
  path     rp;             // root path for document
  observer undo_obs;       // observer for undoing changes
  bool     versioning;     // true during undo and redo operations

protected:
  void apply (patch p);
  void split (patch p1, patch p2, patch& re1, patch& re2);
  patch make_future (patch p1, patch p2);
  patch expose (patch archive);
  void expose ();
  void normalize ();
  int corrected_depth ();

public:
  archiver_rep (double author, path rp);
  ~archiver_rep ();
  void clear ();
  void show_all ();

  void add (modification m);
  void start_slave (double a);
  bool active ();
  bool has_history ();
  void cancel ();    // cancel current series of modifications
  void confirm ();   // move current modifications to history
  bool retract ();   // reopen last history item for further modifications
  bool forget ();    // undo and forget about last history item
  void simplify ();

  int  undo_possibilities ();
  int  redo_possibilities ();
  path undo_one (int i);
  path redo_one (int i);
  path undo (int i=0);
  path redo (int i=0);
  void mark_start (double m);
  bool mark_cancel (double m);
  void mark_end (double m);

  void require_save ();
  void require_autosave ();
  void notify_save ();
  void notify_autosave ();
  bool conform_save ();
  bool conform_autosave ();

  friend void archive_announce (archiver_rep* arch, modification mod);
  friend void global_clear_history ();
  friend void global_confirm ();
};

class archiver {
CONCRETE (archiver);
  archiver (double author, path rp);
};
CONCRETE_CODE (archiver);

#endif // defined ARCHIVER_H
