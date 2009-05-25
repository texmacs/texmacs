
/******************************************************************************
* MODULE     : archiver.hpp
* DESCRIPTION: undo/redo history
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ARCHIVER_H
#define ARCHIVER_H
#include "patch.hpp"

class archiver_rep: public concrete_struct {
  patch archive;        // undo and redo archive
  patch current;        // current sequence of modifications
  int   depth;          // archive depth
  int   last_save;      // archive depth at last save
  int   last_autosave;  // archive depth at last autosave

protected:
  void apply (patch p);
  void show_all ();

public:
  archiver_rep ();
  ~archiver_rep ();
  void clear ();

  void add (patch p);
  void start_slave (double a);
  bool active ();
  bool has_history ();
  void cancel ();    // cancel current series of modifications
  void confirm ();   // move current modifications to history
  void retract ();   // reopen last history item for further modifications
  void forget ();    // undo and forget about last history item
  void simplify ();

  int  undo_possibilities ();
  int  redo_possibilities ();
  path undo_one (int i);
  path redo_one (int i);
  path undo (int i=0);
  path redo (int i=0);

  void require_save ();
  void require_autosave ();
  void notify_save ();
  void notify_autosave ();
  bool conform_save ();
  bool conform_autosave ();
};

class archiver {
CONCRETE (archiver);
  archiver ();
};
CONCRETE_CODE (archiver);

#endif // defined ARCHIVER_H
