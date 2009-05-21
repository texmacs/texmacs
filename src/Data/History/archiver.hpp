
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
  patch before;         // undo history
  patch current;        // current sequence of modifications
  patch after;          // redo future
  int   depth;          // history depth
  int   last_save;      // history depth at last save
  int   last_autosave;  // history depth at last autosave

protected:
  void apply (patch p);
  void show_all ();

public:
  archiver_rep ();
  ~archiver_rep ();
  void clear ();

  void archive (patch p);
  bool active ();
  void cancel ();    // cancel current series of modifications
  void confirm ();   // move current modifications to history
  void merge ();     // merge current modifications with last history item
  void retract ();   // reopen last history item for further modifications
  void forget ();    // undo and forget about last history item
  void simplify ();

  bool no_more_undo ();
  bool no_more_redo ();
  int  undo_possibilities ();
  int  redo_possibilities ();
  path undo ();
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
