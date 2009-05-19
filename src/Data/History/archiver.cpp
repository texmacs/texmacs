
/******************************************************************************
* MODULE     : archiver.cpp
* DESCRIPTION: undo/redo history
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "archiver.hpp"

extern tree the_et;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

archiver_rep::archiver_rep ():
  depth (0),
  last_save (0),
  before (array<patch> ()),
  current (array<patch> ()),
  after (array<patch> ()) {}
archiver_rep::~archiver_rep () {}
archiver::archiver (): rep (tm_new<archiver_rep> ()) {}

/******************************************************************************
* Internal subroutines
******************************************************************************/

void
archiver_rep::apply (patch p) {
  // apply a patch, while disabling versioning during the modifications
  ASSERT (is_applicable (p, the_et), "invalid history");
  bool old= versioning_busy;
  versioning_busy= true;
  ::apply (p, the_et);
  versioning_busy= old;
}

/******************************************************************************
* Routines concerning the current modifications
******************************************************************************/

void
archiver_rep::archive (patch p) {
  patch q= copy (invert (p, the_et));
  current= patch (q, current);
}

void
archiver_rep::cancel () {
  if (N (current) != 0) {
    apply (current);
    current= patch (array<patch> ());
  }
}

void
archiver_rep::confirm () {
  if (N (current) != 0) {
    before= patch (current, before);
    current= patch (array<patch> ());
    after= patch (array<patch> ());
    depth++;
    if (depth <= last_save) last_save= -1;
  }
}

void
archiver_rep::merge () {
  if (N (current) != 0 && N (before) == 2) {
    before= patch (patch (current, before[0]), before[1]);
    current= patch (array<patch> ());
    after= patch (array<patch> ());
    if (depth <= last_save) last_save= -1;
  }
}

void
archiver_rep::retract () {
  if (N (current) == 0 && N (before) == 2) {
    current= before[0];
    before= before[1];
    after= patch (array<patch> ());
    if (depth <= last_save) last_save= -1;
    depth--;
  }
}

void
archiver_rep::forget () {
  if (N (current) == 0 && N (before) == 2) {
    apply (before[0]);
    before= before[1];
    after= patch (array<patch> ());
    if (depth <= last_save) last_save= -1;
    depth--;
  }
}

/******************************************************************************
* Undo and redo
******************************************************************************/

bool
archiver_rep::no_more_undo () {
  return N (before) != 2;
}

bool
archiver_rep::no_more_redo () {
  return N (after) != 2;
}

void
archiver_rep::undo () {
  if (N (current) == 0 && N (before) == 2) {
    patch p= before[0];
    patch q= invert (q, the_et);
    apply (p);
    before= before[1];
    after = patch (q, after);
    depth--;
  }
}

void
archiver_rep::redo () {
  if (N (current) == 0 && N (after) == 2) {
    patch p= after[0];
    patch q= invert (q, the_et);
    apply (p);
    before= patch (q, before);
    after = after[1];
    depth++;
  }
}

void
archiver_rep::notify_save () {
  last_save= depth;
}

bool
archiver_rep::no_changes () {
  return last_save == depth;
}
