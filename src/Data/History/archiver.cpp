
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
static patch make_compound (array<patch> a);
static patch make_branches (array<patch> a);

/******************************************************************************
* Constructors and destructors
******************************************************************************/

archiver_rep::archiver_rep ():
  archive (make_branches (0)),
  current (make_compound (0)),
  depth (0),
  last_save (0),
  last_autosave (0) {}
archiver_rep::~archiver_rep () {}
archiver::archiver (): rep (tm_new<archiver_rep> ()) {}

void
archiver_rep::clear () {
  archive= make_branches (0);
  current= make_compound (0);
  depth= 0;
  last_save= -1;
  last_autosave= -1;
}

/******************************************************************************
* Useful subroutines
******************************************************************************/

static patch
make_compound (array<patch> a) {
  if (N(a) == 1) return a[0];
  else return patch (false, a);
}

static patch
make_branches (array<patch> a) {
  if (N(a) == 1) return a[0];
  else return patch (true, a);
}

static patch
append_branches (patch p1, patch p2) {
  return make_branches (append (branches (p1), branches (p2)));
}

static patch
make_history (patch undo, patch redo) {
  array<patch> a;
  a << undo << branches (redo);
  return make_branches (a);
}

static patch
get_undo (patch p) {
  ASSERT (nr_branches (p) > 0, "undo part unavailable");
  return branch (p, 0);
}

static patch
get_redo (patch p) {
  if (nr_branches (p) == 0) return make_branches (array<patch> ());
  return make_branches (branches (p, 1, nr_branches (p)));
}

static patch
car (patch p) {
  ASSERT (nr_children (branch (p, 0)) == 2, "car unavailable")
  return child (p, 0);
}

static patch
cdr (patch p) {
  ASSERT (nr_children (branch (p, 0)) == 2, "cdr unavailable")
  return child (p, 1);
}

/******************************************************************************
* Internal subroutines
******************************************************************************/

void
archiver_rep::apply (patch p) {
  // apply a patch, while disabling versioning during the modifications
  // cout << "Apply " << p << "\n";
  ASSERT (is_applicable (p, the_et), "invalid history");
  bool old= versioning_busy;
  versioning_busy= true;
  ::apply (p, the_et);
  versioning_busy= old;
}

/*
static patch
expose (patch history, double author) {
  if (N (history) == 2 &&
      get_author (history[0]) != author &&
      N (history[1]) == 2)
    {
      patch p1= history[0];
      patch hh= expose (history[1], author);
      patch p2= hh[0];
      if (get_author (p2) != author) return history;
      cout << "p1 < " << p1 << "\n";
      cout << "p2 < " << p2 << "\n";
      if (!swap (p1, p2)) return history;
      cout << "p1 > " << p1 << "\n";
      cout << "p2 > " << p2 << "\n";
      return patch (p1, patch (p2, hh[1]));
    }
  else return history;
}
*/

void
archiver_rep::show_all () {
  cout << HRULE << archive << LF << HRULE << LF;
}

/******************************************************************************
* Routines concerning the current modifications
******************************************************************************/

void
archiver_rep::add (patch p) {
  // cout << "Add [" << get_author () << "] " << p << "\n";
  patch q= copy (invert (p, the_et));
  current= patch (q, current);
}

void
archiver_rep::start_slave (double a) {
  patch q (a, false);
  current= patch (q, current);
}

bool
archiver_rep::active () {
  return nr_children (current) != 0;
}

bool
archiver_rep::has_history () {
  if (nr_branches (archive) == 0) return false;
  else return nr_branches (get_undo (archive)) != 0;
}

void
archiver_rep::cancel () {
  if (active ()) {
    // cout << "Cancel " << current << "\n";
    apply (current);
    current= make_compound (0);
  }
}

void
archiver_rep::confirm () {
  if (active ()) {
    current= patch (get_author (), compactify (current));
    // cout << "Confirm " << current << "\n";
    archive= patch (current, archive);
    current= make_compound (0);
    depth++;
    if (depth <= last_save) last_save= -1;
    if (depth <= last_autosave) last_autosave= -1;
    //show_all ();
  }
}

void
archiver_rep::retract () {
  if (has_history ()) {
    patch un= car (get_undo (archive));
    patch re= get_redo (archive);
    patch nx= cdr (get_undo (archive));
    //cout << "Retract " << un << "\n";
    if (active ()) current= compactify (patch (current, un));
    else current= un;
    if (nr_branches (re) != 0) {
      patch q= invert (current, the_et);
      re= patch (q, re);
    }
    if (nr_branches (nx) != 0) nx= get_undo (nx);
    archive= make_history (nx, append_branches (re, get_redo (nx)));
    depth--;
    //show_all ();
  }
}

void
archiver_rep::forget () {
  cancel ();
  retract ();
  cancel ();
}

/******************************************************************************
* History simplification
******************************************************************************/

void
archiver_rep::simplify () {
  if (has_history () &&
      nr_branches (cdr (get_undo (archive))) == 1 &&
      nr_children (get_undo (cdr (get_undo (archive)))) == 2)
    {
      patch p1= car (get_undo (archive));
      patch p2= car (get_undo (cdr (get_undo (archive))));
      //cout << "p1= " << p1 << "\n";
      //cout << "p2= " << p2 << "\n";
      bool r= join (p1, p2, the_et);
      //cout << "pr= " << p1 << "\n";
      if (r) {
	patch un= patch (p1, cdr (get_undo (cdr (get_undo (archive)))));
	patch re= get_redo (archive);
	archive= make_history (un, re);
	//show_all ();
	depth--;
	simplify ();
      }
    }
}

/******************************************************************************
* Undo and redo
******************************************************************************/

int
archiver_rep::undo_possibilities () {
  if (has_history ()) return 1;
  else return 0;
}

int
archiver_rep::redo_possibilities () {
  if (nr_branches (archive) == 0) return 0;
  else return nr_branches (get_redo (archive));
}

path
archiver_rep::undo_one (int i) {
  if (active ()) return path ();
  if (undo_possibilities () != 0) {
    ASSERT (i == 0, "index out of range");
    patch p= car (get_undo (archive));
    //cout << "p= " << p << "\n";
    ASSERT (is_applicable (p, the_et), "history corrupted");
    patch q= invert (p, the_et);
    //cout << "q= " << q << "\n";
    apply (p);
    patch r= patch (q, get_redo (archive));
    //cout << "r= " << r << "\n";
    patch nx= cdr (get_undo (archive));
    if (nr_branches (nx) != 0) nx= get_undo (nx);
    patch s= append_branches (r, get_redo (nx));
    archive= make_history (nx, s);
    depth--;
    //show_all ();
    return cursor_hint (q, the_et);
  }
  return path ();
}

path
archiver_rep::redo_one (int i) {
  if (active ()) return path ();
  int n= redo_possibilities ();
  if (n != 0) {
    ASSERT (i >= 0 && i < n, "index out of range");
    patch un= get_undo (archive);
    patch re= get_redo (archive);
    patch p= car (branch (re, i));
    //cout << "p= " << p << "\n";
    ASSERT (is_applicable (p, the_et), "future corrupted");
    patch q= invert (p, the_et);
    //cout << "q= " << q << "\n";
    apply (p);
    patch other= make_branches (append (branches (re, 0, i),
					branches (re, i+1, n)));
    //cout << "other= " << other << "\n";
    patch next= make_history (un, other);
    //cout << "next= " << next << "\n";
    archive= make_history (patch (q, next), cdr (branch (re, i)));
    if (depth <= last_save && i != 0) last_save= -1;
    if (depth <= last_autosave && i != 0) last_autosave= -1;
    depth++;
    //show_all ();
    return cursor_hint (q, the_et);
  }
  return path ();
}

path
archiver_rep::undo (int i) {
  if (active ()) return path ();
  double author= get_author ();
  //before= expose (before, author);
  while (undo_possibilities () != 0) {
    ASSERT (i == 0, "index out of range");
    if (get_author (car (get_undo (archive))) == author)
      return undo_one (i);
    else {
      (void) undo_one (i);
      i= 0;
    }
  }
  return path ();
}

path
archiver_rep::redo (int i) {
  if (active ()) return path ();
  path r;
  double author= get_author ();
  while (redo_possibilities () != 0) {
    ASSERT (i >= 0 && i < redo_possibilities (), "index out of range");
    bool ok= (get_author (car (branch (get_redo (archive), i))) == author);
    path p= redo_one (i);
    if (ok) r= p;
    i= 0;
    if (redo_possibilities () == 0) break;
    if (get_author (car (branch (get_redo (archive), i))) == author) break;
  }
  return r;
}

/******************************************************************************
* Check changes since last save/autosave
******************************************************************************/

void
archiver_rep::require_save () {
  last_save= -1;
}

void
archiver_rep::notify_save () {
  last_save= depth;
}

bool
archiver_rep::conform_save () {
  return last_save == depth;
}

void
archiver_rep::require_autosave () {
  last_autosave= -1;
}

void
archiver_rep::notify_autosave () {
  last_autosave= depth;
}

bool
archiver_rep::conform_autosave () {
  return last_autosave == depth;
}
