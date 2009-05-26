
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
array<patch> singleton (patch p);
static patch make_compound (array<patch> a);
static patch make_branches (array<patch> a);
static patch make_future (patch p1, patch p2);

/******************************************************************************
* Constructors, destructors and printing
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

void
archiver_rep::show_all () {
  cout << HRULE << archive << LF << HRULE << LF;
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

static int
nr_undo (patch p) {
  if (nr_branches (p) == 0 || nr_children (branch (p, 0)) != 2) return 0;
  return 1;
}

static int
nr_redo (patch p) {
  return max (0, nr_branches (p) - 1);
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

static void
split (patch p1, patch p2, patch& re1, patch& re2) {
  //cout << "p1= " << p1 << "\n";
  //cout << "p2= " << p2 << "\n";
  array<patch> a= branches (p2);
  array<patch> a1;
  array<patch> a2;
  for (int i=0; i<N(a); i++) {
    patch q1= p1;
    patch q2= car (a[i]);
    if (get_author (q1) == get_author (q2) || !swap (q1, q2)) a1 << a[i];
    else a2 << patch (q1, make_future (q2, cdr (a[i])));
  }
  re1= make_branches (a1);
  re2= make_branches (a2);
  //cout << "re1= " << re1 << "\n";
  //cout << "re2= " << re2 << "\n";
}

static patch
make_future (patch p1, patch p2) {
  if (nr_branches (p2) == 0 || get_author (p1) == get_author ())
    return patch (p1, p2);
  patch re1, re2;
  split (p1, p2, re1, re2);
  if (nr_branches (re1) != 0) re1= patch (p1, re1);
  return append_branches (re1, re2);
}

static patch
expose (patch archive, double author) {
  if (nr_undo (archive) != 0 &&
      get_author (car (get_undo (archive))) != author &&
      nr_undo (cdr (get_undo (archive))) != 0)
    {
      patch nx1= expose (cdr (get_undo (archive)), author);
      if (get_author (car (get_undo (nx1))) != author) return archive;
      patch un1= car (get_undo (archive));
      patch un2= car (get_undo (nx1));
      patch re1= get_redo (archive);
      patch re2= get_redo (nx1);
      patch nx2= cdr (get_undo (nx1));
      patch fut= make_branches (0);
      if (nr_branches (re2) != 0) fut= make_future (un1, re2);
      if (!swap (un1, un2)) return archive;
      patch nx= make_history (patch (un2, nx2), make_branches (0));
      patch un= patch (un1, nx);
      patch re= append_branches (re1, fut);
      return make_history (un, re);
    }
  else return archive;
}

static patch
normalize (patch archive) {
  if (nr_undo (archive) != 0 && nr_redo (car (get_undo (archive))) != 0) {
    patch un1= get_undo (archive);
    patch re1= get_redo (archive);
    patch p1 = car (un1);
    patch nx1= cdr (un1);
    patch un2= get_undo (nx1);
    patch re2= get_undo (nx1);
    patch Re1, Re2;
    split (p1, re2, Re1, Re2);
    patch ar2= make_history (un2, Re1);
    patch ar1= make_history (patch (p1, ar2), append_branches (re1, Re2));
    return ar1;
  }
  return archive;
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
  return nr_undo (archive) == 1;
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
    //archive= normalize (archive);
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
    archive= expose (archive, get_author ());
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
      nr_undo (cdr (get_undo (archive))) == 1 &&
      nr_redo (cdr (get_undo (archive))) == 0)
    {
      patch p1= car (get_undo (archive));
      patch p2= car (get_undo (cdr (get_undo (archive))));
      //cout << "p1= " << p1 << "\n";
      //cout << "p2= " << p2 << "\n";
      bool r= join (p1, p2, the_et);
      //cout << "pr= " << p1 << "\n";
      if (r) {
	//cout << "\n\nSimplify\n";
	//show_all ();
	patch un= patch (p1, cdr (get_undo (cdr (get_undo (archive)))));
	patch re= get_redo (archive);
	archive= make_history (un, re);
	//show_all ();
	//cout << "\n";
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
  return nr_undo (archive);
}

int
archiver_rep::redo_possibilities () {
  return nr_redo (archive);
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
    //patch re1= make_future (q, get_redo (archive));
    patch re1= patch (q, get_redo (archive));
    patch nx = cdr (get_undo (archive));
    patch re2= get_redo (nx);
    patch re = append_branches (re1, re2);
    patch un = (nr_branches (nx) == 0? nx: get_undo (nx));
    archive= make_history (un, re);
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
    patch nx= make_history (un, other);
    archive= make_history (patch (q, nx), cdr (branch (re, i)));
    //archive= normalize (archive);
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
  path r;
  double author= get_author ();
  archive= expose (archive, author);
  while (undo_possibilities () != 0) {
    ASSERT (i == 0, "index out of range");
    if (get_author (car (get_undo (archive))) == author)
      return undo_one (i);
    else {
      r= undo_one (i);
      i= 0;
    }
  }
  return r;
}

static int
redo_index (array<patch> a, int i) {
  double author= get_author ();
  for (int r=0; r<N(a); r++)
    if (get_author (car (a[r])) == author) {
      if (i == 0) return r;
      else i--;
    }
  for (int r=0; r<N(a); r++)
    if (get_author (car (a[r])) != author) {
      if (i == 0) return r;
      else i--;
    }
  FAILED ("index not found");
  return i;
}

path
archiver_rep::redo (int i) {
  if (active ()) return path ();
  // if (redo_possibilities () != 0) {
  //   i= redo_index (branches (get_redo (archive)), i);
  //   cout << "New index " << i << " for author " << get_author () << "\n";
  // }
  path r;
  double author= get_author ();
  while (redo_possibilities () != 0) {
    ASSERT (i >= 0 && i < redo_possibilities (), "index out of range");
    patch re= branch (get_redo (archive), i);
    bool stop= (nr_branches (cdr (re)) == 0);
    // bool done= (get_author (car (re)) == author);
    r= redo_one (i);
    // if (stop || done) break;
    if (stop) break;
    if (get_author (car (branch (get_redo (archive), 0))) == author) break;
    i= 0;
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
