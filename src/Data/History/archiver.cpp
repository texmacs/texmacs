
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
  before (array<patch> ()),
  current (array<patch> ()),
  after (array<patch> ()),
  depth (0),
  last_save (0),
  last_autosave (0) {}
archiver_rep::~archiver_rep () {}
archiver::archiver (): rep (tm_new<archiver_rep> ()) {}

void
archiver_rep::clear () {
  before= array<patch> ();
  current= array<patch> ();
  after= array<patch> ();
  depth= 0;
  last_save= -1;
  last_autosave= -1;
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
show (patch p) {
  if (N(p) == 0) return;
  if (N(p) == 2) {
    cout << p[0] << LF;
    show (p[1]);
  }
  else {
    for (int i=0; i<N(p); i+=2) {
      cout << "Possibility " << i/2 << LF << INDENT;
      cout << p[i] << LF;
      show (p[i+1]);
      cout << UNINDENT;
    }
  }
}

void
archiver_rep::show_all () {
  cout << HRULE;
  show (before); cout << HRULE;
  show (after); cout << HRULE << LF;
}

/******************************************************************************
* Routines concerning the current modifications
******************************************************************************/

void
archiver_rep::archive (patch p) {
  // cout << "Archive " << p << "\n";
  patch q= copy (invert (p, the_et));
  current= patch (q, current);
}

bool
archiver_rep::active () {
  return N (current) != 0;
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
    current= compactify (current);
    // cout << "Confirm " << current << "\n";
    before= append (array<patch> (current, before), get_children (after));
    current= patch (array<patch> ());
    after= patch (array<patch> ());
    depth++;
    if (depth <= last_save) last_save= -1;
    if (depth <= last_autosave) last_autosave= -1;
    //show_all ();
  }
}

void
archiver_rep::merge () {
  if (N (current) != 0 && N (before) != 0) {
    // cout << "Merge " << current << "\n";
    if (N (after) != 0) { confirm (); return; }
    array<patch> a= get_children (before);
    array<patch> b (1);
    b[0]= compactify (patch (current, a[0]));
    before= patch (append (b, range (a, 1, N(a))));
    current= patch (array<patch> ());
    if (depth <= last_save) last_save= -1;
    if (depth <= last_autosave) last_autosave= -1;
  }
}

void
archiver_rep::retract () {
  if (N (before) != 0) {
    // cout << "Retract " << before[0] << "\n";
    if (active ()) current= compactify (patch (current, before[0]));
    else current= before[0];
    array<patch> a;
    if (N (after) != 0) {
      patch q= invert (current, the_et);
      a= array<patch> (q, after);
    }
    array<patch> c= get_children (before);
    after= patch (append (a, range (c, 2, N(c))));
    before= before[1];
    depth--;
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
  if (N (before) == 2 &&
      N (before[1]) >= 2 &&
      get_type (before[0]) == PATCH_MODIFICATION &&
      get_type (before[1][0]) == PATCH_MODIFICATION)
    {
      modification m1= get_modification (before[0]);
      modification m2= get_modification (before[1][0]);
      //cout << "m1= " << m1 << "\n";
      //cout << "m2= " << m2 << "\n";
      if (m1->k == MOD_INSERT &&
	  m2->k == MOD_INSERT &&
	  is_atomic (m1->t) &&
	  root (m1) == root (m2) &&
	  (index (m2) == index (m1) ||
	   index (m2) == index (m1) + N (m1->t->label)))
	{
	  string s= m1->t->label * m2->t->label;
	  if (index (m2) == index (m1))
	    s= m2->t->label * m1->t->label;
	  modification m= mod_insert (root (m1), index (m1), tree (s));
	  array<patch> a (1); a[0]= m;
	  array<patch> c= get_children (before[1]);
	  before= patch (append (a, range (c, 1, N(c))));
	  depth--;
	}
      else if (m1->k == MOD_REMOVE &&
	       m2->k == MOD_REMOVE &&
	       is_atomic (subtree (the_et, root (m1))) &&
	       root (m1) == root (m2) &&
	       (index (m1) == index (m2) ||
		index (m1) == index (m2) + argument (m2)))
	{
	  modification m= mod_remove (root (m2), index (m2),
				      argument (m1) + argument (m2));
	  array<patch> a (1); a[0]= m;
	  array<patch> c= get_children (before[1]);
	  before= patch (append (a, range (c, 1, N(c))));
	  depth--;
	} 
    }
}

/******************************************************************************
* Undo and redo
******************************************************************************/

bool
archiver_rep::no_more_undo () {
  return N (before) == 0;
}

bool
archiver_rep::no_more_redo () {
  return N (after) == 0;
}

int
archiver_rep::undo_possibilities () {
  if (no_more_undo ()) return 0;
  else return 1;
}

int
archiver_rep::redo_possibilities () {
  return N (after) >> 1;
}

path
archiver_rep::undo () {
  if (N (current) == 0 && N (before) != 0) {
    ASSERT (is_applicable (before[0], the_et), "history corrupted");
    patch p= before[0];
    // cout << "p= " << p << "\n";
    patch q= invert (p, the_et);
    // cout << "q= " << q << "\n";
    apply (p);
    array<patch> c= get_children (before);
    array<patch> a= range (c, 2, N(c));
    before= before[1];
    after= append (array<patch> (q, after), a);
    depth--;
    //show_all ();
    return cursor_hint (q, the_et);
  }
  return path ();
}

path
archiver_rep::redo (int i) {
  ASSERT (i >= 0 && ((2*i) < N(after)), "index out of range");
  if (N (current) == 0 && N (after) != 0) {
    ASSERT (is_applicable (after[2*i], the_et), "future corrupted");
    patch p= after[2*i];
    // cout << "p= " << p << "\n";
    patch q= invert (p, the_et);
    // cout << "q= " << q << "\n";
    apply (p);
    array<patch> c= get_children (after);
    array<patch> a= append (range (c, 0, 2*i), range (c, 2*i+2, N(c)));
    before= patch (append (array<patch> (q, before), a));
    after = after[2*i+1];
    if (depth <= last_save && i != 0) last_save= -1;
    if (depth <= last_autosave && i != 0) last_autosave= -1;
    depth++;
    //show_all ();
    return cursor_hint (q, the_et);
  }
  return path ();
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
