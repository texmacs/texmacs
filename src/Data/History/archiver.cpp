
/******************************************************************************
* MODULE     : archiver.cpp
* DESCRIPTION: manage undo/redo history
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "archiver.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

extern tree the_et;
array<patch> singleton (patch p);
static patch make_compound (array<patch> a);
static patch make_branches (array<patch> a);
static hashset<double> genuine_authors;
static hashset<pointer> archs;
static hashset<pointer> pending_archs;

/******************************************************************************
* Constructors, destructors, printing and announcements
******************************************************************************/

archiver_rep::archiver_rep (double author, path rp2):
  archive (make_branches (0)),
  current (make_compound (0)),
  depth (0),
  last_save (0),
  last_autosave (0),
  the_author (author),
  the_owner (0),
  rp (rp2),
  undo_obs (undo_observer (this)),
  versioning (false)
{
  archs->insert ((pointer) this);
  attach_observer (subtree (the_et, rp), undo_obs);
  genuine_authors->insert (the_author);
}

archiver_rep::~archiver_rep () {
  genuine_authors->remove (the_author);
  detach_observer (subtree (the_et, rp), undo_obs);
  archs->remove ((pointer) this);
  pending_archs->remove ((pointer) this);
}

archiver::archiver (double author, path rp):
  rep (tm_new<archiver_rep> (author, rp)) {}

void
archiver_rep::clear () {
  archive= make_branches (0);
  current= make_compound (0);
  the_owner= 0;
  depth= 0;
  last_save= -1;
  last_autosave= -1;
}

void
archiver_rep::show_all () {
  cout << HRULE << archive << LF << HRULE << LF;
}

////extern tree the_et;

void
archive_announce (archiver_rep* arch, modification mod) {
  //cout << "Archive " << mod << "\n";
  ////stretched_print (the_et, true);
  if (DEBUG_HISTORY) debug_history << "Archive " << mod << "\n";
  ASSERT (arch->rp <= mod->p, "invalid modification");
  if (!arch->versioning) {
    arch->add (mod);
    pending_archs->insert ((pointer) arch);
  }
}

void
global_clear_history () {
  iterator<pointer> it = iterate (archs);
  while (it->busy()) {
    archiver_rep* arch= (archiver_rep*) it->next();
    arch->clear ();
  }
}

void
global_confirm () {
  iterator<pointer> it = iterate (pending_archs);
  while (it->busy()) {
    archiver_rep* arch= (archiver_rep*) it->next();
    arch->confirm ();
    arch->simplify ();
  }
  pending_archs= hashset<pointer> ();
}

void
global_cancel () {
  iterator<pointer> it = iterate (pending_archs);
  while (it->busy()) {
    archiver_rep* arch= (archiver_rep*) it->next();
    arch->cancel ();
    arch->simplify ();
  }
  pending_archs= hashset<pointer> ();
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
  bool old= versioning;
  bool global_old= busy_versioning;
  versioning= true;
  busy_versioning= true;
  ::apply (p, the_et);
  versioning= old;
  busy_versioning= global_old;
}

void
archiver_rep::split (patch p1, patch p2, patch& re1, patch& re2) {
  //cout << "p1= " << p1 << "\n";
  //cout << "p2= " << p2 << "\n";
  array<patch> a= branches (p2);
  array<patch> a1;
  array<patch> a2;
  for (int i=0; i<N(a); i++) {
    patch q1= p1;
    patch q2= car (a[i]);
    if (get_author (q2) != the_author || !swap (q1, q2)) a1 << a[i];
    else a2 << patch (q1, make_future (q2, cdr (a[i])));
  }
  re1= make_branches (a1);
  re2= make_branches (a2);
  //cout << "re1= " << re1 << "\n";
  //cout << "re2= " << re2 << "\n";
}

patch
archiver_rep::make_future (patch p1, patch p2) {
  if (nr_branches (p2) == 0 || get_author (p1) == the_author)
    return patch (p1, p2);
  patch re1, re2;
  split (p1, p2, re1, re2);
  if (nr_branches (re1) != 0) re1= patch (p1, re1);
  return append_branches (re1, re2);
}

patch
archiver_rep::expose (patch archive) {
  if (nr_undo (archive) != 0 &&
      get_author (car (get_undo (archive))) != the_author &&
      nr_undo (cdr (get_undo (archive))) != 0)
    {
      patch nx1= expose (cdr (get_undo (archive)));
      if (get_author (car (get_undo (nx1))) != the_author) return archive;
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
      last_save= last_autosave= -1;
      return make_history (un, re);
    }
  else return archive;
}

void
archiver_rep::expose () {
  archive= expose (archive);
}

void
archiver_rep::normalize () {
  if (nr_undo (archive) != 0 && nr_redo (cdr (get_undo (archive))) != 0) {
    patch un1= get_undo (archive);
    patch re1= get_redo (archive);
    patch p1 = car (un1);
    patch nx1= cdr (un1);
    patch un2= get_undo (nx1);
    patch re2= get_redo (nx1);
    patch Re1, Re2;
    if (get_author (p1) == the_author) return;
    split (p1, re2, Re1, Re2);
    patch ar2= make_history (un2, Re1);
    archive= make_history (patch (p1, ar2), append_branches (re1, Re2));
    last_save= last_autosave= -1;
  }
}

/******************************************************************************
* Routines concerning the current modifications
******************************************************************************/

void
archiver_rep::add (modification m) {
  m= copy (m);
  if (the_owner != 0 && the_owner != get_author ()) {
    //cout << "Change " << the_owner << " -> " << get_author () << "\n";
    confirm ();
  }
  else the_owner= get_author ();
  modification i= invert (m, the_et);
  patch q (i, m);
  //cout << "Add [" << the_owner << "] " << q << "\n";
  current= patch (q, current);
}

void
archiver_rep::start_slave (double a) {
  if (the_owner != 0 && the_owner != get_author ()) {
    //cout << "Change " << the_owner << " -> " << get_author () << "\n";
    confirm ();
  }
  else the_owner= get_author ();
  patch q (a, false);
  //cout << "Add [" << the_owner << "] " << q << "\n";
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
    //cout << "Cancel " << current << "\n";
    apply (current);
    current= make_compound (0);
    the_owner= 0;
  }
}

void
archiver_rep::confirm () {
  if (active ()) {
    current= patch (the_owner, compactify (current));
    if (nr_children (remove_set_cursor (current)) == 0)
      current= make_compound (0);
    if (active ()) {
      //cout << "Confirm " << current << "\n";
      archive= patch (current, archive);
      current= make_compound (0);
      the_owner= 0;
      depth++;
      if (depth <= last_save) last_save= -1;
      if (depth <= last_autosave) last_autosave= -1;
      normalize ();
      //show_all ();
    }
  }
}

bool
archiver_rep::retract () {
  if (!has_history ()) return false;
  if (the_owner != 0 && the_owner != the_author) return false;
  expose ();
  patch un= car (get_undo (archive));
  if (get_author (un) != the_author) return false;
  patch re= get_redo (archive);
  patch nx= cdr (get_undo (archive));
  //cout << "Retract " << un << "\n";
  if (active ()) current= compactify (patch (current, un));
  else current= un;
  the_owner= the_author;
  if (nr_branches (re) != 0) {
    patch q= invert (current, the_et);
    re= patch (q, re);
  }
  if (nr_branches (nx) != 0) nx= get_undo (nx);
  archive= make_history (nx, append_branches (re, get_redo (nx)));
  depth--;
  //show_all ();
  return true;
}

bool
archiver_rep::forget () {
  cancel ();
  bool r= retract ();
  if (r) cancel ();
  return r;
}

void
archiver_rep::forget_cursor () {
  current= remove_set_cursor (current);
}

/******************************************************************************
* Simplification of the history
******************************************************************************/

void
archiver_rep::simplify () {
  if (has_history () &&
      nr_undo (cdr (get_undo (archive))) == 1 &&
      nr_redo (cdr (get_undo (archive))) == 0 &&
      depth != last_save + 1)
    {
      patch p1= car (get_undo (archive));
      patch p2= car (get_undo (cdr (get_undo (archive))));
      ////show_all ();
      ////stretched_print (the_et, true);
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
        if (depth == last_autosave + 1) last_autosave= -1;
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
    ASSERT (is_applicable (p, the_et), "history corrupted");
    patch q= invert (p, the_et);
    apply (p);
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
    if (depth <= last_save && i != 0) last_save= -1;
    if (depth <= last_autosave && i != 0) last_autosave= -1;
    depth++;
    normalize ();
    //show_all ();
    return cursor_hint (q, the_et);
  }
  return path ();
}

path
archiver_rep::undo (int i) {
  if (active ()) return path ();
  path r;
  while (undo_possibilities () != 0) {
    ASSERT (i == 0, "index out of range");
    expose ();
    if (get_author (car (get_undo (archive))) == the_author)
      return undo_one (i);
    else {
      r= undo_one (i);
      i= 0;
    }
  }
  return r;
}

path
archiver_rep::redo (int i) {
  if (active ()) return path ();
  path r;
  bool first= true;
  while (redo_possibilities () != 0) {
    ASSERT (i >= 0 && i < redo_possibilities (), "index out of range");
    patch re= branch (get_redo (archive), i);
    bool done= (get_author (car (re)) == the_author);
    r= redo_one (i);
    if (done && !first) break;
    if (nr_redo (archive) != 1) break;
    i= 0;
    first= false;
    re= branch (get_redo (archive), i);
    if (get_author (car (re)) == the_author) break;
    if (done && genuine_authors->contains (get_author (car (re)))) break;
  }
  return r;
}

/******************************************************************************
* Marking blocks for grouped modifications or canceling
******************************************************************************/

static bool
is_marker (patch p, double m, bool birth) {
  if (get_type (p) == PATCH_AUTHOR)
    return is_marker (p[0], m, birth);
  else if (get_type (p) == PATCH_BIRTH)
    return get_author (p) == m && get_birth (p) == birth;
  else return false;
}

static patch
compress (patch archive1) {
  if (nr_undo (archive1) == 0) return archive1;
  patch un1= get_undo (archive1);
  patch re1= get_redo (archive1);
  if (!is_author (car (un1))) return archive1;
  if (is_birth (car (un1) [0])) return archive1;
  if (!is_branch (re1) || N(re1) != 0) return archive1;
  patch archive2= compress (cdr (un1));
  if (nr_undo (archive2) == 0) return archive1;
  patch un2= get_undo (archive2);
  patch re2= get_redo (archive2);
  if (!is_author (car (un2))) return archive1;
  if (is_birth (car (un2) [0])) return archive1;
  if (!is_branch (re2) || N(re2) != 0) return archive1;
  if (get_author (car (un1)) != get_author (car (un2))) return archive1;
  //cout << "cun1= " << car (un1) << LF;
  //cout << "cun2= " << car (un2) << LF;
  patch cun= compactify (patch (car (un1), car (un2)));
  //cout << "cun = " << cun << LF;
  //return archive1;
  return make_history (patch (cun, cdr (un2)), re2);
}

static patch
remove_marker_bis (patch archive, double m) {
  ASSERT (nr_undo (archive) != 0, "marker not found");
  if (is_marker (car (get_undo (archive)), m, false)) {
    ASSERT (nr_redo (archive) == 0, "cannot remove marker");
    return cdr (get_undo (archive));
  }
  else {
    patch un= get_undo (archive);
    patch re= get_redo (archive);
    patch rem= remove_marker_bis (cdr (un), m);
    return make_history (patch (car (un), rem), re);
  }
}

static patch
remove_marker (patch archive, double m) {
  return remove_marker_bis (compress (archive), m);
}

void
archiver_rep::mark_start (double m) {
  //cout << "Mark start " << m << "\n";
  confirm ();
  start_slave (m);
  confirm ();
  //show_all ();
}

void
archiver_rep::mark_end (double m) {
  //cout << "Mark end " << m << "\n";
  if (active ()) {
    //if (does_modify (current))
    //  cout << "CONFIRM: " << current << "\n";
    confirm ();
  }
  archive= remove_marker (archive, m);
  depth--;
  simplify ();
  //show_all ();
}

bool
archiver_rep::mark_cancel (double m) {
  //cout << "Mark cancel " << m << "\n";
  cancel ();
  while (nr_undo (archive) != 0) {
    expose ();
    if (is_marker (car (get_undo (archive)), m, false)) {
      archive= remove_marker (archive, m);
      depth--;
      simplify ();
      return true;
    }
    if (get_author (car (get_undo (archive))) != the_author) {
      archive= remove_marker (archive, m);
      depth--;
      return false;
    }
    retract ();
    cancel ();
  }
  return false;
}

/******************************************************************************
* Check changes since last save/autosave
******************************************************************************/

int
archiver_rep::corrected_depth () {
  // NOTE : fix depth due to presence of marker
  // FIXME: implement a more robust check for conformity with saved state
  if (nr_undo (archive) == 0) return depth;
  patch p= car (get_undo (archive));
  if (get_type (p) == PATCH_AUTHOR) p= p[0];
  if (get_type (p) == PATCH_BIRTH && get_birth (p) == false) return depth - 1;
  return depth;
}

void
archiver_rep::require_save () {
  last_save= -1;
}

void
archiver_rep::notify_save () {
  last_save= corrected_depth ();
}

bool
archiver_rep::conform_save () {
  return last_save == corrected_depth ();
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
