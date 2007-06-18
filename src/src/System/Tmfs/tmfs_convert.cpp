
/******************************************************************************
* MODULE     : tmfs_convert.cpp
* DESCRIPTION: conversions from/into conventional file systems
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Useful subroutines
******************************************************************************/

/*
bool
is_cruft (string s) {
  return
    s == "CVS" ||
    ends (s, "~");
}

string
create_name (string name, string contents) {
  (void) contents;
  return name;
}

string
load_string (url u) {
  string s= "";
  (void) load_string (u, s, false);
  return s;
}

void
create_parents (url u) {
  if (!exists (url_parent (u))) {
    if (!is_root (u)) create_parents (url_parent (u));
    mkdir (url_parent (u));
  }
}
*/

/******************************************************************************
* Importation and exportation
******************************************************************************/

/*
bool
operator < (string a, string b) {
  return a <= b && a != b;
}

bool
operator <= (property p1, property p2) {
  for (int i=0; i<min(N(p1),N(p2)); i++)
    if (p1[i] < p2[i]) return true;
    else if (p2[i] < p1[i]) return false;
  return N(p1) <= N(p2);
}

properties
tmfs_list_heads_inside (url u, string prj) {
  strings ids= as_strings (tmfs_get_values (seq ("tag", "?id", prj, "head")));
  properties r;
  for (int i=0; i<N(ids); i++) {
    string id= ids[i];
    collection pl= tmfs_get_values (seq ("mirror", id, prj, "?delta"));
    if (N(pl) > 0) {
      url v= first (pl);
      if (u == url_here () || descends (v, u))
	r << seq (as_string (v), id);
    }
  }
  merge_sort (r);
  return r;
}

void
tmfs_export (url prj_dir, url u, string prj) {
  properties ps= tmfs_list_heads_inside (u, prj);
  for (int i=0; i<N(ps); i++) {
    url    v = ps[i][0];
    string id= ps[i][1];
    cout << "Process " << (prj_dir * v) << "\n";
    if (!exists (prj_dir * v)) {
      string val= tmfs_load_file (id);
      create_parents (prj_dir * v);
      save_string (prj_dir * v, val);
      cout << "Export " << id << " -> " << (prj_dir * v) << "\n";
    }
    else {
      string val1= load_string (prj_dir * v);
      string val2= tmfs_load_file (id);
      if (val2 != val1) {
	save_string (prj_dir * v, val2);
	cout << "Update " << id << " -> " << (prj_dir * v) << "\n";
      }
    }
  }
}

void
tmfs_import (url prj_dir, url u, string prj) {
  cout << "Process " << (prj_dir * u) << "\n";
  if (is_or (u)) {
    tmfs_import (prj_dir, u[1], prj);
    tmfs_import (prj_dir, u[2], prj);
  }
  else if (is_directory (prj_dir * u)) {
    bool flag;
    array<string> a= read_directory (prj_dir * u, flag);
    if (flag) return;
    for (int i=0; i<N(a); i++)
      if (!is_cruft (a[i]) && a[i] != "." && a[i] != "..")
	tmfs_import (prj_dir, u * a[i], prj);
  }
  else if (is_regular (prj_dir * u)) {
    string loc= as_string (u);
    solutions  sols= tmfs_query (seq ("mirror", "?id", prj, loc));
    collection ids = tmfs_get_values (sols, seq ("tag", "?id", prj, "head"));
    if (N (ids) == 0) {
      string val = load_string (prj_dir * u);
      string name= create_name (as_string (tail (u)), val);
      string id  = tmfs_create_file (name, val, prj);
      tmfs_set_property (seq ("mirror", id, prj, loc));
      cout << "Import " << u << " -> " << id << "\n";
    }
    else {
      string id  = first (ids);
      string val1= tmfs_load_file (id);
      string val2= load_string (prj_dir * u);
      if (val1 == val2) return;
      id= tmfs_update_file (id, val2);
      cout << "Update " << u << " -> " << id << "\n";
    }
  }

  properties ps= tmfs_list_heads_inside (u, prj);
  for (int i=0; i<N(ps); i++) {
    url    v = ps[i][0];
    string id= ps[i][1];
    if (!exists (prj_dir * v)) {
      tmfs_reset_property (seq ("tag", id, prj, "head"));
      tmfs_reset_property (seq ("mirror", id, prj, as_string (v)));
      cout << "Remove " << v << " -> " << id << "\n";
    }
  }
}
*/

/******************************************************************************
* Importation based on home directories for users and projects
******************************************************************************/

/*
void
tmfs_set_home (string prj, url u) {
  url old_home= tmfs_get_home (prj);
  if (!is_none (old_home))
    tmfs_reset_property (seq ("home", prj, as_string (old_home)));
  tmfs_set_property (seq ("home", prj, as_string (u)));
}

url
tmfs_get_home (string prj) {
  collection c= tmfs_get_values (seq ("home", prj, "?url"));
  if (N(c) != 1) return url_none ();
  return url (first (c));
}

collection
tmfs_get_projects (url u) {
  collection c= tmfs_get_values (seq ("home", "?prj", as_string (u)));
  if (N(c) != 0 || is_root (u)) return c;
  return tmfs_get_projects (url_parent (u));
}

url
delta_dir (url base, url sub) {
  return url_parent (delta (base * "dummy1", sub * "dummy2"));
}

void
tmfs_import (url u) {
  strings prjs= as_strings (tmfs_get_projects (u));
  if (N(prjs) == 0)
    cerr << "TeXmacs] error: no project for " << u << "\n";
  else if (N(prjs) > 1)
    cerr << "TeXmacs] error: too many projects for " << u << "\n";
  else {
    url prj_dir= tmfs_get_home (prjs[0]);
    tmfs_import (prj_dir, delta_dir (prj_dir, u), prjs[0]);
  }
}

void
tmfs_export (url u) {
  strings prjs= as_strings (tmfs_get_projects (u));
  if (N(prjs) == 0)
    cerr << "TeXmacs] error: no project for " << u << "\n";
  else if (N(prjs) > 1)
    cerr << "TeXmacs] error: too many projects for " << u << "\n";
  else {
    url prj_dir= tmfs_get_home (prjs[0]);
    tmfs_export (prj_dir, delta_dir (prj_dir, u), prjs[0]);
  }
}
*/
