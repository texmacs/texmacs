
/******************************************************************************
* MODULE     : database.cpp
* DESCRIPTION: TeXmacs databases
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Database/database.hpp"
#include "hashset.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructors
******************************************************************************/

db_line_rep::db_line_rep (db_atom id2, db_atom attr2, db_atom val2,
                          db_time created2, db_time expires2):
  id (id2), attr (attr2), val (val2), created (created2), expires (expires2) {}

db_line::db_line () {
  rep= tm_new<db_line_rep> (0, 0, 0, DB_MAX_TIME, DB_MAX_TIME); }

db_line::db_line (db_atom id, db_atom attr, db_atom val,
                  db_time created, db_time expires) {
  rep= tm_new<db_line_rep> (id, attr, val, created, expires); }

database_rep::database_rep (url u, bool clone):
  db_name (u), db (), outdated (0), with_history (!clone),
  atom_encode (-1), atom_decode (),
  id_lines (), val_lines (), ids_list (), ids_set (),
  error_flag (false), loaded (""), pending (""),
  start_pending (0), time_stamp (0),
  key_encode (-1), key_decode (),
  atom_indexed (), key_occurrences (),
  key_completions (), name_completions ()
{
  if (is_none (db_name)) error_flag= false;
  else if (!clone) initialize ();
}

database::database () {
  rep= tm_new<database_rep> (url_none ()); };

database::database (url u, bool clone) {
  rep= tm_new<database_rep> (u, clone); };

/******************************************************************************
* Internal subroutines
******************************************************************************/

db_atom
database_rep::create_atom (string s) {
  if (!atom_encode->contains (s)) {
    db_atom code= (db_atom) N (atom_decode);
    atom_encode (s)= code;
    atom_decode << s;
    id_lines << db_line_nrs ();
    val_lines << db_line_nrs ();
    atom_indexed << false;
    name_indexed << false;
  }
  return atom_encode[s];
}

db_line_nr
database_rep::extend_field (db_atom id, db_atom attr, db_atom val, db_time t) {
  db_line_nr nr= (db_line_nr) N(db);
  db_line l (id, attr, val, t, DB_MAX_TIME);
  db << l;
  id_lines[id] << nr;
  val_lines[val] << nr;
  if (!ids_set->contains (id)) {
    ids_set->insert (id);
    ids_list << id;
  }
  string dec= atom_decode[attr];
  if (dec != "contributor") indexate (val);
  if (dec == "name") indexate_name (val);
  //cout << "l. " << nr << ":\t" << id << ", " << attr << ", " << val << LF;
  //cout << "l. " << nr << ":\t" << from_atom (id) << ", " << from_atom (attr) << ", " << from_atom (val) << LF;
  return nr;
}

/******************************************************************************
* Atom management
******************************************************************************/

bool
database_rep::atom_exists (string s) {
  return atom_encode->contains (s);
}

db_atom
database_rep::as_atom (string s) {
  if (atom_encode->contains (s)) return atom_encode[s];
  db_atom r= create_atom (s);
  notify_created_atom (s);
  return r;
}

string
database_rep::from_atom (db_atom a) {
  ASSERT (a < N(atom_decode), "Invalid atom");
  return atom_decode[a];
}

db_atoms
database_rep::as_atoms (strings s) {
  db_atoms r;
  for (int i=0; i<N(s); i++)
    r << as_atom (s[i]);
  return r;
}

strings
database_rep::from_atoms (db_atoms a) {
  strings r;
  for (int i=0; i<N(a); i++)
    r << from_atom (a[i]);
  return r;
}

tree
database_rep::as_tuple (db_atoms a) {
  tree r (TUPLE);
  for (int i=0; i<N(a); i++)
    r << scm_quote (from_atom (a[i]));
  return r;
}

db_atoms
database_rep::entry_as_atoms (tree t) {
  db_atoms r;
  if (is_tuple (t))
    for (int i=0; i<N(t); i++)
      if (is_tuple (t[i]) && N(t[i]) >= 2)
        for (int j=1; j<N(t[i]); j++)
          if (is_atomic (t[i][0]) && is_atomic (t[i][j]))
            r << as_atom (scm_unquote (t[i][0]->label))
              << as_atom (scm_unquote (t[i][j]->label));
  return r;
}

tree
database_rep::entry_from_atoms (db_atoms pairs) {
  array<db_atoms> a;
  for (int i=0; i<N(pairs); i+=2) {
    int found= -1;
    for (int j=0; j<N(a); j++)
      if (a[j][0] == pairs[i]) {
        found= j;
        break;
      }
    if (found >= 0) a[found] << pairs[i+1];
    else {
      db_atoms v;
      v << pairs[i] << pairs[i+1];
      a << v;
    }
  }
  tree r (TUPLE);
  for (int i=0; i<N(a); i++)
    r << as_tuple (a[i]);
  return r;
}

/******************************************************************************
* Basic database operations
******************************************************************************/

void
database_rep::set_field (db_atom id, db_atom attr, db_atoms vals, db_time t) {
  remove_field (id, attr, t);
  for (int i=0; i<N(vals); i++) {
    db_line_nr nr= extend_field (id, attr, vals[i], t);
    notify_extended_field (nr);
  }
}

db_atoms
database_rep::get_field (db_atom id, db_atom attr, db_time t) {
  db_atoms r;
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if (l->attr == attr && ((t == 0) || (l->created <= t && t < l->expires)))
      r << l->val;
  }
  return r;
}

void
database_rep::remove_field (db_atom id, db_atom attr, db_time t) {
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if (l->attr == attr && l->expires == DB_MAX_TIME) {
      l->expires= t;
      notify_removed_field (nrs[i]);
      outdated++;
    }
  }
}

db_atoms
database_rep::get_attributes (db_atom id, db_time t) {
  hashset<db_atom> done;
  db_atoms r;
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if ((t == 0) || (l->created <= t && t < l->expires))
      if (!done->contains (l->attr)) {
        done->insert (l->attr);
        r << l->attr;
      }
  }
  return r;
}

void
database_rep::set_entry (db_atom id, db_atoms pairs, db_time t) {
  remove_entry (id, t);
  for (int i=0; i<N(pairs); i+=2) {
    db_atom attr= pairs[i];
    db_atom val = pairs[i+1];
    db_line_nr nr= extend_field (id, attr, val, t);
    notify_extended_field (nr);
  }
}

db_atoms
database_rep::get_entry (db_atom id, db_time t) {
  db_atoms r;
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if ((t == 0) || (l->created <= t && t < l->expires))
      r << l->attr << l->val;
  }
  return r;
}

void
database_rep::remove_entry (db_atom id, db_time t) {
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if (l->expires == DB_MAX_TIME) {
      l->expires= t;
      notify_removed_field (nrs[i]);
      outdated++;
    }
  }
}

void
database_rep::inspect_history (db_atom name) {
  db_line_nrs nrs= val_lines[name];
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if (from_atom (l->attr) == "name")
      cout << from_atom (l->id) << ", name, " << from_atom (l->val) << ", "
           << ((long int) l->created) << ", " << ((long int) l->expires) << LF;
  }
}

/******************************************************************************
* User interface for basic operations
******************************************************************************/

array<database> dbs;
hashmap<tree,int> db_index;

database
get_database (url u) {
  check_for_updates ();
  if (!db_index->contains (u->t)) {
    db_index (u->t)= N(dbs);
    dbs << database (u);
  }
  return dbs [db_index [u->t]];
}

void
keep_history (url u, bool flag) {
  database db= get_database (u);
  bool sync= (db->with_history != flag);
  db->with_history= flag;
  if (sync) sync_databases ();
}

void
set_field (url u, string id, string attr, strings vals, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atom _attr= db->as_atom (attr);
  db_atoms _vals= db->as_atoms (vals);
  db->set_field (_id, _attr, _vals, t);
}

strings
get_field (url u, string id, string attr, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atom _attr= db->as_atom (attr);
  db_atoms _vals= db->get_field (_id, _attr, t);
  return db->from_atoms (_vals);
}

void
remove_field (url u, string id, string attr, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atom _attr= db->as_atom (attr);
  db->remove_field (_id, _attr, t);
}

strings
get_attributes (url u, string id, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atoms _attrs= db->get_attributes (_id, t);
  return db->from_atoms (_attrs);
}

void
set_entry (url u, string id, tree e, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atoms _pairs= db->entry_as_atoms (e);
  db->set_entry (_id, _pairs, t);
}

tree
get_entry (url u, string id, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db_atoms _pairs= db->get_entry (_id, t);
  return db->entry_from_atoms (_pairs);
}

void
remove_entry (url u, string id, db_time t) {
  database db= get_database (u);
  db_atom _id= db->as_atom (id);
  db->remove_entry (_id, t);
}

strings
query (url u, tree q, db_time t, int limit) {
  database db= get_database (u);
  db_atoms _ids= db->query (q, t, limit);
  return db->from_atoms (_ids);
}

void
inspect_history (url u, string name) {
  database db= get_database (u);
  if (db->atom_exists (name)) {
    db_atom _val= db->as_atom (name);
    db->inspect_history (_val);
  }
}

strings
get_completions (url u, string s) {
  database db= get_database (u);
  return db->compute_completions (s);
}

strings
get_name_completions (url u, string s) {
  database db= get_database (u);
  return db->compute_name_completions (s);
}
