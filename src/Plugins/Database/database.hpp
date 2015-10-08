
/******************************************************************************
* MODULE     : database.hpp
* DESCRIPTION: TeXmacs databases
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DATABASE_H
#define DATABASE_H
#include "url.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

/******************************************************************************
* Individual lines in databases
******************************************************************************/

typedef int db_atom;
typedef double db_time;
typedef array<db_atom> db_atoms;
#define DB_MAX_TIME ((db_time) 10675199166.0)

class db_line_rep: public concrete_struct {
public:
  db_atom id;
  db_atom attr;
  db_atom val;
  db_time created;
  db_time expires;

  db_line_rep (db_atom id, db_atom attr, db_atom val,
               db_time created, db_time expires);

  friend class database;
};

class db_line {
  CONCRETE(db_line);
  db_line ();
  db_line (db_atom id, db_atom attr, db_atom val,
           db_time created, db_time expires);
};
CONCRETE_CODE(db_line);

/******************************************************************************
* Databases
******************************************************************************/

typedef int db_line_nr;
typedef array<db_line_nr> db_line_nrs;
typedef array<string> strings;
typedef db_atoms db_constraint;
typedef array<db_constraint> db_constraints;
typedef int db_key;
typedef array<db_key> db_keys;

class database;
class database_rep: public concrete_struct {
private:
  url db_name;
  array<db_line> db;
  int outdated;
  bool with_history;

  hashmap<string,db_atom> atom_encode;
  array<string> atom_decode;
  array<db_line_nrs> id_lines;
  array<db_line_nrs> val_lines;
  db_atoms ids_list;
  hashset<db_atom> ids_set;

  bool error_flag;
  string loaded;
  string pending;
  int start_pending;
  int time_stamp;
  
  hashmap<string,db_atom> key_encode;
  array<string> key_decode;
  array<bool> atom_indexed;
  array<bool> name_indexed;
  array<db_atoms> key_occurrences;
  hashmap<string,db_keys> key_completions;
  hashmap<string,db_atoms> name_completions;

public:
  bool atom_exists (string s);
  db_atom as_atom (string s);
  string from_atom (db_atom a);
  db_atoms as_atoms (strings s);
  strings from_atoms (db_atoms a);
  tree as_tuple (db_atoms a);
  db_atoms entry_as_atoms (tree t);
  tree entry_from_atoms (db_atoms pairs);

private:
  db_atom create_atom (string s);
  db_line_nr extend_field (db_atom id, db_atom attr, db_atom vals, db_time t);
  bool line_satisfies (db_line_nr nr, db_constraint c, db_time t);
  bool id_satisfies (db_atom id, db_constraint c, db_time t);
  bool id_satisfies (db_atom id, db_constraints cs, db_time t);
  db_constraint encode_constraint (tree q);
  db_constraints encode_constraints (tree q);
  db_atoms filter (db_atoms ids, tree qt, db_time t, int limit);
  int compute_complexity (tree q);
  int ansatz_index (tree q);
  db_atoms ansatz (tree ql, db_time t);
  db_atoms filter_modified (db_atoms ids, db_time t1, db_time t2);

private:
  void notify_created_atom (string s);
  void notify_extended_field (db_line_nr nr);
  void notify_removed_field (db_line_nr nr);
  void replay (string s);
  void replay (database clone, int start, bool all);
  database compress ();
  void initialize ();
  void purge ();

private:
  db_key as_key (string s);
  string from_key (db_key a);
  void add_completed_as (db_key k);
  void indexate (db_atom val);
  void indexate_name (db_atom val);
  db_constraint encode_keywords_constraint (tree q);
  strings compute_completions (string s);
  strings compute_name_completions (string s);
  tree normalize_query (tree q);

private:
  array<strings> build_sort_tuples (db_atoms ids, db_atoms attrs, db_time t);
  db_atoms sort_results (db_atoms ids, tree q, db_time t);

public:
  database_rep (url u, bool clone= false);

  void set_field (db_atom id, db_atom attr, db_atoms vals, db_time t);
  db_atoms get_field (db_atom id, db_atom attr, db_time t);
  void remove_field (db_atom id, db_atom attr, db_time t);
  db_atoms get_attributes (db_atom id, db_time t);
  void set_entry (db_atom id, db_atoms pairs, db_time t);
  db_atoms get_entry (db_atom id, db_time t);
  void remove_entry (db_atom id, db_time t);
  db_atoms query (tree qt, db_time t, int limit);
  void inspect_history (db_atom name);

  friend void keep_history (url u, bool flag);
  friend void sync_databases ();
  friend void check_for_updates ();
  friend strings get_completions (url u, string s);
  friend strings get_name_completions (url u, string s);
};

class database {
  CONCRETE(database);
  database ();
  database (url u, bool clone= false);
};
CONCRETE_CODE(database);

void keep_history (url u, bool flag);
void set_field (url u, string id, string attr, strings vals, db_time t);
strings get_field (url u, string id, string attr, db_time t);
void remove_field (url u, string id, string attr, db_time t);
strings get_attributes (url u, string id, db_time t);
void set_entry (url u, string id, tree e, db_time t);
tree get_entry (url u, string id, db_time t);
void remove_entry (url u, string id, db_time t);
strings query (url u, tree q, db_time t, int limit);
void inspect_history (url u, string name);
strings get_completions (url u, string s);
strings get_name_completions (url u, string s);

void sync_databases ();
void check_for_updates ();

#endif // defined DATABASE_H
