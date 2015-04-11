
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
#define DB_MAX_TIME ((db_time) 10675199166)

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
typedef db_atoms db_query;

class database_rep: public concrete_struct {
private:
  url db_name;
  array<db_line> db;

  hashmap<string,db_atom> atom_encode;
  array<string> atom_decode;

  array<db_line_nrs> id_lines;
  array<db_line_nrs> val_lines;
  db_atoms ids_list;
  hashset<db_atom> ids_set;

  bool error_flag;
  string loaded;
  string pending;

public:
  db_atom as_atom (string s);
  string as_string (db_atom a);
  db_atoms as_atoms (strings s);
  strings as_strings (db_atoms a);
  tree as_tuple (db_atoms a);
  db_atoms entry_as_atoms (tree t);
  tree entry_from_atoms (db_atoms pairs);

private:
  db_line_nr extend_field (db_atom id, db_atom attr, db_atom vals, db_time t);
  bool line_satisfies (db_line_nr nr, db_query q, db_time t);
  bool id_satisfies (db_atom id, db_query q, db_time t);
  db_query encode_query (tree q);
  db_atoms filter (db_atoms ids, tree qt, db_time t, int limit);
  int compute_complexity (tree q);
  int ansatz_index (tree q);
  db_atoms ansatz (tree ql, db_time t);

private:
  void notify_created_atom (string s);
  void notify_extended_field (db_line_nr nr);
  void notify_removed_field (db_line_nr nr);
  void replay (string s);
  void initialize ();
  void purge ();

public:
  database_rep (url u);

  void set_field (db_atom id, db_atom attr, db_atoms vals, db_time t);
  db_atoms get_field (db_atom id, db_atom attr, db_time t);
  void remove_field (db_atom id, db_atom attr, db_time t);
  db_atoms get_attributes (db_atom id, db_time t);
  void set_entry (db_atom id, db_atoms pairs, db_time t);
  db_atoms get_entry (db_atom id, db_time t);
  void remove_entry (db_atom id, db_time t);
  db_atoms query (tree qt, db_time t, int limit);

  friend class database;
  friend void sync_databases ();
};

class database {
  CONCRETE(database);
  database ();
  database (url u);
};
CONCRETE_CODE(database);

void set_field (url u, string id, string attr, strings vals, db_time t);
strings get_field (url u, string id, string attr, db_time t);
void remove_field (url u, string id, string attr, db_time t);
strings get_attributes (url u, string id, db_time t);
void set_entry (url u, string id, tree e, db_time t);
tree get_entry (url u, string id, db_time t);
void remove_entry (url u, string id, db_time t);
strings query (url u, tree q, db_time t, int limit);

void sync_databases ();

#endif // defined DATABASE_H
