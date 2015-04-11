
/******************************************************************************
* MODULE     : db_query.cpp
* DESCRIPTION: TeXmacs database queries
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Database/database.hpp"

/******************************************************************************
* Fast checking whether a line satisfies a query
******************************************************************************/

/*
bool
database_rep::line_satisfies (db_line_nr nr, db_query q, db_time t) {
  db_line& l= db[nr];
  if (t < l->created || t >= t->expires) return false;
  int pos= 0;
  while (pos < N(q)) {
    int n= q[pos++];
    int attr= q[pos++];
    if (l->attr != attr) return false;
    bool ok= false;
    for (int i=0; i<n; i++)
      ok= ok || l->val == q[pos+i];
    pos += n;
  }
  return true;
}

bool
database_rep::id_satisfies (db_atom id, db_query q, db_time t) {
  db_line_nrs nrs= id_lines[id];
  for (int i=0; i<N(nrs); i++)
    if (line_satisfies (nrs[i], q, t)) return true;
  return false;
}

db_query
query_failed () {
  db_query r;
  r << -1;
  return r;
}

db_query
database_rep::encode_query (tree q) {
  db_query r;
  if (!is_tuple (q)) query_failed ();
  for (int i=0; i<N(q); i++)
    if (is_tuple (q, "attr-vals")) {
      if (N(q) <= 2 || !is_atomic (q[1])) return query_failed ();
      if (!atom_encode->contains (q[1]->label)) return query_failed ();
      db_atoms ats= as_atoms (q, 1);
      if (N(ats) == 1) return query_failed ();
      r << (N(ats)-1) << ats;
    }
  return r;
}

db_atoms
database_rep::filter (db_atoms ids, tree qt, db_time t, int limit) {
  db_query q= encode_query (qt);
  if (N(q) == 1 && q[0] < 0) return db_atoms ();
  db_atoms r;
  for (int i=0; i<N(ids); i++)
    if (id_satisfies (ids[i], q, t)) {
      r << ids[i];
      if (N(r) >= limit) break;
    }
  return r;
}
*/

/******************************************************************************
* Fast checking whether a line satisfies a query
******************************************************************************/

/*
db_line_nrs
database_rep::filter_id (db_line_nrs nrs, db_atoms ids) {
  db_line_nrs r;
  for (int i=0; i<N(nrs); i++) {
    db_atom id= db[nrs[i]]->id;
    if (contains (id, ids)) r << nrs[i];
  }
  return r;
}

db_line_nrs
database_rep::filter_attrs (db_line_nrs nrs, db_atoms attrs) {
  db_line_nrs r;
  for (int i=0; i<N(nrs); i++) {
    db_atom attr= db[nrs[i]]->attr;
    if (contains (attr, attrs)) r << nrs[i];
  }
  return r;
}

db_line_nrs
database_rep::filter_val (db_line_nrs nrs, db_atoms vals) {
  db_line_nrs r;
  for (int i=0; i<N(nrs); i++) {
    db_atom val= db[nrs[i]]->val;
    if (contains (val, vals)) r << nrs[i];
  }
  return r;
}

db_line_nrs
database_rep::filter_time (db_line_nrs nrs, db_time t) {
  db_line_nrs r;
  for (int i=0; i<N(nrs); i++) {
    db_line& l= db[nrs[i]];
    if (l->created <= t && t < l->expires) r << nrs[i];
  }
  return r;
}

db_line_nrs
database_rep::filter_query (db_line_nrs nrs, tree q) {
  if (is_tuple (q, "id")) {
    db_atoms ids= query_args_as_atoms (q);
    if (N(ids) == 0) return db_line_nrs ();
    else return filter_id (nrs, ids);
  }
  else if (is_tuple (q, "attr")) {
    db_atoms attrs= query_args_as_atoms (q);
    if (N(attrs) == 0) return db_line_nrs ();
    else return filter_attr (nrs, attrs);
  }
  else if (is_tuple (q, "val")) {
    db_atoms vals= query_args_as_atoms (q);
    if (N(vals) == 0) return db_line_brs ();
    else return filter_val (nrs, vals);
  }
  else if (is_tuple (q, "time", 1) && is_int (q[1]))
    return filter_time (nrs, as_double (q[1]));
  else
    return db_line_nrs ();
}
*/

/******************************************************************************
* Estimating the complexity of a query
******************************************************************************/

/*
int
database_rep::complexity_id (db_atoms ids) {
  int r=0;
  for (int i=0; i<N(ids); i++)
    r += N (id_lines[ids[i]]);
  return r;
}

int
database_rep::complexity_attr (db_atoms attrs) {
  return 1000000000;
}

int
database_rep::complexity_val (db_atoms vals) {
  int r=0;
  for (int i=0; i<N(vals); i++)
    r += N (val_lines[vals[i]]);
  return r;
}

int
database_rep::complexity_time (db_time t) {
  return 1000000000;
}

int
database_rep::complexity_query (tree q) {
  if (is_tuple (q, "id"))
    return complexity_id (query_args_as_atoms (q));
  else if (is_tuple (q, "attr"))
    return complexity_attr (query_args_as_atoms (q));
  else if (is_tuple (q, "val"))
    return complexity_val (query_args_as_atoms (q));
  else if (is_tuple (q, "time", 1) && is_int (q[1]))
    return complexity_time (as_double (q[1]));
  else
    return 0;
}
*/

/******************************************************************************
* Performing a multiple query
******************************************************************************/

/*
db_line_nrs
database_rep::query (tree q) {
  if (!is_tuple (q)) return db_line_nrs ();
  int best  = -1;
  int lowest= 1000000000;
  for (int i=0; i<N(q); i++) {
    int c= complexity_query (q[i]);
    if (c < lowest) {
      best= i;
      lowest= c;
    }
  }
  if (lowest == 0) return db_line_nrs ();
  

  if (is_tuple (query, "id"))
    return complexity_id (query_args_as_atoms (query));
  else if (is_tuple (query, "attr"))
    return complexity_attr (query_args_as_atoms (query));
  else if (is_tuple (query, "val"))
    return complexity_val (query_args_as_atoms (query));
  else if (is_tuple (query, "time", 1) && is_int (query[1]))
    return complexity_time (as_double (query[1]));
  else
    return 0;
}
*/
