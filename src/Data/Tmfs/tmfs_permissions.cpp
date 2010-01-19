
/******************************************************************************
* MODULE     : tmfs_permissions.cpp
* DESCRIPTION: permissions for reading, writing or modifying properties
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"

static string tmfs_user= "root";
static hashmap<string,bool> empty_map (false);
static hashmap<string,hashmap<string,bool> > tmfs_permissions (empty_map);
static hashmap<string,bool> tmfs_cycle_table (false);

string property_append (property p);

/******************************************************************************
* User management
******************************************************************************/

string
tmfs_create_user (string name) {
  if (N (tmfs_search_user (name)) != 0) return "";
  string user= tmfs_create_ressource ();
  string home= tmfs_create_file (name * " - home", "", user);
  properties ps;
  ps << seq ("user", user)
     << seq ("name", user, name)
     << seq ("owner", user, user)
     << seq ("in", user, user)
     << seq ("read", user, user)
     << seq ("write", user, user)
     << seq ("home", user, home);
  tmfs_save_ressource (user, "", ps);
  return user;
}

collection
tmfs_search_user (string name) {
  properties ps; ps << seq ("name", "?user", name) << seq ("user", "?user");
  return as_collection (tmfs_get_solutions (ps), "?user");
}

void
tmfs_set_user (string user) {
  tmfs_user= user;
}

string
tmfs_get_user () {
  return tmfs_user;
}

/******************************************************************************
* Determine permissions
******************************************************************************/

bool tmfs_allows (string id, string type, string user);

bool
tmfs_allows_via (string id, string type, string user, string via) {
  (void) id;
  //cout << "Allows? " << id << ", " << type << ", " << user
  //<< " via " << via << LF;
  if (user == via) return true;
  if (is_identifier (via)) return tmfs_allows (via, type, user);
  return false;
}

bool
tmfs_allows_compute (string id, string type, string user) {
  if (user == "root") return true;
  property query= seq (type, id, "?user");
  solutions sols= tmfs_get_solutions (query);
  strings a= as_strings (as_collection (sols, query));
  for (int i=0; i<N(a); i++)
    if (tmfs_allows_via (id, type, user, a[i]))
      return true;
  return false;
}

bool
tmfs_allows (string id, string type, string user) {
  string s= property_append (seq (id, user));
  if (!tmfs_permissions[type]->contains (s)) {
    //cout << "Allows? " << id << ", " << type << ", " << user << INDENT << LF;
    if (!tmfs_permissions->contains (type))
      tmfs_permissions (type)= hashmap<string,bool> (false);
    if (tmfs_cycle_table[s]) {
      //cout << UNINDENT << "Aborted" << LF;
      return false;
    }
    tmfs_cycle_table(s)= true;
    bool ok= tmfs_allows_compute (id, type, user);
    tmfs_permissions[type](s)= ok;
    tmfs_cycle_table(s)= false;
    //cout << UNINDENT << "Allows? " << id << ", " << type << ", " << user
    //<< " -> " << tmfs_permissions[type][s] << LF;
  }
  return tmfs_permissions[type][s];
}

/******************************************************************************
* Permission property and solution permissions
******************************************************************************/

bool
tmfs_allows (string id, string type) {
  return tmfs_allows (id, type, tmfs_user);
}

bool
tmfs_allows (property p, string type) {
  for (int i=0; i<N(p); i++)
    if (is_identifier (p[i]) && !tmfs_allows (p[i], type))
      return false;
  return true;
}

bool
tmfs_allows (solution sol, string type) {
  iterator<string> it= iterate (sol);
  while (it->busy ()) {
    string s= sol[it->next ()];
    if (is_identifier (s) && !tmfs_allows (s, type))
      return false;
  }
  return true;
}

strings
tmfs_filter (strings ss, string type) {
  strings r;
  for (int i=0; i<N(ss); i++)
    if (tmfs_allows (ss[i], type))
      r << ss[i];
  return r;
}

properties
tmfs_filter (properties ps, string type) {
  properties qs;
  for (int i=0; i<N(ps); i++)
    if (tmfs_allows (ps[i], type))
      qs << ps[i];
  return qs;
}

solutions
tmfs_filter (solutions sols, string type) {
  solutions r;
  for (int i=0; i<N(sols); i++)
    if (tmfs_allows (sols[i], type))
      r << sols[i];
  return r;
}

/******************************************************************************
* Setting attributes (i.e. properties with checked permissions)
******************************************************************************/

void
tmfs_set_attributes (string ressource, properties ps) {
  if (tmfs_allows (ressource, "owner"))
    tmfs_save_ressource (ressource, tmfs_load_ressource_file (ressource), ps);
}

properties
tmfs_get_attributes (string ressource) {
  if (tmfs_allows (ressource, "read"))
    return tmfs_load_ressource_properties (ressource);
  return properties ();
}

void
tmfs_add_attributes (string ressource, properties add_ps) {
  properties ps= tmfs_get_attributes (ressource);
  ps= reset (ps, add_ps);
  ps << add_ps;
  tmfs_set_attributes (ressource, ps);
}

void
tmfs_remove_attributes (string ressource, properties sub_ps) {
  properties ps= tmfs_get_attributes (ressource);
  ps= reset (ps, sub_ps);
  tmfs_set_attributes (ressource, ps);
}

void
tmfs_change_attributes (string ressource, properties mod_ps) {
  properties ps= tmfs_get_attributes (ressource);
  ps= reset (ps, widen (mod_ps));
  ps << mod_ps;
  tmfs_set_attributes (ressource, ps);
}

/******************************************************************************
* Querying properties with permission checking
******************************************************************************/

solutions
tmfs_query (property query) {
  if (!tmfs_allows (query, "read")) return solutions ();
  solutions sols= tmfs_get_solutions (query);
  return tmfs_filter (sols, "read");
}

collection
tmfs_query (property query, string unknown) {
  return as_collection (tmfs_query (query), unknown);
}

solutions
tmfs_query (properties queries) {
  if (N(tmfs_filter (queries, "read")) != N(queries)) return solutions ();
  solutions sols= tmfs_get_solutions (queries);
  return tmfs_filter (sols, "read");
}

collection
tmfs_query (properties queries, string unknown) {
  return as_collection (tmfs_query (queries), unknown);
}
