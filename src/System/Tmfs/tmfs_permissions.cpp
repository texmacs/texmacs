
/******************************************************************************
* MODULE     : tmfs_permissions.cpp
* DESCRIPTION: permissions for reading, writing or modifying properties
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "Scheme/object.hpp"

static string tmfs_user= "root";
static hashmap<string,bool> empty_map (false);
static hashmap<string,hashmap<string,bool> > tmfs_permissions (empty_map);
static hashmap<string,bool> tmfs_cycle_table (false);

string property_encode (property p);

/******************************************************************************
* User management
******************************************************************************/

string
tmfs_create_identifier () {
  return as_string (call ("create-unique-id"));
}

string
tmfs_create_user (string name) {
  if (N (tmfs_search_user (name)) != 0) return "";
  string id= tmfs_create_identifier ();
  tmfs_raw_set_property (seq ("user", id));
  tmfs_raw_set_property (seq ("name", id, name));
  tmfs_raw_set_property (seq ("owner", id, id));
  tmfs_raw_set_property (seq ("in", id, id));
  tmfs_raw_set_property (seq ("read", id, id));
  tmfs_raw_set_property (seq ("write", id, id));
  return id;
}

collection
tmfs_search_user (string name) {
  solutions sols= tmfs_raw_get_solutions (seq ("name", "?user", name));
  sols= tmfs_raw_get_solutions (sols, seq ("user", "?user"));
  sols= tmfs_filter (sols, "read");
  return as_collection (sols, "?user");
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
* Resources
******************************************************************************/

string
tmfs_create_ressource (string type, string user) {
  string id= tmfs_create_identifier ();
  tmfs_raw_set_property (seq (type, id));
  tmfs_raw_set_property (seq ("owner", id, user));
  tmfs_raw_set_property (seq ("in", id, user));
  tmfs_raw_set_property (seq ("read", id, user));
  tmfs_raw_set_property (seq ("write", id, user));
  return id;
}

string
tmfs_create_similar_ressource (string type, string old_id) {
  properties ps;
  string new_id= tmfs_create_identifier ();
  tmfs_raw_set_property (seq (type, new_id));
  ps= tmfs_raw_get_matches (seq ("owner", old_id, "?user"));
  tmfs_raw_set_properties (substitute (ps, old_id, new_id));
  ps= tmfs_raw_get_matches (seq ("in", old_id, "?user"));
  tmfs_raw_set_properties (substitute (ps, old_id, new_id));
  ps= tmfs_raw_get_matches (seq ("read", old_id, "?user"));
  tmfs_raw_set_properties (substitute (ps, old_id, new_id));
  ps= tmfs_raw_get_matches (seq ("write", old_id, "?user"));
  tmfs_raw_set_properties (substitute (ps, old_id, new_id));
  return new_id;
}

/******************************************************************************
* Determine permissions
******************************************************************************/

bool tmfs_allows (string id, string type, string user);

bool
tmfs_allows_via (string id, string type, string user, string via) {
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
  solutions sols= tmfs_raw_get_solutions (query);
  strings a= as_strings (as_collection (sols, query));
  for (int i=0; i<N(a); i++)
    if (tmfs_allows_via (id, type, user, a[i]))
      return true;
  return false;
}

bool
tmfs_allows (string id, string type, string user) {
  string s= property_encode (seq (id, user));
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
* Interface for setting and getting permissions
******************************************************************************/

collection
tmfs_get_permissions (string id, string type) {
  return tmfs_get_values (seq (type, id, "?user"));
}

void
tmfs_set_permissions (string id, string type, collection users) {
  if (tmfs_allows (id, "owner") && N(users) != 0) {
    tmfs_raw_reset_property (seq (type, id, "?user"));
    strings a= as_strings (users);
    for (int i=0; i<N(a); i++)
      tmfs_raw_set_property (seq (type, id, a[i]));
  }
}

void
tmfs_set_permissions (string id, collection users) {
  tmfs_set_permissions (id, "owner", users);
  tmfs_set_permissions (id, "in", users);
  tmfs_set_permissions (id, "read", users);
  tmfs_set_permissions (id, "write", users);
}

void
tmfs_set_similar_permissions (string id, string type, string similar_id) {
  tmfs_set_permissions (id, type, tmfs_get_permissions (similar_id, type));
}

void
tmfs_set_similar_permissions (string id, string similar_id) {
  tmfs_set_similar_permissions (id, "owner", similar_id);
  tmfs_set_similar_permissions (id, "in", similar_id);
  tmfs_set_similar_permissions (id, "read", similar_id);
  tmfs_set_similar_permissions (id, "write", similar_id);
}

/******************************************************************************
* Property setting and getting according to permissions
******************************************************************************/

void
tmfs_set_property (property p) {
  if (tmfs_allows (p, "owner")) {
    if (N(p) == 3) tmfs_permissions(p[0])= hashmap<string,bool> (false);
    tmfs_raw_set_property (p);
  }
}

void
tmfs_set_properties (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_set_property (ps[i]);
}

void
tmfs_reset_property (property p) {
  if (tmfs_allows (p, "owner")) {
    if (N(p) == 3) tmfs_permissions(p[0])= hashmap<string,bool> (false);
    tmfs_raw_reset_property (p);
  }
}

void
tmfs_reset_properties (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_reset_property (ps[i]);
}

solutions
tmfs_get_solutions (property query) {
  if (!tmfs_allows (query, "read")) return solutions ();
  solutions sols= tmfs_raw_get_solutions (query);
  return tmfs_filter (sols, "read");
}

solutions
tmfs_get_solutions (solutions sols1, property query) {
  if (!tmfs_allows (query, "read")) return solutions ();
  solutions sols= tmfs_raw_get_solutions (sols1, query);
  return tmfs_filter (sols, "read");
}

properties
tmfs_get_matches (property query) {
  return substitute (query, tmfs_get_solutions (query));
}

properties
tmfs_get_matches (solutions sols, property query) {
  return substitute (query, tmfs_get_solutions (query));
}

collection
tmfs_get_values (property query) {
  return as_collection (tmfs_get_solutions (query), query);
}

collection
tmfs_get_values (solutions sols, property query) {
  return as_collection (tmfs_get_solutions (sols, query), query);
}

properties
tmfs_get_properties (string id) {
  properties ps;
  ps << tmfs_get_matches (seq ("?a", id));
  ps << tmfs_get_matches (seq ("?a", id, "?b"));
  ps << tmfs_get_matches (seq ("?a", "?b", id));
  ps << tmfs_get_matches (seq ("?a", id, "?b", "?c"));
  ps << tmfs_get_matches (seq ("?a", "?b", id, "?c"));
  ps << tmfs_get_matches (seq ("?a", "?b", "?c", id));
  return ps;
}
