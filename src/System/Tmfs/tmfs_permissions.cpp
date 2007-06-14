
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

static string tmfs_user= "root";
static hashmap<string,bool> empty_map (false);
static hashmap<string,hashmap<string,bool> > tmfs_permissions (empty_map);

string property_encode (property p);

/******************************************************************************
* Determine permissions
******************************************************************************/

bool tmfs_allows (string id, string type, string who);

bool
tmfs_allows_via (string id, string type, string who, string via) {
  if (who == via) return true;
  if (is_identifier (via)) return tmfs_allows (id, type, via);
  return false;
}

bool
tmfs_allows_compute (string id, string type, string who) {
  if (who == "root") return true;
  property query= seq (type, id, "?who");
  solutions sols= tmfs_raw_get_solutions (query);
  strings a= as_strings (as_collection (sols, query));
  for (int i=0; i<N(a); i++)
    if (tmfs_allows_via (id, type, who, a[i]))
      return true;
  return false;
}

bool
tmfs_allows (string id, string type, string who) {
  string s= property_encode (seq (id, who));
  if (!tmfs_permissions[type]->contains (s)) {
    if (!tmfs_permissions->contains (type))
      tmfs_permissions (type)= hashmap<string,bool> (false);
    tmfs_permissions[type](s)= tmfs_allows_compute (id, type, who);
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
