
/******************************************************************************
* MODULE     : tmfs_versioning.cpp
* DESCRIPTION: versioning for the TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Versioning routines
******************************************************************************/

int
tmfs_get_revision (string id) {
  collection c= tmfs_get_values (seq ("revision", id, "?rev"));
  if (N(c) != 1) return 0;
  return as_int (first (c));
}

string
tmfs_create_file (string name, string contents, string user) {
  string main_id= tmfs_create_ressource ("file", user);
  string new_id = tmfs_create_ressource ("revision", user);
  tmfs_set_property (seq ("name", main_id, name));
  tmfs_set_property (seq ("version", new_id, main_id));
  tmfs_set_property (seq ("revision", new_id, "1"));
  tmfs_set_property (seq ("tag", new_id, user, "head"));
  tmfs_save (new_id, contents);
  return new_id;
}

string
tmfs_create_similar_file (string name, string contents, string old_id) {
  string main_id= tmfs_create_similar_ressource ("file", old_id);
  string new_id = tmfs_create_similar_ressource ("revision", old_id);
  tmfs_set_property (seq ("name", main_id, name));
  tmfs_set_property (seq ("version", new_id, main_id));
  tmfs_set_property (seq ("revision", new_id, "1"));
  solutions sols= tmfs_get_solutions (seq ("in", old_id, "?project"));
  property p= seq ("tag", new_id, "?project", "head");
  tmfs_set_properties (substitute (p, sols));
  tmfs_save (new_id, contents);
  return new_id;
}

collection
tmfs_search_file (string name) {
  solutions sols= tmfs_get_solutions (seq ("name", "?file", name));
  return tmfs_get_values (sols, seq ("file", "?file"));
}

collection
tmfs_search_head (string name) {
  solutions sols= tmfs_get_solutions (seq ("name", "?file", name));
  sols= tmfs_get_solutions (sols, seq ("file", "?file"));
  sols= tmfs_get_solutions (sols, seq ("version", "?rev", "?file"));
  sols= tmfs_get_solutions (sols, seq ("tag", "?rev", "?prj", "head"));
  return as_collection (sols, "?rev");
}

string
tmfs_update_file (string old_id, string contents) {
  if (!tmfs_allows (old_id, "read")) return "";
  if (!tmfs_allows (old_id, "write")) return "";
  int revision = tmfs_get_revision (old_id);
  string new_id= tmfs_create_similar_ressource ("revision", old_id);
  tmfs_set_property (seq ("revision", new_id, as_string (revision + 1)));
  tmfs_set_property (seq ("update", old_id, new_id));
  properties props= tmfs_get_properties (old_id);
  strings a;
  a << "owner" << "in" << "read" << "write"
    << "revision" << "tag" << "branch" << "update";
  props= exclude_types (props, as_collection (a));
  tmfs_set_properties (substitute (props, old_id, new_id));
  solutions sols= tmfs_get_solutions (seq ("tag", old_id, "?p", "head"));
  tmfs_set_properties (substitute (seq ("tag", new_id, "?p", "head"), sols));
  tmfs_reset_properties (substitute (seq ("tag", old_id, "?p", "head"), sols));
  tmfs_save (new_id, contents);
  return new_id;
}

string
tmfs_load_file (string id) {
  if (!tmfs_allows (id, "read")) return "";
  return tmfs_load (id);
}

/******************************************************************************
* Projects, snapshots and branches
******************************************************************************/

string
tmfs_create_project (string name) {
  string prj= tmfs_create_ressource ("project");
  tmfs_set_property (seq ("name", prj, name));
  return prj;
}

collection
tmfs_search_project (string name) {
  solutions sols= tmfs_get_solutions (seq ("name", "?prj", name));
  return tmfs_get_values (sols, seq ("project", "?prj"));
}

string
tmfs_create_snapshot (string name, string prj) {
  string shot= tmfs_create_ressource ("snapshot");
  tmfs_set_property (seq ("name", shot, name));
  collection ids= tmfs_get_heads (prj);
  iterator<string> it= iterate (ids);
  while (it->busy ())
    tmfs_set_property (seq ("tag", it->next (), prj, shot));
  return shot;
}

collection
tmfs_search_snapshot (string name) {
  solutions sols= tmfs_get_solutions (seq ("name", "?shot", name));
  return tmfs_get_values (sols, seq ("snapshot", "?shot"));
}

collection
tmfs_get_projects (string id) {
  return tmfs_get_values (seq ("in", id, "?prj"));
}

collection
tmfs_get_heads (string prj) {
  return tmfs_get_values (seq ("tag", "?id", prj, "head"));
}

void
tmfs_branch_file (string old_id, string prj) {
  string new_id= tmfs_create_ressource ("revision", prj);
  tmfs_set_property (seq ("revision", new_id, "1"));
  tmfs_set_property (seq ("branch", old_id, new_id));
  tmfs_set_property (seq ("tag", new_id, prj, "head"));
  properties props= tmfs_get_properties (old_id);
  strings a;
  a << "owner" << "in" << "read" << "write"
    << "revision" << "tag" << "branch" << "update";
  props= exclude_types (props, as_collection (a));
  tmfs_set_properties (substitute (props, old_id, new_id));
  tmfs_save (new_id, tmfs_load_file (old_id));
}

void
tmfs_branch_project (string old_prj, string new_prj) {
  tmfs_set_property (seq ("branch", old_prj, new_prj));
  strings ids= as_strings (tmfs_get_heads (old_prj));
  ids= tmfs_filter (ids, "read");
  for (int i=0; i<N(ids); i++)
    tmfs_branch_file (ids[i], new_prj);
}
