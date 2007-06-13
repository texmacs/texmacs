
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
#include "Scheme/object.hpp"

/******************************************************************************
* New identifier
******************************************************************************/

string
tmfs_new_identifier () {
  return as_string (call ("create-unique-id"));
}

/******************************************************************************
* Copy properties from old version
******************************************************************************/

property
substitute (property p, string what, string by) {
  property q= copy (p);
  for (int i=0; i<N(p); i++)
    if (p[i] == what) q[i]= by;
  return q;
}

properties
substitute (properties ps, string what, string by) {
  properties qs;
  for (int i=0; i<N(ps); i++)
    qs << substitute (ps[i], what, by);
  return qs;
}

void
tmfs_copy_attributes (string old_id, string new_id,
		      collection except= collection ())
{
  properties ps= tmfs_get_attributes (old_id);
  properties qs;
  for (int i=0; i<N(ps); i++)
    if (N(ps[i]) > 1 && !(except[ps[i][0]] > 0))
      qs << ps;
  tmfs_set_attributes (substitute (qs, old_id, new_id));
}

/******************************************************************************
* Versioning routines
******************************************************************************/

int
tmfs_get_revision (string id) {
  collection c= tmfs_get_property_value (seq ("revision", id, "?rev"));
  if (N(c) != 1) return 0;
  return as_int (first (c));
}

string
tmfs_new_revision (string contents, int revision) {
  string new_id= tmfs_new_identifier ();
  tmfs_save (new_id, contents);
  tmfs_set_property (seq ("head", new_id));
  tmfs_set_property (seq ("revision", new_id, as_string (revision)));
  return new_id;
}

string
tmfs_new (string contents) {
  string main_id= tmfs_new_identifier ();
  string new_id= tmfs_new_revision (contents, 1);
  tmfs_set_attribute (seq ("version", new_id, main_id));
  return new_id;
}

string
tmfs_update (string old_id, string contents) {
  int revision= tmfs_get_revision (old_id);
  string new_id= tmfs_new_revision (contents, revision + 1);
  tmfs_copy_attributes (old_id, new_id);
  tmfs_set_property (seq ("update", old_id, new_id));
  tmfs_reset_property (seq ("head", old_id));
  return new_id;
}

/******************************************************************************
* Projects and branches
******************************************************************************/

void
tmfs_set_project (string id, string prj) {
  tmfs_set_attribute (seq ("in", id, prj));
}

void
tmfs_reset_project (string id, string prj) {
  tmfs_reset_attribute (seq ("in", id, prj));
}

strings
tmfs_get_projects (string id) {
  return as_strings (tmfs_get_attribute_value (seq ("in", id, "?prj")));
}

strings
tmfs_get_project_heads (string prj) {
  solutions sols= tmfs_get_property (seq ("in", "?id", prj));
  collection c= tmfs_get_property_value (sols, seq ("head", "?id"));
  return as_strings (c);
}

void
tmfs_new_branch (string old_prj, string new_prj) {
  strings ids= as_strings (tmfs_get_project_heads (old_prj));
  for (int i=0; i<N(ids); i++) {
    string old_id= ids[i];
    string new_id= tmfs_new_revision (tmfs_load (old_id), 1);
    tmfs_copy_attributes (old_id, new_id, singleton ("in"));
    tmfs_set_project (new_id, new_prj);
    tmfs_set_property (seq ("branch", old_prj, new_prj));
    tmfs_set_property (seq ("branch", old_id, new_id));
  }
}
