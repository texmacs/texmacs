
/******************************************************************************
* MODULE     : tmfs_versioning.cpp
* DESCRIPTION: versioning for the TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"
#include "scheme.hpp"

/******************************************************************************
* Versioning
******************************************************************************/

string
tmfs_create_identifier () {
  return as_string (call ("create-unique-id"));
}

string
tmfs_create_ressource () {
  return tmfs_create_identifier ();
}

string
tmfs_create_version (string ressource) {
  string id  = tmfs_create_identifier ();
  string head= tmfs_get_head (ressource);
  tmfs_set_property (seq ("version", id, ressource));
  if (head != "") tmfs_set_property (seq ("next", head, id));
  return id;
}

void
tmfs_save_version_file (string version, string contents) {
  if (contents != "") tmfs_save (version, contents);
}

void
tmfs_save_version_properties (string version, properties props) {
  tmfs_save (version * "!", properties_encode (props));
}

string
tmfs_load_version_file (string version) {
  return tmfs_load (version);
}

properties
tmfs_load_version_properties (string version) {
  return properties_decode (tmfs_load (version * "!"));
}

void
tmfs_set_head (string ressource, string version) {
  string head= tmfs_get_head (ressource);
  if (ressource == head) return;
  if (head != "") {
    tmfs_reset_property (seq ("head", ressource, head));
    properties props= tmfs_load_version_properties (head);
    tmfs_reset_properties (props);
  }
  if (version != "") {
    tmfs_set_property (seq ("head", ressource, version));
    properties props= tmfs_load_version_properties (version);
    tmfs_set_properties (props);
  }
}

void
tmfs_reset_head (string ressource) {
  tmfs_set_head (ressource, "");
}

string
tmfs_get_head (string ressource) {
  solutions sols= tmfs_get_solutions (seq ("head", ressource, "?v"));
  collection c= as_collection (sols, "?v");
  if (N(c) == 0) return "";
  return first (c);
}

collection
tmfs_get_versions (string ressource) {
  solutions sols= tmfs_get_solutions (seq ("version", "?v", ressource));
  return as_collection (sols, "?v");
}

void
tmfs_save_ressource (string ressource, string contents, properties ps) {
  string version= tmfs_create_version (ressource);
  tmfs_save_version_file (version, contents);
  tmfs_save_version_properties (version, ps);
  tmfs_set_head (ressource, version);
}

string
tmfs_load_ressource_file (string ressource) {
  string version= tmfs_get_head (ressource);
  if (version == "") return "";
  return tmfs_load_version_file (version);
}

properties
tmfs_load_ressource_properties (string ressource) {
  string version= tmfs_get_head (ressource);
  if (version == "") return properties ();
  return tmfs_load_version_properties (version);
}
