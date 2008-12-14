
/******************************************************************************
* MODULE     : tmfs_file.cpp
* DESCRIPTION: files and projects
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Versioning routines
******************************************************************************/

string
tmfs_create_file (string name, string contents, string user, properties xps) {
  string master= tmfs_create_identifier ();
  string file  = tmfs_create_ressource ();
  properties ps;
  ps << seq ("file", file)
     << seq ("name", file, name)
     << seq ("master", file, master)
     << seq ("owner", file, user)
     << seq ("in", file, user)
     << seq ("read", file, user)
     << seq ("write", file, user)
     << substitute (xps, "self", file);
  tmfs_save_ressource (file, contents, ps);
  return file;
}

collection
tmfs_search_file (string name) {
  properties ps; ps << seq ("name", "?file", name) << seq ("file", "?file");
  return tmfs_query (ps, "?file");
}

void
tmfs_save (string file, string contents, properties ps) {
  if (tmfs_allows (file, "write") && tmfs_allows (file, "owner"))
    tmfs_save_ressource (file, contents, ps);
}

void
tmfs_save_file (string file, string cont) {
  if (tmfs_allows (file, "write"))
    tmfs_save_ressource (file, cont, tmfs_load_ressource_properties (file));
}

string
tmfs_load_file (string file) {
  if (tmfs_allows (file, "read"))
    return tmfs_load_ressource_file (file);
  return "";
}

/******************************************************************************
* Projects
******************************************************************************/

string
tmfs_create_project (string name, string user) {
  string project= tmfs_create_ressource ();
  string home= tmfs_create_file (name * " - home", "", project);
  properties ps;
  ps << seq ("project", project)
     << seq ("name", project, name)
     << seq ("owner", project, user)
     << seq ("in", project, user)
     << seq ("read", project, user)
     << seq ("write", project, user)
     << seq ("home", project, home);
  tmfs_save_ressource (project, "", ps);
  return project;
}

collection
tmfs_search_project (string name) {
  properties ps; ps << seq ("name", "?p", name) << seq ("project", "?p");
  return tmfs_query (ps, "?p");
}

collection
tmfs_get_file_projects (string file) {
  return tmfs_query (seq ("in", file, "?project"), "?project");
}

collection
tmfs_get_project_files (string project) {
  properties ps; ps << seq ("in", "?file", project) << seq ("file", "?file");
  collection files= tmfs_query (ps, "?file");
  collection homes= tmfs_query (seq ("home", project, "?home"), "?home");
  return files * invert (homes);
}

/******************************************************************************
* Branches
******************************************************************************/

void
tmfs_branch_file (string old_file, string branch) {
  string contents= tmfs_load_file (old_file);
  properties ps  = tmfs_get_attributes (old_file);
  string new_file= tmfs_create_ressource ();
  ps= substitute (ps, old_file, new_file);
  properties mp;
  mp << seq ("owner", old_file, "?user")
     << seq ("in", old_file, "?user")
     << seq ("read", old_file, "?user")
     << seq ("write", old_file, "?user");
  ps= reset (ps, mp);
  ps << seq ("owner", new_file, branch)
     << seq ("in", new_file, branch)
     << seq ("read", new_file, branch)
     << seq ("write", new_file, branch);
  tmfs_save_ressource (new_file, contents, ps);
}

string
tmfs_create_branch (string project, string name, string user) {
  collection c= tmfs_query (seq ("name", project, "?name"), "?name");
  if (N(c) == 0) return "";
  string full_name= first (c) * " - " * name;
  string branch= tmfs_create_project (full_name, user);
  strings files= as_strings (tmfs_get_project_files (project));
  for (int i=0; i<N(files); i++)
    tmfs_branch_file (files[i], branch);
  return branch;
}
