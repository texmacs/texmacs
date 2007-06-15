
/******************************************************************************
* MODULE     : tmfs.hpp
* DESCRIPTION: the TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TMFS_H
#define TMFS_H
#include "disk_table.hpp"

typedef array<string> strings;
typedef array<string> property;
typedef array<property> properties;
typedef hashmap<string,string> solution;
typedef array<solution> solutions;

inline bool is_unknown (string s) { return N(s)>1 && s[0] == '?'; }
inline bool is_identifier (string s) { return N(s)>1 && s[0] == '+'; }

strings seq (string s1);
strings seq (string s1, string s2);
strings seq (string s1, string s2, string s3);
strings seq (string s1, string s2, string s3, string s4);
collection singleton (string s, int eps= 1);
string first (collection s);
collection filter (collection c, bool file_flag);
collection simplify (collection c);
collection invert (collection s);
void merge (collection& s1, collection s2);
collection operator * (collection c1, collection c2);
int total_size (collection s);
transaction atom (string key, collection val);
void add (transaction& t, string key, string val, int eps);
transaction filter (transaction t, bool file_flag);
transaction simplify (transaction t);
transaction invert (transaction t);
void merge (transaction& t1, transaction t2);
transaction operator * (transaction t1, transaction t2);
int total_size (transaction t);
collection as_collection (strings a);
strings as_strings (collection ss);
collection as_collection (tree t);
tree as_tree (collection ss);
strings as_strings (tree t);
tree as_tree (strings a);
tree as_tree (solutions sols);

void tmfs_write (transaction t);
transaction tmfs_read (collection c);
void tmfs_set (string key, collection val);
void tmfs_set (string key, string val);
void tmfs_reset (string key, collection val);
void tmfs_reset (string key, string val);
collection tmfs_get (string key);
void tmfs_save (string key, string val);
void tmfs_remove (string key);
string tmfs_load (string key);

property substitute (property p, string what, string by);
properties substitute (properties ps, string what, string by);
property substitute (property p, solution sol);
properties substitute (property p, solutions sols);
properties exclude_types (properties ps, collection c);
collection as_collection (solutions sols, string key);
collection as_collection (solutions sols, property p);
void tmfs_raw_set_property (property p);
void tmfs_raw_set_properties (properties ps);
void tmfs_raw_reset_property (property p);
collection tmfs_raw_get_values (property query);
solutions tmfs_raw_get_solutions (property query);
properties tmfs_raw_get_matches (property query);
solutions tmfs_raw_get_solutions (solutions sols, property ps);

string tmfs_create_identifier ();
string tmfs_create_user (string name);
collection tmfs_search_user (string name);
void tmfs_set_user (string user);
string tmfs_get_user ();
bool tmfs_allows (string id, string type);
bool tmfs_allows (property p, string type);
bool tmfs_allows (solution sol, string type);
strings tmfs_filter (strings ss, string type);
properties tmfs_filter (properties ps, string type);
solutions tmfs_filter (solutions sols, string type);
collection tmfs_get_permissions (string id, string type);
void tmfs_set_permissions (string id, string type, collection users);
void tmfs_set_permissions (string id, collection users);
void tmfs_set_similar_permissions (string id, string type, string similar_id);
void tmfs_set_similar_permissions (string id, string similar_id);
void tmfs_set_property (property p);
void tmfs_set_properties (properties ps);
void tmfs_reset_property (property p);
void tmfs_reset_properties (properties ps);
solutions tmfs_get_solutions (property query);
solutions tmfs_get_solutions (solutions sols, property query);
properties tmfs_get_matches (property query);
properties tmfs_get_matches (solutions sols, property query);
collection tmfs_get_values (property query);
collection tmfs_get_values (solutions sols, property query);
properties tmfs_get_properties (string id);
string tmfs_create_ressource (string type, string user= tmfs_get_user ());
string tmfs_create_similar_ressource (string type, string old_id);

string tmfs_create_file (string name, string c, string user= tmfs_get_user ());
string tmfs_create_similar_file (string name, string contents, string old_id);
collection tmfs_search_file (string name);
collection tmfs_search_head (string name);
string tmfs_update_file (string old_id, string contents);
string tmfs_load_file (string id);
string tmfs_create_project (string name);
collection tmfs_search_project (string name);
string tmfs_create_snapshot (string name, string prj);
collection tmfs_search_snapshot (string name);
collection tmfs_get_projects (string id);
collection tmfs_get_heads (string prj);
void tmfs_branch_file (string old_id, string prj);
void tmfs_branch_project (string old_prj, string new_prj);

void tmfs_import (url prj_dir, url u, string prj);
void tmfs_export (url prj_dir, url u, string prj);
void tmfs_set_home (string prj, url u);
url tmfs_get_home (string prj);
collection tmfs_get_projects (url u);
void tmfs_import (url u);
void tmfs_export (url u);

#endif // defined TMFS_H
