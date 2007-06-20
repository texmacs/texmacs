
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

// Collections
collection singleton (string s, int eps= 1);
string first (collection s);
collection filter (collection c, bool file_flag);
collection simplify (collection c);
collection invert (collection s);
void merge (collection& s1, collection s2);
collection operator * (collection c1, collection c2);
int total_size (collection s);
collection as_collection (strings a);
strings as_strings (collection ss);

// Transactions
transaction atom (string key, collection val);
void add (transaction& t, string key, string val, int eps);
transaction filter (transaction t, bool file_flag);
transaction simplify (transaction t);
transaction invert (transaction t);
void merge (transaction& t1, transaction t2);
transaction operator * (transaction t1, transaction t2);
int total_size (transaction t);

// Properties
strings seq (string s1);
strings seq (string s1, string s2);
strings seq (string s1, string s2, string s3);
strings seq (string s1, string s2, string s3, string s4);
property property_quote (property p);
property property_unquote (property p);
string property_append (property p);
property property_unappend (string s);
string properties_encode (properties ps);
properties properties_decode (string s);
bool matches (property p, property q);
properties reset (properties ps, property p);
properties reset (properties ps, properties qs);
property substitute (property p, string what, string by);
properties substitute (properties ps, string what, string by);
property substitute (property p, solution sol);
properties substitute (property p, solutions sols);
properties exclude_types (properties ps, collection c);
properties widen (properties ps);

// Solutions
collection as_collection (solutions sols, string key);
collection as_collection (solutions sols, property p);
solutions combine (solutions sols1, solutions sols2);
property simplify (property p, solutions sols);

// Low level disk access
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

// Property management
void tmfs_set_property (property p);
void tmfs_set_properties (properties ps);
void tmfs_reset_property (property p);
void tmfs_reset_properties (properties ps);
solutions tmfs_get_solutions (property p);
solutions tmfs_get_solutions (properties ps);

// Versioning of ressources with properties
string tmfs_create_identifier ();
string tmfs_create_ressource ();
string tmfs_create_version (string ressource);
void tmfs_save_version_file (string version, string contents);
void tmfs_save_version_properties (string version, properties props);
string tmfs_load_version_file (string version);
properties tmfs_load_version_properties (string version);
void tmfs_set_head (string ressource, string version);
void tmfs_reset_head (string ressource);
string tmfs_get_head (string ressource);
collection tmfs_get_versions (string ressource);
void tmfs_save_ressource (string ressource, string contents, properties ps);
string tmfs_load_ressource_file (string ressource);
properties tmfs_load_ressource_properties (string ressource);

// Permission management
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
void tmfs_set_attributes (string file, properties ps);
properties tmfs_get_attributes (string file);
void tmfs_add_attributes (string file, properties add_ps);
void tmfs_remove_attributes (string file, properties sub_ps);
void tmfs_change_attributes (string file, properties mod_ps);
solutions tmfs_query (property query);
collection tmfs_query (property query, string unknown);
solutions tmfs_query (properties queries);
collection tmfs_query (properties queries, string unknown);

// File and project management
string tmfs_create_file (string name, string c, string user= tmfs_get_user (),
			 properties xps= properties ());
string tmfs_create_similar_file (string name, string contents, string old_id);
collection tmfs_search_file (string name);
void tmfs_save_file (string file, string contents);
string tmfs_load_file (string file);
string tmfs_create_project (string name, string user= tmfs_get_user ());
collection tmfs_search_project (string name);
collection tmfs_get_file_projects (string file);
collection tmfs_get_project_files (string project);
void tmfs_branch_file (string old_file, string branch);
string tmfs_create_branch (string p, string name, string u= tmfs_get_user ());

// Compatability with conventional file systems
void tmfs_import (url prj_dir, url u, string prj);
void tmfs_export (url prj_dir, url u, string prj);
void tmfs_set_root (string prj, url u);
url tmfs_get_root (string prj);
collection tmfs_get_projects (url u);
void tmfs_import (url u);
void tmfs_export (url u);

// Server
void tmfs_start_server ();
string tmfs_server_read (int fd);
void tmfs_server_write (int fd, string s);
void tmfs_start_client (string host);
string tmfs_client_read ();
void tmfs_client_write (string s);

#endif // defined TMFS_H
