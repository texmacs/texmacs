
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

property substitute (property p, solution sol);
properties substitute (property p, solutions sols);
collection as_collection (solutions sols, string key);
collection as_collection (solutions sols, property p);
void tmfs_raw_set_property (property p);
void tmfs_raw_reset_property (property p);
solutions tmfs_raw_get_solutions (property query);
solutions tmfs_raw_get_solutions (solutions sols, property ps);

bool tmfs_allows (string id, string type);
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

#endif // defined TMFS_H
