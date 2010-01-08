
/******************************************************************************
* MODULE     : url.hpp
* DESCRIPTION: unified resource location handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef URL_H
#define URL_H
#include "tree.hpp"

#define URL_SYSTEM 0
#define URL_UNIX 1
#define URL_STANDARD 2

/******************************************************************************
* The url data type
******************************************************************************/

struct url_rep: concrete_struct {
  tree t;
  inline url_rep (tree t2): t (t2) {}
};

class url {
  CONCRETE(url);
private:
  url (tree t): rep (tm_new<url_rep> (t)) {}
public:
  url (const char* name);
  url (string name);
  url (string dir, string name);
  inline bool operator == (url u) { return rep->t == u->t; }
  inline bool operator != (url u) { return rep->t != u->t; }
  inline url operator [] (int i) { return url (rep->t[i]); }
  friend url as_url (tree t);
};
CONCRETE_CODE(url);

tm_ostream& operator << (tm_ostream& out, url u);
string as_string (url u, int type= URL_SYSTEM);
inline url as_url(tree t) { return url(t); }

/******************************************************************************
* url constructors
******************************************************************************/

url url_general (string name, int type);
url url_unix (string name);
url url_unix (string dir, string name);
url url_system (string name);
url url_system (string dir, string name);
url url_standard (string name);
url url_standard (string dir, string name);

inline url url_none () { return as_url (tuple ("none")); }
inline url url_here () { return as_url (tree (".")); }
inline url url_parent () { return as_url (tree ("..")); }
inline url url_pwd () { return url_system ("$PWD"); }

url url_root (string protocol);       // root url
url url_ramdisc (string contents);    // ramdisc with contents contents
url url_wildcard ();                  // any url
url url_wildcard (string name);       // string with * wildcards

url operator * (url u1, url u2);      // concatenation of url with rootless url
url operator * (url u1, const char* name);
url operator * (url u1, string name);
url operator | (url u1, url u2);      // disjunction of urls like in file paths

inline url url_parent (url u) { return u * url_parent (); }

/******************************************************************************
* predicates
******************************************************************************/

inline bool is_none (url u) { return is_tuple (u->t, "none", 0); }
inline bool is_here (url u) { return u->t == "."; }
inline bool is_parent (url u) { return u->t == ".."; }
inline bool is_atomic (url u) { return is_atomic (u->t); }
inline bool is_concat (url u) { return is_tuple (u->t, "concat", 2); }
inline bool is_or (url u) { return is_tuple (u->t, "or", 2); }
inline bool is_root (url u) {
  return is_tuple (u->t, "root") && (N(u->t) >= 2); }
inline bool is_root (url u, string s) {
  return is_root (u) && (u[1]->t->label == s); }
inline bool is_root_web (url u) {
  return is_root (u, "http") || is_root (u, "ftp"); }
inline bool is_root_tmfs (url u) { return is_root (u, "tmfs"); }
inline bool is_wildcard (url u) { return is_tuple (u->t, "wildcard"); }
inline bool is_wildcard (url u, int n) {
  return is_tuple (u->t, "wildcard", n); }

bool is_rooted (url u);
bool is_rooted (url u, string protocol);
bool is_rooted_web (url u);
bool is_rooted_tmfs (url u);
bool is_name (url u);
bool is_rooted_name (url u);
bool is_path (url u);
bool is_rooted_path (url u);
bool is_ramdisc (url u);

/******************************************************************************
* operations on urls
******************************************************************************/

url    head (url u);               // keep only the directory of the file
url    tail (url u);               // keep only the file name without path
string suffix (url u);             // get suffix of file
url    glue (url u, string s);     // glue suffix to url tail
url    unglue (url u, int nr);     // remove nr chars from suffix
url    unblank (url u);            // a/b/ -> a/b
url    relative (url base, url u); // a/b, c -> a/c
url    delta (url base, url u);    // relative (a, delta (a, b)) == b
url    reroot (url u, string s);   // reroot using new protocol
url    expand (url u);             // rewrite a/{b:c} -> a/b:a/c
url    sort (url u);               // order items in ors
url    factor (url u);             // inverse of expand; also sorts
bool   descends (url u, url base); // does u descend from base?
bool   is_secure (url u);          // is u secure?

/******************************************************************************
* url resolution
******************************************************************************/

url  complete (url u, string filter= "fr"); // wildcard completion
url  resolve (url u, string filter= "fr");  // find first match only
url  resolve_in_path (url u);               // find file in path
bool exists (url u);                        // file exists
bool exists_in_path (url u);                // file exists in path
bool has_permission (url u, string filter); // check file permissions
url  descendance (url u);                   // utility for style&package menus
string concretize (url u);                  // system name for resolved url
string materialize (url u, string f= "fr"); // resolve + concretize

#endif // defined URL_H
