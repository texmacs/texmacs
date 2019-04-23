
/******************************************************************************
* MODULE     : url.cpp
* DESCRIPTION: unified resource location handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* The url class uses a tree representation for urls.
* This allows us to generalize the concept of an url and allow paths and
* patterns to be regarded as urls too. An url is either a string or a tuple
* of one of the following types:
*   "." -- here
*   ".." -- parent
*   none -- invalid url
*   concat -- a/b/c is represented as (concat "a" (concat "b" "c"));
*   or -- the path a:b/c is represented as (or "a" (concat "b" "c"));
*   root -- the url http://gnu.org yields (concat (root "http") "gnu.org");
*   wildcard -- (wildcard) corresponds to any url, (wildcard "*.tm")
*     to all strings which end with .tm and (wildcard "*.tm" "file")
*     to all TeXmacs files (i.e. discarding directories ending with .tm).
*******************************************************************************
* There are three main types of urls:
*   - rootless urls, like a/b/c. These urls are mainly used in computations.
*     For example, they can be appended to another url.
*   - Standard rooted urls, like file:///usr or http://www.texmacs.org.
*     These are the same as those used on the web.
*   - System urls, characterized by a "default" root.
*     These urls are similar to standard rooted urls, but they behave
*     in a slightly different way with respect to concatenation.
*     For instance http://www.texmacs.org/Web * file:///tmp would yield
*     file:///tmp, where as http://www.texmacs.org/Web * /tmp yields
*     http://www.texmacs.org/tmp
*******************************************************************************
* There are several formats for parsing (and printing) urls:
*   - System format: the usual format on your operating system.
*     On unix systems "/usr/bin:/usr/local/bin" would be a valid url
*     representing a path and on windows systems "c:\windows;c:\TeXmacs"
*     would be OK.
*   - Unix format: this format forces unix-like notation even for
*     other systems like Windows. This is convenient for url's in
*     the source code. Unix environment variables like ~ and $TEXMACS_PATH
*     can also be part of the url.
*   - Standard format: the format which is used on the web.
*     Notice that ftp://www.texmacs.org/pub and ftp://www.texmacs.org/pub/
*     represent different urls. The second one is represented by concating
*     on the right with an empty name.
*******************************************************************************
* When an explicit operation on urls need to be performed,
* like reading a file, the url is first "resolved" into a simple url
* with a unique name (modulo symbolic links) for the resource.
* Next, the url is "concretized" as a file name which is understood
* by the operating system. This may require searching the file from the web.
* Concretized urls should be used quickly and not memorized,
* since such names may be the names of temporary files,
* which may be destroyed soon afterwards.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "url.hpp"
#include "sys_utils.hpp"
#include "web_files.hpp"
#include "file.hpp"
#include "analyze.hpp"

#include <ctype.h>

#ifdef OS_MINGW
#define WINPATHS
#endif

#ifdef WINPATHS
#define URL_CONCATER  '\\'
#define URL_SEPARATOR ';'
#else
#define URL_CONCATER  '/'
#define URL_SEPARATOR ':'
#endif

/******************************************************************************
* Unrooted url constructors
******************************************************************************/

static url
url_get_atom (string s, int type) {
  if (type < URL_STANDARD) {
    if (s == "~") return url_system (get_env ("HOME"));
    if (starts (s, "$")) {
      string val= get_env (s (1, N(s)));
      if (val == "") return url_none ();
      return unblank (url_system (val));
    }
  }
  if (occurs ("*", s)) return url_wildcard (s);
#ifdef WINPATHS
  if (N(s)==2 && ends (s, ":"))
    s->resize(1); // remove the ':' after unit letter
#endif
  return as_url (tree (s));
}

static url
url_get_name (string s, int type= URL_STANDARD, int i=0) {
  char sep= (type == URL_SYSTEM)? URL_CONCATER: '/';
  int start= i, n= N(s);
  while ((i<n) && (s[i] != sep) && (s[i] != '/')) i++;
  url u= url_get_atom (s (start, i), type);
  // url u= tree (s (start, i));
  if (i == n) return u;
  if (start == i) return url_get_name (s, type, i+1);
  return u * url_get_name (s, type, i+1);
}

static url
url_get_path (string s, int type= URL_STANDARD, int i=0) {
  char sep= (type == URL_SYSTEM)? URL_SEPARATOR: ':';
  int start= i, n= N(s);
  if (i == n) return url_none ();
  while ((i<n) && (s[i] != sep)) i++;
  url u= url_general (s (start, i), type);
  if (i == n) return u;
  if (start == i) return url_get_path (s, type, i+1);
  return u | url_get_path (s, type, i+1);
}

/******************************************************************************
* Rooted url constructors
******************************************************************************/

url
url_root (string protocol) {
  return as_url (tuple ("root", protocol));
}

url
url_ramdisc (string contents) {
  return as_url (tuple ("root", "ramdisc", contents));
}

static url
url_default (string name, int type= URL_SYSTEM) {
  url u= url_get_name (name, type);
#ifdef WINPATHS
  // FIXME: this hack seems a bit too simple
  if (is_concat (u) && (u[2]->t == "")) u= u[1];
  // cout << name << " -> " << url_root ("default") * u << "\n";
  return url_root ("default") * u;
#else
  if (u->t == "") return url_root ("default");
  return url_root ("default") * u;
#endif
}

static url
url_mingw_default (string name, int type) {
  string s= name (0, 2) * ":" * name (2, N(name));
  return url_root ("default") * url_get_name (s, type);  
}

static url
url_path (string s, int type= URL_SYSTEM) {
  url u= url_get_path (s, type);
  return u;
}

static url
url_local (string name) {
  url u= url_get_name (name, URL_SYSTEM);
  return reroot (u, "file");
}

static url
url_file (string name) {
  url u= url_get_name (name);
  return url_root ("file") * u;
}

static url
url_http (string name) {
  url u= url_get_name (name);
  return url_root ("http") * u;
}

static url
url_https (string name) {
  url u= url_get_name (name);
  return url_root ("https") * u;
}

static url
url_ftp (string name) {
  url u= url_get_name (name);
  return url_root ("ftp") * u;
}

static url
url_tmfs (string name) {
  url u= url_get_name (name);
  return url_root ("tmfs") * u;
}

static url
url_blank (string name) {
  url u= url_get_name (name);
  return url_root ("blank") * u;
}

/******************************************************************************
* Generic url constructor
******************************************************************************/

static bool
heuristic_is_path (string name, int type) {
  char sep= (type==0)? URL_SEPARATOR: ':';
  int i, n= N(name);
  for (i=0; i<n; i++)
    if (name[i] == sep)
      return true;
  return false;
}

static bool
heuristic_is_default (string name, int type) {
#ifdef WINPATHS
  // FIXME: we probably should take into account 'type' too
  if (N(name) < 2) return false;
  if ((name[0] == '\\') && (name[1] == '\\')) return true;
  return
    isalpha (name[0]) && (name[1] == ':') &&
    ((N(name)==2) || (name[2] == '\\') || (name[2] == '/'));
#else
  char sep= (type==0)? URL_CONCATER: '/';
  return (name != "") && (name[0] == sep);
#endif
}

static bool
heuristic_is_http (string name) {
  return starts (name, "www.");
  // FIXME: we might want to recognize some other ones like google.com too
}

static bool
heuristic_is_ftp (string name) {
  return starts (name, "ftp.");
}

static bool
heuristic_is_mingw_default (string name, int type) {
#ifdef WINPATHS
  return type != URL_SYSTEM && N(name) >= 2 &&
         name[0] == '/' && is_alpha (name[1]) &&
         (N(name) == 2 || name[2] == '/');
#else
  (void) name; (void) type; return false;
#endif         
}

url
url_general (string name, int type= URL_SYSTEM) {
  if (starts (name, "local:")) return url_local (name (6, N (name)));
  if (starts (name, "file://")) return url_file (name (7, N (name)));
  if (starts (name, "http://")) return url_http (name (7, N (name)));
  if (starts (name, "https://")) return url_https (name (8, N (name)));
  if (starts (name, "ftp://")) return url_ftp (name (6, N (name)));
  if (starts (name, "tmfs://")) return url_tmfs (name (7, N (name)));
  if (starts (name, "//")) return url_blank (name (2, N (name)));
  if (heuristic_is_path (name, type)) return url_path (name, type);
  if (heuristic_is_default (name, type)) return url_default (name, type);
  if (heuristic_is_mingw_default (name, type)) return url_mingw_default (name, type);
  if (type != URL_CLEAN_UNIX) {
    if (heuristic_is_http (name)) return url_http (name);
    if (heuristic_is_ftp (name)) return url_ftp (name);
  }
  return url_get_name (name, type);
}

url
url_unix (string name) {
  return url_general (name, URL_UNIX);
}

url
url_unix (string dir, string name) {
  return url_unix (dir) * url_unix (name);
}

url
url_system (string name) {
  return url_general (name, URL_SYSTEM);
}

url
url_system (string dir, string name) {
  return url_system (dir) * url_system (name);
}

url
url_standard (string name) {
  return url_general (name, URL_STANDARD);
}

url
url_standard (string dir, string name) {
  return url_standard (dir) * url_standard (name);
}

url::url (): rep (tm_new<url_rep> (tuple ("none"))) {}
url::url (const char* name): rep (tm_new<url_rep> (url_unix (name)->t)) {}
url::url (string name): rep (tm_new<url_rep> (url_unix (name)->t)) {}
url::url (string path_name, string name):
  rep (tm_new<url_rep> (url_unix (path_name, name)->t)) {}

/******************************************************************************
* Computational url constructors
******************************************************************************/

static bool
is_special_root (url u) {
#ifdef WINPATHS
  return is_root (u);
#else
  return is_root_web (u);
#endif
}

static bool
is_semi_root (url u) {
  // url u such that u/.. == u (website or windows drive name)
  return is_concat (u) && is_special_root (u[1]) && is_atomic (u[2]);
}

url
operator * (url u1, url u2) {
  //cout << "concat " << u1->t << " * " << u2->t << "\n";
  if (is_root (u2) || (is_concat (u2) && is_root (u2[1]))) {
    if (is_concat (u1) && is_root_web (u1[1])) {
      if (is_root (u2, "default") ||
          (is_concat (u2) && is_root (u2[1], "default")))
        {
          url v= u1[2];
          while (is_concat (v)) v= v[1];
          if (is_root (u2)) return u1[1] * v;
          return u1[1] * v * u2[2];
        }
      if (is_root (u2, "blank") ||
          (is_concat (u2) && is_root (u2[1], "blank")))
        return reroot (u2, u1[1][1]->t->label);
    }
    return u2;
  }
  if (is_here (u1) || (u1->t == "")) return u2;
  if (is_here (u2)) return u1;
  if (is_none (u1)) return url_none ();
  if (is_none (u2)) return url_none ();
  if (u2 == url_parent ()) {
    if (is_root (u1)) return u1;
    if (is_pseudo_atomic (u1) && (!is_parent (u1))) return url_here ();
    if (is_semi_root (u1)) return u1;
  }
  if (is_concat (u2) && (u2[1] == url_parent ())) {
    if (is_root (u1)) return u1 * u2[2];
    if (is_pseudo_atomic (u1) && (!is_parent (u1))) return u2[2];
    if (is_semi_root (u1)) return u1 * u2[2];
  }
  if (is_concat (u1)) return u1[1] * (u1[2] * u2);
  return as_url (tuple ("concat", u1->t, u2->t));
}

url
operator * (url u1, const char* name) {
  return u1 * url (name);
}

url
operator * (url u1, string name) {
  return u1 * url (name);
}

url
operator | (url u1, url u2) {
  if (is_none (u1)) return u2;
  if (is_none (u2)) return u1;
  if (is_or (u1)) return u1[1] | (u1[2] | u2);
  if (u1 == u2) return u2;
  if (is_or (u2) && (u1 == u2[1])) return u2;
  return as_url (tuple ("or", u1->t, u2->t));
}

url
url_wildcard () {
  return as_url (tuple ("wildcard"));
}

url
url_wildcard (string name) {
  return as_url (tuple ("wildcard", name));
}

/******************************************************************************
* url predicates
******************************************************************************/

bool
is_rooted (url u) {
  return
    is_root (u) ||
    (is_concat (u) && is_rooted (u[1])) ||
    (is_or (u) && is_rooted (u[1]) && is_rooted (u[2]));
}

bool
is_rooted (url u, string protocol) {
  return
    is_root (u, protocol) ||
    (is_concat (u) && is_rooted (u[1], protocol)) ||
    (is_or (u) && is_rooted (u[1], protocol) && is_rooted (u[2], protocol));
}

bool
is_rooted_web (url u) {
  return
    is_root_web (u) ||
    (is_concat (u) && is_rooted_web (u[1])) ||
    (is_or (u) && is_rooted_web (u[1]) && is_rooted_web (u[2]));
}

bool
is_rooted_tmfs (url u) {
  return
    is_root_tmfs (u) ||
    (is_concat (u) && is_rooted_tmfs (u[1])) ||
    (is_or (u) && is_rooted_tmfs (u[1]) && is_rooted_tmfs (u[2]));
}

bool
is_tmfs_protocol (url u, string protocol) {
  return
    u->t == protocol ||
    (is_concat (u) && is_tmfs_protocol (u[1], protocol));
}

bool
is_rooted_tmfs (url u, string protocol) {
  return
    (is_concat (u) && is_root_tmfs (u[1]) &&
                      is_tmfs_protocol (u[2], protocol)) ||
    (is_or (u) && is_rooted_tmfs (u[1], protocol) &&
                  is_rooted_tmfs (u[2], protocol));
}

bool
is_rooted_blank (url u) {
  return
    is_root_blank (u) ||
    (is_concat (u) && is_rooted_blank (u[1])) ||
    (is_or (u) && is_rooted_blank (u[1]) && is_rooted_blank (u[2]));
}

bool
is_name (url u) {
  if (is_atomic (u)) return true;
  if (!is_concat (u)) return false;
  return is_name (u[1]) && is_name (u[2]);
}

bool
is_rooted_name (url u) {
  return is_concat (u) && is_root (u[1]) && is_name (u[2]);
}

bool
is_name_in_path (url u) {
  if (is_name (u)) return true;
  return is_concat (u) && is_root (u[1], "default") && is_name (u[2]);
}

bool
is_path (url u) {
  if (is_atomic (u)) return true;
  if ((!is_or (u)) && (!is_concat (u))) return false;
  return is_path (u[1]) && is_path (u[2]);
}

bool
is_rooted_path (url u) {
  return is_rooted (u) && is_path (u);
}

bool
is_ramdisc (url u) {
  return is_concat (u) && is_root (u[1], "ramdisc");
}

/******************************************************************************
* Conversion routines for urls
******************************************************************************/

string
as_string (url u, int type) {
  // This routine pritty prints an url as a string.
  // FIXME: the current algorithm is quadratic in time.
  if (is_none (u)) return "{}";
  if (is_atomic (u)) return u->t->label;
  if (is_concat (u)) {
    int stype= type;
    if (is_root (u[1]) && (!is_root (u[1], "default"))) stype= URL_STANDARD;
    string sep= (stype==URL_SYSTEM? string (URL_CONCATER): string ("/"));
    string s1 = as_string (u[1], type);
    string s2 = as_string (u[2], stype);
    if (is_root (u[1], "default")) s1= "";
    if ((!is_name (u[1])) && (!is_root (u[1]))) s1= "{" * s1 * "}";
    if ((!is_concat (u[2])) && (!is_atomic (u[2])) && (!is_wildcard (u[2], 1)))
      s2= "{" * s2 * "}";
#ifdef WINPATHS
    if (((is_root (u[1], "default") && type == URL_SYSTEM) ||
         is_root (u[1], "file"))) { // have to return the windows format
      string root, remain;
      if (is_concat (u[2])) {		
        root = as_string (u[2][1], type);
        // root might be unit letter or hostname. It depends on the length
        remain = as_string (u[2][2], type);
      }
      else {
        root = s2;
        remain = "";
      }
      if (is_root (u[1], "default")) {
        if (N(root) == 1) return root * ":\\" * remain;	// drive letter
        else return "\\\\" * root * "\\" * remain;
      }
      else {
        if (N(root) == 1) return s1 * "/" * root * ":/" * remain; // local file
        else return s1 * root * "/" * remain; // remote
      }
    }
#endif
    return s1 * sep * s2;
  }
  if (is_or (u)) {
    string s1= as_string (u[1], type);
    string s2= as_string (u[2], type);
    if (!is_name_in_path (u[1])) s1= "{" * s1 * "}";
    if ((!is_or (u[2])) && (!is_name_in_path (u[2]))) s2= "{" * s2 * "}";
#ifdef WINPATHS
    if (type == URL_STANDARD) return s1 * ":" * s2;
    else return s1 * string (URL_SEPARATOR) * s2;
#else
    return s1 * string (URL_SEPARATOR) * s2;
#endif
  }
#ifdef WINPATHS
  if (is_root (u, "default")) {
    int stype= type;
    if (is_root (u[1]) && (!is_root (u[1], "default"))) stype= URL_STANDARD;
	if (stype == URL_SYSTEM) return ""; else return "/";
  }
#else
  if (is_root (u, "default")) return "/";
#endif
  if (is_root (u, "blank")) return "/";
  if (is_root (u, "file")) return u[1]->t->label * "://";
  if (is_root (u)) return u[1]->t->label * ":/";
  if (is_wildcard (u, 0)) return "**";
  if (is_wildcard (u, 1)) return u->t[1]->label;
  FAILED ("bad url");
  return "";
}

tm_ostream&
operator << (tm_ostream& out, url u) {
  return out << as_string (u, URL_SYSTEM);
}

/******************************************************************************
* Operations on urls
******************************************************************************/

url
head (url u) {
  return u * url_parent ();
}

url
tail (url u) {
  if (is_concat (u)) {
    if (is_root_web (u[1]) && is_atomic (u[2])) return url_here ();
    return tail (u[2]);
  }
  if (is_or (u)) return tail (u[1]) | tail (u[2]);
  if (is_root (u)) return url_here ();
  return u;
}

string
suffix (url u) {
  u= tail (u);
  if (!is_atomic (u)) return "";
  string s= as_string (u);
  int i, n= N(s);
  for (i=n-1; i>=0; i--)
    if (s[i]=='.') break;
  if ((i>0) && (i<n-1)) {
    string r= s (i+1, n);
    while ((N(r)>0) && (r[N(r)-1]=='~' || r[N(r)-1]=='#')) r= r(0, N(r)-1);
    return locase_all(r);
  }
  return "";
}

string
basename (url u, string suf) {
  string s= as_string (tail (u));
  if (suf != "" && N(s) > N(suf) && suf == s(N(s)-N(suf),N(s)))
    return s(0, N(s)-N(suf));
  return s;
}

string
basename (url u) {
  string s= suffix (u);
  if (N(s) != 0) s= "." * s;
  return basename (u, s);
}

url
glue (url u, string s) {
  if (is_atomic (u)) return as_url (tree (u->t->label * s));
  if (is_concat (u)) return u[1] * glue (u[2], s);
  if (is_or (u)) return glue (u[1], s) | glue (u[2], s);
  if (is_none (u)) return u;
  failed_error << "u= " << u << "\n";
  failed_error << "s= " << s << "\n";
  FAILED ("can't glue string to url");
  return u;
}

url
unglue (url u, int nr) {
  if (is_atomic (u))
    return as_url (tree (u->t->label (0, max (N(u->t->label) - nr, 0))));
  if (is_concat (u)) return u[1] * unglue (u[2], nr);
  if (is_or (u)) return unglue (u[1], nr) | unglue (u[2], nr);
  if (is_none (u)) return u;
  failed_error << "u = " << u << "\n";
  failed_error << "nr= " << nr << "\n";
  FAILED ("can't unglue from url");
  return u;
}

url
unblank (url u) {
  if (is_concat (u) && (u[2]->t == "")) return u[1];
  if (is_concat (u)) return u[1] * unblank (u[2]);
  if (is_or (u)) return unblank (u[1]) | unblank (u[2]);
  return u;
}

url
relative (url base, url u) {
  return head (base) * u;
}

url
delta_sub (url base, url u) {
  if (is_atomic (base))
    return u;
  if (is_concat (base) && is_concat (u) && (base[1] == u[1])) {
    if (is_special_root (base[1]) &&
	is_concat (base[2]) && is_concat (u[2]) &&
	base[2][1] != u[2][1])
      return url_none ();
    return delta_sub (base[2], u[2]);
  }
  if (is_concat (base) && !is_semi_root (base))
    return url_parent () * delta_sub (head (base), u);
  return url_none ();
}

url
delta (url base, url u) {
  if (is_or (u))
    return delta (base, u[1]) | delta (base, u[2]);
  url res= delta_sub (base, u);
  if (is_none (res)) return u;
  return res;
}

static url
expand (url u1, url u2) {
  if (is_or (u1)) return expand (u1[1], u2) | expand (u1[2], u2);
  if (is_or (u2)) return expand (u1, u2[1]) | expand (u1, u2[2]);
  if (is_ancestor (u2)) {
    if (is_concat (u1)) return u1 | expand (u1[1], u2);
    if (is_special_root (u1)) return u2;
    return u1 | u2;
  }
  if (is_concat (u2) && is_ancestor (u2[1]))
    return expand (expand (u1, u2[1]), u2[2]);
  return u1 * u2;
}

url
expand (url u) {
  if (is_or (u)) return expand (u[1]) | expand (u[2]);
  if (is_concat (u)) return expand (expand (u[1]), expand (u[2]));
  return u;
}

bool
descends (url u, url base) {
  if (is_or (base))
    return descends (u, base[1]) || descends (u, base[2]);
  if (is_or (u))
    return descends (u[1], base) && descends (u[2], base);
  if (u == base)
    return true;
  if (is_concat (u) && is_atomic (base))
    return u[1] == base;
  if (is_concat (u) && is_concat (base))
    return u[1] == base[1] && descends (u[2], base[2]);
  return false;
}

bool
is_secure (url u) {
  return descends (u, expand (url_path ("$TEXMACS_SECURE_PATH")));
}

/******************************************************************************
* Url sorting and factorization
******************************************************************************/

static bool
operator <= (url u1, url u2) {
  if (is_atomic (u1) && is_atomic (u2))
    return u1->t->label <= u2->t->label;
  if (is_atomic (u1)) return true;
  if (is_atomic (u2)) return false;
  if (is_concat (u1) && is_concat (u2)) {
    if (u1[1] == u2[1]) return u1[2] <= u2[2];
    else return u1[1] <= u2[1];
  }
  if (is_concat (u1)) return true;
  if (is_concat (u2)) return false;
  return true; // does not matter for sorting
}

static url
sort_sub (url add, url to) {
  if (is_or (to)) {
    if (add <= to[1]) return add | to;
    return to[1] | sort_sub (add, to[2]);
  }
  if (add <= to) return add | to;
  else return to | add;
}

url
sort (url u) {
  if (is_or (u))
    return sort_sub (u[1], sort (u[2]));
  else return u;
}

static url
factor_sorted (url u) {
  if (!is_or (u)) return u;
  url v= factor_sorted (u[2]);
  if (is_concat (u[1])) {
    if (is_concat (v) && (u[1][1] == v[1]))
      return u[1][1] * (u[1][2] | v[2]);
    if (is_or (v) && is_concat (v[1]) && (u[1][1] == v[1][1]))
      return (u[1][1] * (u[1][2] | v[1][2])) | v[2];
  }
  return u[1] | v;
}

static url
factor_sub (url u) {
  if (is_concat (u)) return u[1] * factor (u[2]);
  if (is_or (u)) return factor_sub (u[1]) | factor_sub (u[2]);
  return u;
}

url
factor (url u) {
  return factor_sub (factor_sorted (sort (u)));
}

/******************************************************************************
* Url resolution and wildcard expansion
******************************************************************************/

url complete (url base, url u, string filter, bool flag);

string
get_root (url u) {
  if (is_concat (u)) return get_root (u[1]);
  if (is_or (u)) {
    string s1= get_root (u[1]);
    string s2= get_root (u[2]);
    if (s1 == s2) return s1; else return "";
  }
  if (is_root (u)) return u[1]->t->label;
  return "";
}

url
unroot (url u) {
  if (is_concat (u)) return unroot (u[1]) * u[2];
  if (is_or (u)) return unroot (u[1]) | unroot (u[2]);
  if (is_root (u)) return url_here ();
  return u;
}

url
reroot (url u, string protocol) {
  if (is_concat (u)) return reroot (u[1], protocol) * u[2];
  if (is_or (u)) return reroot (u[1], protocol) | reroot (u[2], protocol);
  if (is_root (u)) return url_root (protocol);
  return u;
}

static url
complete (url base, url sub, url u, string filter, bool flag) {
  if (is_or (sub)) {
    url res1= complete (base, sub[1], u, filter, flag);
    if ((!is_none (res1)) && flag) return res1;
    return res1 | complete (base, sub[2], u, filter, flag);
  }
  if (is_concat (sub) && is_rooted (sub[1])) {
    url res= complete (sub[1], sub[2], u, filter, flag);
    return sub[1] * res;
  }
  return sub * complete (base * sub, u, filter, flag);
}

url
complete (url base, url u, string filter, bool flag) {
  // cout << "complete " << base << " |||| " << u << LF;
  if (!is_rooted(u)) {
     if (is_none (base)) return base;
     if (is_none (u)) return u;
     if ((!is_root (base)) && (!is_rooted_name (base))) {
        failed_error << "base= " << base << LF;
        FAILED ("invalid base url");
     }
  }
  if (is_name (u) || (is_concat (u) && is_root (u[1]) && is_name (u[2]))) {
    url comp= base * u;
    if (is_rooted (comp, "default") || is_rooted (comp, "file")) {
      if (is_of_type (comp, filter)) return reroot (u, "default");
      return url_none ();
    }
    if (is_rooted_web (comp) || is_rooted_tmfs (comp) || is_ramdisc (comp)) {
      if (is_of_type (comp, filter)) return u;
      return url_none ();
    }
    failed_error << "base= " << base << LF;
    failed_error << "u= " << u << LF;
    ASSERT (is_rooted (comp), "unrooted url");
    FAILED ("bad protocol in url");
  }
  if (is_root (u)) {
    // FIXME: test filter flags here
    return u;
  }
  if (is_concat (u) && is_wildcard (u[1], 0) && is_wildcard (u[2], 1)) {
    // FIXME: ret= ret | ... is unefficient (quadratic) in main loop
    if (!(is_rooted (base, "default") || is_rooted (base, "file"))) {
      failed_error << "base= " << base << LF;
      FAILED ("wildcards only implemented for files");
    }
    url ret= url_none ();
    bool error_flag;
    array<string> dir= read_directory (base, error_flag);
    int i, n= N(dir);
    for (i=0; i<n; i++) {
      if ((!is_none (ret)) && flag) return ret;
      if ((dir[i] == ".") || (dir[i] == "..")) continue;
      if (starts (dir[i], "http://") ||
          starts (dir[i], "https://") ||
          starts (dir[i], "ftp://"))
        if (is_directory (base * dir[i])) continue;
      ret= ret | (dir[i] * complete (base * dir[i], u, filter, flag));
      if (match_wildcard (dir[i], u[2][1]->t->label))
	ret= ret | complete (base, dir[i], filter, flag);
    }
    return ret;
  }
  if (is_concat (u)) {
    url sub= complete (base, u[1], "", false);
    // "" should often be faster than the more correct "d" here
    return complete (base, sub, u[2], filter, flag);
  }
  if (is_or (u)) {
    url res1= complete (base, u[1], filter, flag);
    if ((!is_none (res1)) && flag) return res1;
    return res1 | complete (base, u[2], filter, flag);
  }
  if (is_wildcard (u)) {
    // FIXME: ret= ret | ... is unefficient (quadratic) in main loop
    if (!(is_rooted (base, "default") || is_rooted (base, "file"))) {
      failed_error << "base= " << base << LF;
      FAILED ("wildcards only implemented for files");
    }
    url ret= url_none ();
    if (is_wildcard (u, 0) && is_of_type (base, filter)) ret= url_here ();
    bool error_flag;
    array<string> dir= read_directory (base, error_flag);
    int i, n= N(dir);
    for (i=0; i<n; i++) {
      if ((!is_none (ret)) && flag) return ret;
      if ((dir[i] == ".") || (dir[i] == "..")) continue;
      if (starts (dir[i], "http://") ||
          starts (dir[i], "https://") ||
          starts (dir[i], "ftp://"))
        if (is_directory (base * dir[i])) continue;
      if (is_wildcard (u, 0))
	ret= ret | (dir[i] * complete (base * dir[i], u, filter, flag));
      else if (match_wildcard (dir[i], u[1]->t->label))
	ret= ret | complete (base, dir[i], filter, flag);
    }
    return ret;
  }
  failed_error << "url= " << u << LF;
  FAILED ("bad url");
  return u;
}

url
complete (url u, string filter, bool flag) {
  url home= url_pwd ();
  return home * complete (home, u, filter, flag);
}

url
complete (url u, string filter) {
  // This routine can be used in order to find all possible matches
  // for the wildcards in an url and replace the wildcards by these matches.
  // Moreover, matches are normalized (file root -> default root).
  url r =  complete (u, filter, false);
  // cout << "complete:" << u << " filter:" << filter << " result:" << r << LF;
  return r;
}

url
resolve (url u, string filter) {
  // This routine does the same thing as complete, but it stops at
  // the first match. It is particularly useful for finding files in paths.
  return complete (u, filter, true);
  /*
  url res= complete (u, filter, true);
  if (is_none (res))
    cout << "Failed resolution of " << u << ", " << filter << LF;
  return res;
  */
}

url
resolve_in_path (url u) {
  if (use_which) {
    string name = escape_sh (as_string (u));
    string which= var_eval_system ("which " * name * " 2> /dev/null");
    if (ends (which, name))
      return which;
    else if ((which != "") &&
	     (!starts (which, "which: ")) &&
	     (!starts (which, "no ")))
      cout << "TeXmacs] " << which << "\n";
  }
#ifdef OS_MINGW
  return resolve ((url_path ("$TEXMACS_PATH/bin") | url_path ("$PATH")) * u, "x");
#else
  return resolve (url_path ("$PATH") * u, "x");
#endif
}

bool
exists (url u) {
  return !is_none (resolve (u, "r"));
}

bool
exists_in_path (url u) {
#ifdef OS_MINGW
  return !is_none (resolve_in_path (url (as_string (u) * ".bat"))) ||\
  	 !is_none (resolve_in_path (url (as_string (u) * ".exe"))) ||\
	 !is_none (resolve_in_path (url (as_string (u) * ".com")));
#else
  return !is_none (resolve_in_path (u));
#endif
}

bool
has_permission (url u, string filter) {
  return !is_none (resolve (u, filter));
}

static url
descendance_sub (url u) {
  if (is_or (u))
    return descendance_sub (u[1]) | descendance_sub (u[2]);
  return complete (u, url_wildcard (), "r", false);
}

url
descendance (url u) {
  // Utility for style and package menus in tm_server.cpp
  // Compute and merge subdirectories of directories in path
  return factor (descendance_sub (u));
}

/******************************************************************************
* Concretization of resolved urls
******************************************************************************/

string
concretize (url u) {
  // This routine transforms a resolved url into a system file name.
  // In the case of distant files from the web, a local copy is created.
  if (is_rooted (u, "default") ||
      is_rooted (u, "file") ||
      is_rooted (u, "blank"))
        return as_string (reroot (u, "default"));
  if (is_rooted_web (u)) return concretize (get_from_web (u));
  if (is_rooted_tmfs (u)) return concretize (get_from_server (u));
  if (is_ramdisc (u)) return concretize (get_from_ramdisc (u));
  if (is_here (u)) return as_string (url_pwd ());
  if (is_parent (u)) return as_string (url_pwd () * url_parent ());
  if (is_wildcard (u, 1)) return u->t[1]->label;
  std_warning << "Couldn't concretize " << u->t << LF;
  // failed_error << "u= " << u << LF;
  // FAILED ("url has no root");
  return "xxx";
}

string
materialize (url u, string filter) {
  // Combines resolve and concretize
  url r= resolve (u, filter);
  if (!(is_rooted (r) || is_here (r) || is_parent (r))) {
    failed_error << "u= " << u << LF;
    FAILED ("url could not be resolved");
  }
  return concretize (r);
}
