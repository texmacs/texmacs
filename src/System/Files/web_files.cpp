
/******************************************************************************
* MODULE     : web_files.cpp
* DESCRIPTION: file handling via the web
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"

#define MAX_CACHED 25
static int web_nr=0;
static array<tree> web_cache (MAX_CACHED);
static hashmap<tree,tree> web_cache_resolve ("");

/******************************************************************************
* Caching
******************************************************************************/

static url
get_cache (url name) {
  if (web_cache_resolve->contains (name->t)) {
    int i, j;
    tree tmp= web_cache_resolve [name->t];
    for (i=0; i<MAX_CACHED; i++)
      if (web_cache[i] == name->t) {
	// cout << name << " in cache as " << tmp << " at " << i << "\n";
	for (j=i; ((j+1) % MAX_CACHED) != web_nr; j= (j+1) % MAX_CACHED)
	  web_cache[j]= web_cache[(j+1) % MAX_CACHED];
	web_cache[j]= name->t;
	break;
      }
    return as_url (tmp); // url_system (tmp);
  }
  return url_none ();
}

static url
set_cache (url name, url tmp) {
  web_cache_resolve->reset (web_cache [web_nr]);
  web_cache [web_nr]= name->t;
  web_cache_resolve (name->t)= tmp->t;
  web_nr= (web_nr+1) % MAX_CACHED;
  return tmp;
}

void
web_cache_invalidate (url name) {
  for (int i=0; i<MAX_CACHED; i++)
    if (web_cache[i] == name->t) {
      web_cache[i]= tree ("");
      web_cache_resolve->reset (name->t);
    }
}

/******************************************************************************
* Web files
******************************************************************************/

static string
web_encode (string s) {
  return tm_decode (s);
}

url
get_from_web (url name) {
  if (!is_rooted_web (name)) return url_none ();
  url res= get_cache (name);
  if (!is_none (res)) return res;

  string test= var_eval_system ("which wget");
  if (!ends (test, "wget")) return url_none ();
  url tmp= url_temp ();
  string tmp_s= escape_sh (concretize (tmp));
  string cmd= "wget --header='User-Agent: TeXmacs-" TEXMACS_VERSION "' -q";
  cmd << " --no-check-certificate --tries=1";
  cmd << " -O " << tmp_s << " " << escape_sh (web_encode (as_string (name)));
  // cout << cmd << "\n";
  system (cmd);
  // cout << "got " << name << " as " << tmp << "\n";

  if (var_eval_system ("cat " * tmp_s * " 2> /dev/null") == "") {
    remove (tmp);
    return url_none ();
  }
  else return set_cache (name, tmp);
}

/******************************************************************************
* Files from a hyperlink file system
******************************************************************************/

url
get_from_server (url u) {
  if (!is_rooted_tmfs (u)) return url_none ();
  url res= get_cache (u);
  if (!is_none (res)) return res;

  string name= as_string (u);
  if (ends (name, "~") || ends (name, "#")) {
    if (!is_rooted_tmfs (name)) return url_none ();
    if (!as_bool (call ("tmfs-can-autosave?", unglue (u, 1))))
      return url_none ();
  }
  string r= as_string (call ("tmfs-load", object (name)));
  if (r == "") return url_none ();
  url tmp= url_temp (string (".") * suffix (name));
  (void) save_string (tmp, r, true);

  //return set_cache (u, tmp);
  return tmp;
  // FIXME: certain files could be cached, but others not
  // for instance, files which are loaded in a delayed fashion
  // would always be cached as empty files, which is erroneous.
}

bool
save_to_server (url u, string s) {
  if (!is_rooted_tmfs (u)) return true;
  string name= as_string (u);
  (void) call ("tmfs-save", object (name), object (s));
  return false;
}

/******************************************************************************
* Ramdisc
******************************************************************************/

url
get_from_ramdisc (url u) {
  if (!is_ramdisc (u)) return url_none ();
  url res= get_cache (u);
  if (!is_none (res)) return (res);
  url tmp= url_temp (string (".") * suffix (u));
  save_string (tmp, u[1][2]->t->label);
  return set_cache (u, tmp);
}
