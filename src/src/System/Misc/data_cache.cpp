
/******************************************************************************
* MODULE     : data_cache.cpp
* DESCRIPTION: utilities for caching data
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "data_cache.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "iterator.hpp"

/******************************************************************************
* Caching routines
******************************************************************************/

static hashmap<tree,tree> cache_data ("?");
static hashset<string> cache_loaded;
static hashset<string> cache_changed;
static hashmap<string,bool> cache_valid (false);

void
cache_set (string buffer, tree key, tree t) {
  tree ckey= tuple (buffer, key);
  if (cache_data[ckey] != t) {
    cache_data (ckey)= t;
    cache_changed->insert (buffer);
  }
}

void
cache_reset (string buffer, tree key) {
  tree ckey= tuple (buffer, key);
  cache_data->reset (ckey);
  cache_changed->insert (buffer);
}

bool
is_cached (string buffer, tree key) {
  tree ckey= tuple (buffer, key);
  return cache_data->contains (ckey);
}

tree
cache_get (string buffer, tree key) {
  tree ckey= tuple (buffer, key);
  return cache_data [ckey];
}

bool
is_up_to_date (url dir, bool reset) {
  string name_dir= concretize (dir);
  if (reset) cache_valid->reset (name_dir);
  if (cache_valid->contains (name_dir)) return cache_valid [name_dir];
  int l= last_modified (dir, false);
  if (is_cached ("validate_cache.scm", name_dir)) {
    int r= as_int (cache_get ("validate_cache.scm", name_dir) -> label);
    if (l == r) {
      cache_valid (name_dir)= true;
      return true;
    }
    //cout << name_dir << " no longer up to date " << r << " -> " << l << "\n";
  }
  //else cout << name_dir << " not up to date " << l << "\n";
  cache_set ("validate_cache.scm", name_dir, as_string (l));
  cache_valid (name_dir)= false;
  return false;
}

bool
is_recursively_up_to_date (url dir) {
  if (!is_up_to_date (dir)) return false;
  bool error_flag;
  array<string> a= read_directory (dir, error_flag);
  for (int i=0; i<N(a); i++)
    if (url (a[i]) != url_here () && url (a[i]) != url_parent ())
      if (is_directory (dir * a[i]))
	if (!is_recursively_up_to_date (dir * a[i]))
	  return false;
  return true;
}

/******************************************************************************
* Which files should be stored in the cache?
******************************************************************************/

static string texmacs_path;
static string texmacs_doc_path;
static string texmacs_home_path;

bool
do_cache_dir (string name) {
  return
    starts (name, texmacs_path) ||
    starts (name, texmacs_doc_path);
}

bool
do_cache_stat (string name) {
  return
    starts (name, texmacs_path) ||
    starts (name, texmacs_home_path * "/fonts") ||
    starts (name, texmacs_doc_path);
}

bool
do_cache_stat_fail (string name) {
  return
    starts (name, texmacs_path) ||
    starts (name, texmacs_doc_path);
}

bool
do_cache_file (string name) {
  return
    starts (name, texmacs_path) ||
    starts (name, texmacs_home_path * "/fonts");
}

bool
do_cache_doc (string name) {
  return starts (name, texmacs_doc_path);
}

/******************************************************************************
* Saving and loading the cache to/from disk
******************************************************************************/

void
cache_save (string buffer) {
  if (cache_changed->contains (buffer)) {
    string cache_file= texmacs_home_path * "/system/cache/" * buffer;
    string cached;
    iterator<tree> it= iterate (cache_data);
    if (buffer == "file_cache" || buffer == "doc_cache") {
      while (it->busy ()) {
	tree ckey= it->next ();
	if (ckey[0] == buffer) {
	  cached << ckey[1]->label << "\n";
	  cached << cache_data [ckey]->label << "\n";
	  cached << "%-%-tm-cache-%-%\n";
	}
      }
    }
    else {
      cached << "(tuple\n";
      while (it->busy ()) {
	tree ckey= it->next ();
	if (ckey[0] == buffer) {
	  cached << tree_to_scheme (ckey[1]) << " ";
	  cached << tree_to_scheme (cache_data [ckey]) << "\n";
	}
      }
      cached << ")";
    }
    (void) save_string (cache_file, cached);
    cache_changed->remove (buffer);
  }
}

void
cache_load (string buffer) {
  if (!cache_loaded->contains (buffer)) {
    string cache_file= texmacs_home_path * "/system/cache/" * buffer;
    string cached;
    if (!load_string (cache_file, cached, false)) {
      if (buffer == "file_cache" || buffer == "doc_cache") {
	int i=0, n= N(cached);
	while (i<n) {
	  int start= i;
	  while (i<n && cached[i] != '\n') i++;
	  string key= cached (start, i);
	  i++; start= i;
	  while (i<n && (cached[i] != '\n' ||
			 !test (cached, i+1, "%-%-tm-cache-%-%"))) i++;
	  string im= cached (start, i);
	  i++;
	  while (i<n && cached[i] != '\n') i++;
	  i++;
	  //cout << "key= " << key << "\n----------------------\n";
	  //cout << "im= " << im << "\n----------------------\n";
	  cache_data (tuple (buffer, key))= im;
	}
      }
      else {
	tree t= scheme_to_tree (cached);
	for (int i=0; i<N(t)-1; i+=2)
	  cache_data (tuple (buffer, t[i]))= t[i+1];
      }
    }
    cache_loaded->insert (buffer);
  }
}

void
cache_memorize () {
  cache_save ("file_cache");
  cache_save ("doc_cache");
  cache_save ("dir_cache.scm");
  cache_save ("stat_cache.scm");
  cache_save ("font_cache.scm");
  cache_save ("validate_cache.scm");
}

void
cache_refresh () {
  cache_data   = hashmap<tree,tree> ("?");
  cache_loaded = hashset<string> ();
  cache_changed= hashset<string> ();
  cache_load ("file_cache");
  cache_load ("dir_cache.scm");
  cache_load ("stat_cache.scm");
  cache_load ("font_cache.scm");
  cache_load ("validate_cache.scm");
}

void
cache_initialize () {
  texmacs_path= concretize ("$TEXMACS_PATH");
  if (get_env ("TEXMACS_HOME_PATH") == "")
    texmacs_home_path= concretize ("$HOME/.TeXmacs");
  else texmacs_home_path= concretize ("$TEXMACS_HOME_PATH");
  if (get_env ("TEXMACS_DOC_PATH") == "")
    texmacs_doc_path= concretize ("$TEXMACS_PATH/doc");
  else texmacs_doc_path= concretize ("$TEXMACS_DOC_PATH");
  cache_refresh ();
  if (is_recursively_up_to_date (url (texmacs_path) * "fonts/type1") &&
      is_recursively_up_to_date (url (texmacs_path) * "fonts/truetype") &&
      is_recursively_up_to_date (url (texmacs_home_path) * "fonts/type1") &&
      is_recursively_up_to_date (url (texmacs_home_path) * "fonts/truetype"));
  else remove (url (texmacs_home_path) * "fonts/error" * url_wildcard ("*"));
}
