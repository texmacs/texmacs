
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
#include "url.hpp"
#include "file.hpp"
#include "convert.hpp"

static string cache_file ("$TEXMACS_HOME_PATH/system/cache.scm");
static bool cache_loaded= false;
static bool cache_changed= false;
static hashmap<string,tree> cache_data ("?");

void
cache_set (string s, tree t) {
  if (cache_data[s] != t) {
    cache_data (s)= t;
    cache_changed= true;
  }
}

void
cache_reset (string s) {
  cache_data->reset (s);
  cache_changed= true;
}

bool
is_cached (string s) {
  return cache_data->contains (s);
}

tree
cache_get (string s) {
  return cache_data [s];
}

void
cache_save () {
  if (cache_changed) {
    tree t (cache_data);
    (void) save_string (cache_file, tree_to_scheme (t));
    cache_changed= false;
  }
}

void
cache_load () {
  if (!cache_loaded) {
    string cached;
    if (!load_string (cache_file, cached)) {
      tree t= scheme_to_tree (cached);
      cache_data= hashmap<string,tree> ("?", t);
    }
    cache_loaded= true;
  }
}
