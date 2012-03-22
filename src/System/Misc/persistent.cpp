
/******************************************************************************
* MODULE     : persistent.cpp
* DESCRIPTION: persistent caching of string key-value pairs
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "persistent.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

/******************************************************************************
* Global data
******************************************************************************/

#define MAX_BRANCH 26
#define MAX_INNER 10
#define MAX_SIZE 4096
#define MAX_FILES 26

static hashmap<string,string>       persistent_pre   ("");
static hashmap<string,url>          persistent_file  (url_none ());
static hashmap<string,unsigned int> persistent_hash  (0);
static hashmap<string,bool>         persistent_has   (false);
static hashmap<string,string>       persistent_cache ("");

static int number_persistent_file_names= -1;

/******************************************************************************
* Writing and reading hashmaps to and from disk
******************************************************************************/

void
write_escaped (string& r, string s) {
  int n= N(s);
  for (int i=0; i<n; i++) {
    char c= s[i];
    if (c == '\\') r << '\\' << '\\';
    else if (c == '\n') r << '\\' << 'n';
    else r << c;
  }
}

void
persistent_write_map (url file, hashmap<string,string> map) {
  string s;
  iterator<string> it= iterate (map);
  while (it->busy ()) {
    string key= it->next ();
    write_escaped (s, key);
    s << '\n';
    write_escaped (s, map[key]);
    s << '\n';
  }
  save_string (file, s, false);
}

string
read_escaped (string& s, int& i) {
  string r;
  int n= N(s);
  for (; i<n; i++) {
    char c= s[i];
    if (c == '\\') {
      i++;
      if (s[i] == 'n') r << '\n';
      else if (s[i] == '\\') r << '\\';
    }
    else if (c == '\n') {
      i++;
      break;
    }
    else r << c;
  }
  return r;
}

hashmap<string,string>
persistent_read_map (url file) {
  string s;
  load_string (file, s, false);
  hashmap<string,string> map;
  int i=0, n= N(s);
  while (i<n) {
    string key= read_escaped (s, i);
    string val= read_escaped (s, i);
    map (key)= val;
  }
  return map;
}

/******************************************************************************
* Cache management subroutines
******************************************************************************/

string
local_prefix (url dir) {
  string name= as_string (dir);
  if (!persistent_pre->contains (name)) {
    string prefix= as_string (N (persistent_pre) + 1) * ":";
    persistent_pre (name)= prefix;
    if (!is_directory (dir)) mkdir (dir);
    if (!is_directory (dir * url ("_"))) mkdir (dir * url ("_"));
  }
  return persistent_pre [name];
}

void
persistent_init_key (url dir, string key) {
  string v= local_prefix (dir) * key;
  if (!persistent_file->contains (v)) {
    persistent_file (v)= dir;
    persistent_hash (v)= (unsigned int) hash (key);
  }
}

void
persistent_update_key (url dir, string key, url file, unsigned int code) {
  unsigned int ncode= code / MAX_BRANCH;
  unsigned int mcode= code % MAX_BRANCH;
  url nfile= file * url (string ((char) (97 + mcode)));
  string v= local_prefix (dir) * key;
  persistent_file (v)= nfile;
  persistent_hash (v)= ncode;
}

/******************************************************************************
* Storing in cache
******************************************************************************/

int
total_size (hashmap<string,string> map) {
  int sum= 0;
  iterator<string> it= iterate (map);
  while (it->busy ()) {
    string key= it->next ();
    sum += N(key) + N(map[key]);
  }
  return sum;
}

void
persistent_write (url dir, string key, string val,
                  url file, unsigned int code)
{
  string v= local_prefix (dir) * key;
  if (is_directory (file)) {
    persistent_update_key (dir, key, file, code);
    persistent_write (dir, key, val,
                      persistent_file [v], persistent_hash [v]);
  }
  else {
    hashmap<string,string> map ("");
    if (is_regular (file))
      map= persistent_read_map (file);
    map (key)= val;
    if (code == 0 || (N (map) <= MAX_INNER && total_size (map) <= MAX_SIZE))
      persistent_write_map (file, map);
    else {
      remove (file);
      mkdir (file);
      iterator<string> it= iterate (map);
      while (it->busy ()) {
        string skey= it->next ();
        string sval= map [skey];
        string sv  = local_prefix (dir) * skey;
        unsigned int scode= persistent_hash [sv];
        persistent_write (dir, skey, sval, file, scode);
      }
    }
  }
}

void
persistent_set (url dir, string key, string val) {
  string v= local_prefix (dir) * key;
  persistent_init_key (dir, key);
  persistent_write (dir, key, val, persistent_file [v], persistent_hash [v]);
  persistent_has   (v)= true;
  persistent_cache (v)= val;
}

/******************************************************************************
* Removing from cache
******************************************************************************/

void
persistent_remove (url dir, string key, url file, unsigned int code) {
  string v= local_prefix (dir) * key;
  if (is_directory (file)) {
    persistent_update_key (dir, key, file, code);
    persistent_remove (dir, key, persistent_file [v], persistent_hash [v]);
  }
  else if (is_regular (file)) {
    hashmap<string,string> map= persistent_read_map (file);
    if (map->contains (key)) {
      map->reset (key);
      persistent_write_map (file, map);
    }
  }
}

void
persistent_reset (url dir, string key) {
  string v= local_prefix (dir) * key;
  persistent_init_key (dir, key);
  persistent_remove (dir, key, persistent_file [v], persistent_hash [v]);
  persistent_has (v)= false;
  persistent_cache->reset (v);
}

/******************************************************************************
* Retrieving from cache
******************************************************************************/

void
persistent_retrieve (url dir, string key, url file, unsigned int code) {
  string v= local_prefix (dir) * key;
  if (is_directory (file)) {
    persistent_update_key (dir, key, file, code);
    persistent_retrieve (dir, key, persistent_file [v], persistent_hash [v]);
  }
  else if (is_regular (file)) {
    hashmap<string,string> map= persistent_read_map (file);
    if (map->contains (key)) {
      persistent_cache (v)= map [key];
      persistent_has   (v)= true;
    }
    else persistent_has (v)= false;
  }
  else persistent_has (v)= false;
}

bool
persistent_contains (url dir, string key) {
  string v= local_prefix (dir) * key;
  if (!persistent_has->contains (v)) {
    persistent_init_key (dir, key);
    persistent_retrieve (dir, key, persistent_file [v], persistent_hash [v]);
  }
  return persistent_has [v];
}

string
persistent_get (url dir, string key) {
  string v= local_prefix (dir) * key;
  if (!persistent_has->contains (v)) {
    persistent_init_key (dir, key);
    persistent_retrieve (dir, key, persistent_file [v], persistent_hash [v]);
  }
  return persistent_cache [v];
}

/******************************************************************************
* Creation of persistent file names
******************************************************************************/

url
persistent_make_file_name (url dir, int nr, string suffix) {
  if (nr < MAX_FILES)
    return dir * url (string ((char) (97 + nr)) * suffix);
  else {
    int dnr = nr % MAX_FILES;
    int snr = (nr / MAX_FILES) - 1;
    url sdir= dir * url (string ((char) (65 + dnr)));
    if (!is_directory (sdir)) mkdir (sdir);
    return persistent_make_file_name (sdir, snr, suffix);
  }
}

url
persistent_file_name (url dir, string suffix) {
  (void) local_prefix (dir);
  dir= dir * url ("_");
  url nr= dir * url ("_");
  if (number_persistent_file_names == -1) {
    if (is_regular (nr)) {
      string s;
      load_string (nr, s, false);
      number_persistent_file_names= as_int (s);
    }
    else number_persistent_file_names= 0;
  }
  url r= persistent_make_file_name (dir, number_persistent_file_names, suffix);
  number_persistent_file_names++;
  save_string (nr, as_string (number_persistent_file_names));
  return r;
}
