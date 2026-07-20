
/******************************************************************************
* MODULE     : tree_cache.cpp
* DESCRIPTION: tree caching mechanism for server
* COPYRIGHT  : (C) 2026 Robin WILS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_cache.hpp"
#include "convert.hpp"
#include "drd_std.hpp"
#include "hashmap.hpp"
#include "image_files.hpp"
#include "iterator.hpp"
#include "scheme.hpp"
#include "tm_ostream.hpp"
#include "tm_timer.hpp"
#include "tree.hpp"
#include "tree_label.hpp"
#include "url.hpp"
#include "file.hpp"
#include <cerrno>
#include <unistd.h>

static const char* tree_cache_root= "$TEXMACS_HOME_PATH/system/tmp/tree_cache";

static tree_cache&
get_tree_cache (string host) {
  // lazy init otherwise constructor fails before texmacs init
  // Use tree cache per host to avoid collisions between different servers.
  static hashmap<string,tree_cache*> instances;
  if (instances->contains (host)) {
    return *(instances (host));
  }
  instances (host) = new tree_cache (string (tree_cache_root) * "/" * host);
  return *(instances (host));
}

// All hosts that currently own an on-disk cache directory.
static array<string>
tree_cache_hosts () {
  array<string> result;
  bool error;
  array<string> entries= read_directory (url (tree_cache_root), error);
  if (error) return result;
  for (int i= 0; i < N (entries); i++)
    if (entries[i] != "." && entries[i] != "..")
      result << entries[i];
  return result;
}

hashmap<string,cache_entry>&
cache_eviction_policy::cache_map () {
  return owner->cache;
}

disk_lru::disk_lru (tree_cache* o, int me, int ms)
: cache_eviction_policy (o, me, ms) {}

/******************************************************************************
* Disk LRU policy
******************************************************************************/

bool
disk_lru::should_evict () {
  return dir_size (owner->get_base_dir (), owner->get_depth ()) >= max_size;
}

int
disk_lru::current_size () {
  return dir_size (owner->get_base_dir (), owner->get_depth ());
}

int
disk_lru::dir_size (url u, int depth) {
  if (depth == 0) {
    return read_dir_size (u * "stats");
  }

  bool error;
  array<string> entries= read_directory (u, error);
  if (error) {
    io_error << "could not read dir " << u << LF;
    return 0;
  }

  int tot= 0;
  for (int idx=0; idx < N(entries); ++idx)
    tot += dir_size (u * entries[idx], depth - 1);
  return tot;
}

int
disk_lru::read_dir_size (url stats_u) {
  string s;
  if (load_string (stats_u, s, false, false)) return 0;
  return as_int (s);
}

void
disk_lru::set_dir_size (url stats_u, int total) {
  string s= as_string (total < 0 ? 0 : total);
  owner->persist (concretize (stats_u), s);
}

void
disk_lru::on_insert (string key, cache_entry& e) {
  url stats_u= url (owner->entry_dir (key)) * "stats";
  set_dir_size (stats_u, read_dir_size (stats_u) + e.size);
}

void
disk_lru::on_hit (string key, cache_entry& e) {
}

void
disk_lru::on_evict (string key) {
  url stats_u= url (owner->entry_dir (key)) * "stats";
  struct_stat buf;
  int sz= 0;
  if (texmacs_stat (owner->entry_path (key), &buf) == 0) sz= (int) buf.st_size;
  set_dir_size (stats_u, read_dir_size (stats_u) - sz);
}

void
disk_lru::scan_oldest (url u, int depth, string prefix,
                       string& best, time_t& best_atime, bool& found) {
  bool error;
  array<string> entries= read_directory (u, error);
  if (error) return;
  for (int i= 0; i < N(entries); i++) {
    string e= entries[i];
    if (e == "." || e == "..") continue;
    if (depth == 0) {
      if (e == "stats") continue;
      struct_stat buf;
      if (texmacs_stat (concretize (u * e), &buf) != 0) continue;
      time_t atime= buf.st_atime;
      if (!found || atime < best_atime) {
        best_atime= atime;
        best= prefix * e;
        found= true;
      }
    }
    else scan_oldest (u * e, depth - 1, prefix * e, best, best_atime, found);
  }
}

string
disk_lru::pick_victim () {
  string best;
  time_t best_at= 0;
  bool found= false;

  scan_oldest (owner->get_base_dir (), owner->get_depth (), "",
               best, best_at, found);

  return found ? best : string ();
}

void
disk_lru::recount () {
  recount_dir (owner->get_base_dir (), owner->get_depth ());
}

void
disk_lru::recount_dir (url u, int depth) {
  bool error;
  array<string> entries= read_directory (u, error);
  if (error) return;
  if (depth == 0) {
    int tot= 0;
    for (int i= 0; i < N (entries); i++) {
      string e= entries[i];
      if (e == "stats") continue;
      // skip files with dots (., .., extensions, hidden)
      bool dotted= false;
      for (int j= 0; j < N (e); j++) if (e[j] == '.') { dotted= true; break; }
      if (dotted) continue;
      struct_stat buf;
      if (texmacs_stat (concretize (u * e), &buf) == 0)
        tot += (int) buf.st_size;
    }
    set_dir_size (u * "stats", tot);
    return;
  }
  for (int i= 0; i < N (entries); i++) {
    string e= entries[i];
    if (e == "." || e == "..") continue;
    recount_dir (u * e, depth - 1);
  }
}

/******************************************************************************
* Tree cache
******************************************************************************/

tree_cache::tree_cache (string dir, int dd, int me, size_t ms)
  : base_dir (concretize (url (dir))),
    index_file (concretize (url (dir) * "index.scm")),
    dir_depth (dd),
    cache (cache_entry{.t=UNINIT}), size (0),
    policy (new disk_lru (this, me, ms)) {}

tree_cache::~tree_cache () {
  delete policy;
}

string
tree_cache::entry_dir (string h) {
  string d= base_dir;
  for (int i= 0; i < dir_depth; ++i)
    d= d * "/" * h (i, i+1);
  return d;
}

string
tree_cache::entry_path (string h) {
  return entry_dir (h) * "/" * h (dir_depth, N (h));
}

void
tree_cache::insert_entry (string key, tree t, int sz) {
  cache_entry e{.t= t, .size= sz, .hit_count= 0, .last_access= 0};
  policy->on_insert (key, e);
}

void
tree_cache::evict_if_needed () {
  while (policy->should_evict ()) {
    string victim= policy->pick_victim ();
    if (N (victim) == 0) break;
    // size -= cache (victim).size;          // RAM size counter (disabled)
    policy->on_evict (victim);
    remove (url (entry_path (victim)));
    // cache->reset (victim);                // RAM cache entry (disabled)
  }
}

void
tree_cache::run_janitor () {
  // One janitor across processes: take an exclusive, non-blocking lock on
  // janitor.lock. The OS lock is the mutex and is auto-released if the holder
  // crashes. Open with lock=false so the open does
  // not itself take a (blocking) lock, then lock explicitly non-blocking.
  url lock_url= url (base_dir) * "janitor.lock";
  FILE* lk= texmacs_fopen (concretize (lock_url), "a", false);
  if (lk == NULL) return;
  texmacs_lock_file (lk, true);
  // another process is already cleaning
  if (lk == NULL) return;

  // recount to catch potential inconsitencies on concurrent stats writes
  if (policy->should_evict ()) {
    io_info << "tree_cache janitor: eviction needed, cleaning..." << LF;
    policy->recount ();
    evict_if_needed ();
  } else {
    io_info << "tree_cache janitor: no eviction needed" << LF;
  }

  texmacs_fclose (lk);            // unlock + close
}

// Mostly used in tests for deterministic hash. (collision tests)
static uint64_t hash_limit= 0;

void
tree_hash_set_limit (int n) {
  hash_limit= (n <= 0) ? 0 : (uint64_t) n;
}

// hash to be used for Content addressed storage (filenames on disk)
uint64_t hash64 (string s) {
  int n=N(s);
  // golden ratio arbitrary seed
  uint64_t h = 0x9E3779B97F4A7C15;

  for (int i = 0; i < n; i++) {
    h ^= s[i];
    // 2^64 / pi
    // fx hasher does h = ((h << 5) | (h >> 59)) ^ s[i];
    // good constant: odd, dense
    h *= 0x517CC1B727220A95;
    h ^= h >> 17;
  }

  // finalizer: better avalanche/distribution (from murmur3 hash)
  h ^= h >> 33;
  h *= 0xff51afd7ed558ccd;
  h ^= h >> 33;

  if (hash_limit != 0) h %= hash_limit;
  return h;
}

static string
hash_as_hex (uint64_t h) {
  static const char* hex_string= "0123456789ABCDEF";
  static char buf[16];

  for (int i = 15; i >= 0; i--) {
    buf[i] = hex_string [h & 0xF];
    h >>= 4;
  }
  return string(buf, 16);
}

string
hash_hex (string s) {
  return hash_as_hex (hash64 (s));
}

string
tree_hash (tree t) {
  if (is_atomic (t))
    return hash_hex (t->label);
  string data= copy (as_string (L (t)));
  for (int i= 0; i < N (t); i++)
    data << tree_hash (t[i]);
  return hash_hex (data);
}

void
tree_cache::clear () {
  rmdir_recursive (url (base_dir));
  //cache->clear ();   // RAM mirror (unused in disk-only model, kept consistent)
  size= 0;
}

cache_status
tree_cache::status (string key) {
  string name= entry_path (key);
  texmacs_reset_last_error ();
  struct_stat buf;
  if (texmacs_stat (name, &buf) != 0) {
    if (errno == 0 || errno == ENOENT || errno == ENOTDIR) return CACHE_MISS;
    io_error << "could not stat tree cache " << name
      << ", " << texmacs_get_last_error_str () << LF;
    return CACHE_ERROR;
  }
  return CACHE_HIT;
}

cache_status
tree_cache::lookup (string key, tree& out) {
  string name= entry_path (key);
  texmacs_reset_last_error ();
  struct_stat buf;
  if (texmacs_stat (name, &buf) != 0) {
    // Unix/macOS/Windows: errno distinguishes missing from other failures.
    // Android (Qt) doesn't set errno; we default to MISS there since the
    // server (the only caller that branches on MISS vs ERROR via update())
    // doesn't run on Android.
    if (errno == 0 || errno == ENOENT || errno == ENOTDIR) return CACHE_MISS;
    io_error << "could not stat tree cache " << name
      << ", " << texmacs_get_last_error_str () << LF;
    return CACHE_ERROR;
  }
  ssize_t fsize= (ssize_t) buf.st_size;

  texmacs_reset_last_error ();
  FILE* fin= texmacs_fopen (name, "r", false);
  if (fin == NULL) {
    if (errno == 0 || errno == ENOENT || errno == ENOTDIR) return CACHE_MISS;
    io_error << "could not open tree cache " << name
      << ", " << texmacs_get_last_error_str () << LF;
    return CACHE_ERROR;
  }
  string content;
  content->resize (fsize);
  ssize_t readed= texmacs_fread (&content[0], fsize, fin);
  texmacs_fclose (fin);
  if (readed != fsize) {
    io_error << "short read on tree cache " << name << LF;
    return CACHE_ERROR;
  }

  tree t= stree_to_tree (string_to_object (content));
  if (key != tree_hash (t)) {
    io_error << "cache content from disk differs from requested : "
      << name << LF;
    return CACHE_ERROR;
  }

  // insert_entry here would re-count an already-stored blob into the shard
  // total; the bump belongs only on a fresh write (put/update).
  // insert_entry (key, t, (int) fsize);   // RAM populate (disabled)
  out= t;
  return CACHE_HIT;
}

bool
tree_cache::contains (string key) {
  tree dummy;
  // ERROR maps to false so a subsequent put() can self-repair a corrupt entry
  return lookup (key, dummy) == CACHE_HIT;
}

void
tree_cache::put (string key, tree t) {
  // if (cache->contains (key)) return;        // RAM guard (disabled)
  if (status (key) == CACHE_HIT) return;

  if (key != tree_hash (t)) return;

  string tree_str= object_to_string (tree_to_stree (t));
  string name= entry_path (key);
  mkdir (url (entry_dir (key)));
  if (!persist (name, tree_str)) return;

  insert_entry (key, t, N (tree_str));
}

tree
tree_cache::get (string key) {
  tree out;
  if (lookup (key, out) == CACHE_HIT) return out;
  return tree (UNINIT);
}

static bool
should_cache (tree t) {
  if (is_func (t, IMAGE) &&
      is_func (t[0], TUPLE, 2) &&
      is_func (t[0][0], RAW_DATA, 1))
    return the_drd->get_type (t) == TYPE_REGULAR;
  return is_func (t, GRAPHICS);
}

bool
tree_cache::persist (string name, string s) {
  texmacs_reset_last_error();

  int rnd= (int) (((unsigned int) random ()) & 0xffffff);
  url tmp_url= glue (name,
                     ".tmp-" * as_string (getpid ()) * "-" * as_string (rnd));
  string tmp_fname= concretize (tmp_url);

  // no flock, write to unique random tmp file + atomic move
  FILE* fout = texmacs_fopen (tmp_fname, "w", false);
  if (fout == NULL) {
    io_error << "could not open tree cache tmp file for writing " << tmp_fname
      << ", " << texmacs_get_last_error_str () << LF;
    return false;
  }

  int n= N(s);
  ssize_t written = texmacs_fwrite (&s[0], n, fout);
  texmacs_fclose (fout);

  bool ok= (written == n);
  if (!ok) {
    io_error << "could not write to tree cache " << name
      << ", " << texmacs_get_last_error_str () << LF;
  }

  if (!texmacs_rename (tmp_fname, name)) {
    io_error << "could not rename tree cache temp into place " << name << LF;
    remove (tmp_url);
    return false;
  }

  return ok;
}

tree
tree_cache::generate_cache_ref (string hash, tree ref) {
  tree cache_ref (CACHE_REF, hash,  as_string (L(ref)));
  if (is_func (ref, IMAGE) &&
    N(ref) == 5 &&
    is_func (ref[0], TUPLE, 2) &&
    is_func (ref[0][0], RAW_DATA, 1) &&
    is_atomic (ref[0][0][0]) && is_atomic (ref[0][1])) {
    int w{-1}, h{-1};

    url image= url_ramdisc (ref[0][0][0]->label) *
      url ("image." * ref[0][1]->label);

    image_size (image, w, h);

    cache_ref << ref[1] << ref[2];
    cache_ref << as_tree (w) << as_tree (h);
  }

  return cache_ref;
}


tree
tree_cache::update (tree t) {
  if (is_atomic (t)) return t;

  array<tree> children;
  for (int i= 0; i < N (t); i++)
    children << update (t[i]);

  tree result (L (t), children);
  if (should_cache (result)) {
    string h= tree_hash (result);

    tree existing;
    switch (lookup (h, existing)) {
      case CACHE_HIT:
        return existing == result ? generate_cache_ref (h, result) : result;
      case CACHE_ERROR:
        // disk is unreliable for this entry; serve the real tree inline
        // rather than a CACHE_REF the client cannot resolve
        return result;
      case CACHE_MISS:
        break;
    }

    string name= entry_path (h);
    mkdir (url (entry_dir (h)));

    string tree_str= object_to_string (tree_to_stree (t));
    if (!persist (name, tree_str))
      return result;
    debug_io << "wrote " << N(tree_str) << " bytes to tree cache at "
             << name << LF;
    insert_entry (h, result, N (tree_str));

    return generate_cache_ref (h, result);
  }

  return result;
}

void
tree_cache_clear (string host) {
  get_tree_cache (host).clear ();
}

void
tree_cache_clear_all () {
  array<string> hosts= tree_cache_hosts ();
  for (int i= 0; i < N (hosts); i++)
    get_tree_cache (hosts[i]).clear ();
}

bool
tree_cache_contains (string host, string key) {
  return get_tree_cache (host).contains (key);
}

void
tree_cache_put (string host, string key, tree t) {
  get_tree_cache (host).put (key, t);
}

tree
tree_cache_get (string host, string key) {
  return get_tree_cache (host).get (key);
}

// Read-only fallback for contexts without a tmfs host. Searches every host's
// cache on disk. Only resolve to a tree if there are no collisions, which means
// that multiple hosts share the same cache content.
tree
tree_cache_get_any (string key) {
  array<string> hosts= tree_cache_hosts ();
  tree found (UNINIT);
  for (int i= 0; i < N (hosts); i++) {
    tree t= get_tree_cache (hosts[i]).get (key);
    if (t == tree (UNINIT)) continue;

    if (found == tree (UNINIT)) {
      found= t;
    } else if (found != t) {
      return tree (UNINIT);
    }
  }
  return found;
}

tree
tree_cache_update (string host, tree t) {
  return get_tree_cache (host).update (t);
}

void
tree_cache_janitor (string host) {
  get_tree_cache (host).run_janitor ();
}

void
tree_cache_janitor_all () {
  array<string> hosts= tree_cache_hosts ();
  for (int i= 0; i < N (hosts); i++)
    get_tree_cache (hosts[i]).run_janitor ();
}

void
tree_cache_set_max_size (string host, int ms) {
  get_tree_cache (host).set_max_size (ms);
}

int
tree_cache_size (string host) {
  return get_tree_cache (host).disk_size ();
}
