
/******************************************************************************
* MODULE     : tree_cache.hpp
* DESCRIPTION: tree caching mechanism for server
* COPYRIGHT  : (C) 2026 Robin WILS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_CACHE_H
#define TREE_CACHE_H

#include "tree.hpp"
#include "string.hpp"
#include "url.hpp"
#include "hashmap.hpp"

struct cache_entry {
  tree t;
  int size;
  int hit_count;
  time_t last_access;
};

class tree_cache;

enum cache_status { CACHE_HIT, CACHE_MISS, CACHE_ERROR };

class cache_eviction_policy {
protected:
  tree_cache* owner;
  hashmap<string,cache_entry>& cache_map ();
  int max_entries;
  int max_size;
public:
  cache_eviction_policy (tree_cache* o, int me, int ms)
  : owner (o), max_entries (me), max_size (ms) {}
  virtual ~cache_eviction_policy () {}
  virtual bool   should_evict () = 0;
  virtual void   on_insert (string key, cache_entry& e) { (void) key; (void) e; }
  virtual void   on_hit    (string key, cache_entry& e) { (void) key; (void) e; }
  virtual void   on_evict  (string key) { (void) key; }
  virtual void   recount   () {}
  virtual string pick_victim () = 0;
  virtual int    current_size () { return 0; }
  void   set_max_size (int ms) { max_size= ms; }
  int    get_max_size () const { return max_size; }
};

class disk_lru : public cache_eviction_policy {
public:
  disk_lru (tree_cache* o, int me, int ms);
  bool   should_evict () override;
  void   on_insert (string key, cache_entry& e) override;
  void   on_hit    (string key, cache_entry& e) override;
  void   on_evict  (string key) override;
  void   recount   () override;
  string pick_victim () override;
  int    current_size () override;

private:
  void recount_dir (url u, int depth);
  int  dir_size (url u, int depth);
  int  read_dir_size (url stats_u);
  void set_dir_size (url stats_u, int total);
  void scan_oldest (url u, int depth, string prefix,
                    string& best, time_t& best_at, bool& found);
};

class tree_cache {
  friend class cache_eviction_policy;
  friend class disk_lru;   // uses persist() for atomic stats writes
public:
  tree_cache (string dir,
              int    dir_depth     = 2,
              int    max_entries   = 1024,
              size_t max_size      = 500 * 1024 * 1024); // 500 MB

  ~tree_cache ();
  void   clear ();
  bool   contains (string key);
  void   put (string key, tree t);
  tree   get (string key);
  tree   update (tree t);
  void   run_janitor ();
  void   set_max_size (int ms) { policy->set_max_size (ms); }
  int    disk_size () { return policy->current_size (); }
  int    entries () const { return N(cache); }
  int    get_size () const { return size; }
  int    get_depth () const { return dir_depth; }
  string get_base_dir () const { return base_dir; }
  string get_base_dir (string host, string port) const {
    return as_string (url (dir_depth) * host * port);
  }
  string entry_path (string hash);
  string entry_dir  (string hash);

private:
  cache_status status (string key);
  cache_status lookup (string key, tree& out);

  bool   persist (string name, string s);
  void   insert_entry (string key, tree t, int sz);
  void   evict_if_needed ();
  tree   generate_cache_ref (string hash, tree ref);

  string base_dir;
  string index_file;
  int    dir_depth;
  hashmap<string,cache_entry> cache;
  size_t size;
  cache_eviction_policy* policy;
};

string tree_hash (tree t);
void   tree_hash_set_limit (int n); // test-only: force hash collisions

void   tree_cache_clear (string host);
void   tree_cache_clear_all ();
bool   tree_cache_contains (string host, string key);
void   tree_cache_put (string host, string key, tree t);
tree   tree_cache_get (string host, string key);
tree   tree_cache_get_any (string key); // search every host
tree   tree_cache_update (string host, tree t);
void   tree_cache_janitor (string host);
void   tree_cache_janitor_all ();
void   tree_cache_set_max_size (string host, int ms);
int    tree_cache_size (string host);

#endif // defined TREE_CACHE_H
