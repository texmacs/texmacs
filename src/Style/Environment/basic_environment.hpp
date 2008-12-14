
/******************************************************************************
* MODULE     : basic_environment.hpp
* DESCRIPTION: hash tables as environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BASIC_ENVIRONMENT_H
#define BASIC_ENVIRONMENT_H
#include "assoc_environment.hpp"

/******************************************************************************
* Basic environments are represented by an array of hash_nodes of length n=2^p
* The hash table both has n slots and maximal size n. The association list
* corresponding to hash code h starts at index 'a[h%n].start'. Each item in
* the list is a hash_node with a key-value pair and the index 'next' of
* the next item. The end of the list is indicated by the index -1.
* A list with unused space is also maintained and it starts at index 'free'.
******************************************************************************/

class basic_environment_rep;
class hash_node {
  static tree uninit;
public:
  int  key;    // key of the pair
  tree val;    // value of the pair
  int  start;  // index of list associated to hash code
  int  next;   // next index in list after this pair (or next free space)
public:
  inline hash_node (): val (uninit), start (-1) {}
};

/******************************************************************************
* Basic environments
******************************************************************************/

class basic_environment_rep: public environment_rep {
  static tree uninit;
public:
  int size;      // total number of elements
  int n;         // allocated number of bags (power of two and size <= n)
  hash_node* a;  // the nodes, which are bags at the same time
  int free;      // index of free space

public:
  inline basic_environment_rep (int n2):
    size (0), n (n2), a (new hash_node[n]), free (0) {
      for (int i=0; i<n; i++) a[i].next= i+1; }
  inline ~basic_environment_rep () {
    delete[] a; }

  void raw_insert (int key, const tree& val);
  void raw_write (int key, const tree& val);
  tree* raw_read (int key);
  void raw_remove (int key);
  void multiple_insert (hash_node* b, int k);
  void multiple_write (hash_node* b, int k);
  void multiple_remove (hash_node* b, int k);
  void resize (int new_n);

  inline bool contains (int key) {
    return raw_read (key) != 0; }
  inline tree read (int key) {
    tree* ptr= raw_read (key);
    return ptr==NULL? uninit: *ptr; }
  inline void write (int key, const tree& val) {
    if (size >= n) resize (n << 1);
    raw_write (key, val); }
  inline void remove (int key) {
    raw_remove (key);
    if (size < (n>>2)) resize (n>>1); }
  void print (const string& prefix);
};

class basic_environment {
  ABSTRACT_NULL(basic_environment);
  inline tree operator [] (int key) {
    return rep->read (key); }
  inline basic_environment (int n):
    rep (new basic_environment_rep (n)) {}
  inline basic_environment (assoc_environment env):
    rep (new basic_environment_rep (round_pow2 (env->n))) {
      for (int i=0; i<env->n; i++)
	rep->raw_insert (env->a[i].key, env->a[i].val); }
  inline friend environment as_environment (const basic_environment& env) {
    return environment ((environment_rep*) env.rep); }
  inline friend basic_environment as_basic_environment (const environment& e) {
    return basic_environment ((basic_environment_rep*) as_pointer (e)); }
};
ABSTRACT_NULL_CODE(basic_environment);

#endif // defined BASIC_ENVIRONMENT_H
