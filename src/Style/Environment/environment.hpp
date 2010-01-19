
/******************************************************************************
* MODULE     : environment.hpp
* DESCRIPTION: abstract environments for style rewriting
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include "tree.hpp"

//#define CLASSICAL_MACRO_EXPANSION
#define ALTERNATIVE_MACRO_EXPANSION

/******************************************************************************
* Helper functions which should be moved elsewhere
******************************************************************************/

inline int
weak_hash (tree t) {
  tree_rep* rep= t.operator -> ();
  return hash ((void*) rep);
}

inline bool
weak_equal (tree t1, tree t2) {
  tree_rep* rep1= t1.operator -> ();
  tree_rep* rep2= t2.operator -> ();
  return rep1 == rep2;
}

inline int
round_pow2 (int i) {
  int n=1;
  while (n<i) n <<= 1;
  return n;
}

/******************************************************************************
* Abstract environments
******************************************************************************/

class environment;
class environment_rep: public concrete_struct {
public:
  inline environment_rep () {}
  inline virtual ~environment_rep () {}

  virtual bool contains (int key) = 0;
  virtual tree read (int key) = 0;
  virtual void write (int key, const tree& val) = 0;
  virtual void remove (int key) = 0;
  virtual void print (const string& prefix) = 0;

  inline bool contains (const string& key) {
    return contains ((int) make_tree_label (key)); }
  inline tree read (const string& key) {
    return read ((int) make_tree_label (key)); }
  inline void write (const string& key, const tree& val) {
    write ((int) make_tree_label (key), val); }
  inline void remove (const string& key) {
    remove ((int) make_tree_label (key)); }

  friend class environment;
};

class environment {
  ABSTRACT_NULL(environment);
  inline tree operator [] (int key) {
    return rep->read (key); }
  inline tree operator [] (const string& key) {
    return rep->read (key); }
  inline friend environment_rep* as_pointer (const environment& env) {
    return env.rep; }
  inline friend int weak_hash (environment env) {
    return hash ((void*) env.rep); }
  inline friend bool weak_equal (environment env1, environment env2) {
    return env1.rep == env2.rep; }
};
ABSTRACT_NULL_CODE(environment);

void test_environments ();

#endif // defined ENVIRONMENT_H
