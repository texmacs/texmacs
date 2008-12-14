
/******************************************************************************
* MODULE     : list_environment.hpp
* DESCRIPTION: linked lists of several environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LIST_ENVIRONMENT_H
#define LIST_ENVIRONMENT_H
#include "basic_environment.hpp"

/******************************************************************************
* List environments
******************************************************************************/

class list_environment_rep;
class list_environment {
  ABSTRACT_NULL(list_environment);
  inline list_environment (basic_environment env);
  inline list_environment (basic_environment env, list_environment next);
  inline friend environment as_environment (const list_environment& env) {
    return environment ((environment_rep*) env.rep); }
  inline friend list_environment as_list_environment (const environment& env) {
    return list_environment ((list_environment_rep*) as_pointer (env)); }
};

class list_environment_rep: public environment_rep {
  static tree uninit;
public:
  basic_environment env;  // local environment
  list_environment next;  // next environment
  int misses;             // count for number of local environment misses

public:
  inline list_environment_rep (basic_environment env2, list_environment next2):
    env (env2), next (next2), misses (0) {}

  void compress ();
  tree* raw_read (int key);

  inline bool contains (int key) {
    return raw_read (key) != 0; }
  inline tree read (int key) {
    tree* ptr= raw_read (key);
    return ptr==NULL? uninit: *ptr; }
  inline void write (int key, const tree& val) {
    env->write (key, val); }
  inline void remove (int key) {
    // NOTE: it is not allowed to change next, in the case
    // when 'key' does not exist in the local environment 'env'
    env->remove (key); }
  void print (const string& prefix);
};

ABSTRACT_NULL_CODE(list_environment);
inline list_environment::list_environment
  (basic_environment env):
    rep (new list_environment_rep (env, list_environment ())) {}
inline list_environment::list_environment
  (basic_environment env, list_environment next):
    rep (new list_environment_rep (env, next)) {}

int total_size (list_environment l);
basic_environment flatten (list_environment l);

#endif // defined LIST_ENVIRONMENT_H
