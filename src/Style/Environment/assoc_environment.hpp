
/******************************************************************************
* MODULE     : assoc_environment.hpp
* DESCRIPTION: association arrays as environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ASSOC_ENVIRONMENT_H
#define ASSOC_ENVIRONMENT_H
#include "environment.hpp"

/******************************************************************************
* An assoc environment is represented by an array of assoc_nodes
******************************************************************************/

class assoc_node {
  static tree uninit;
public:
  int  key;    // key of the pair
  tree val;    // value of the pair
public:
  inline assoc_node (): val (uninit) {}
};

/******************************************************************************
* Assoc environments
******************************************************************************/

class assoc_environment;
class assoc_environment_rep: public environment_rep {
  static tree uninit;
public:
  int n;
  assoc_node* a;
public:
  inline assoc_environment_rep (int n2):
    n (n2), a (new assoc_node[n]) {}
  inline ~assoc_environment_rep () {
    delete[] a; }
  inline void raw_write (int i, int key, const tree& val) {
    a[i].key= key; a[i].val= val; }
  inline void raw_write (int i, const string& key, const tree& val) {
    a[i].key= (int) make_tree_label (key); a[i].val= val; }

  bool contains (int key);
  tree read (int key);
  void write (int key, const tree& val);
  void remove (int key);
  void print (const string& prefix);
};

class assoc_environment {
  ABSTRACT(assoc_environment);
  inline assoc_environment (int n):
    rep (new assoc_environment_rep (n)) {}
  inline tree operator [] (int key) {
    return rep->read (key); }
  inline friend environment as_environment (const assoc_environment& env) {
    return environment ((environment_rep*) env.rep); }
  inline friend assoc_environment as_assoc_environment (const environment& e) {
    return assoc_environment ((assoc_environment_rep*) as_pointer (e)); }
};
ABSTRACT_CODE(assoc_environment);

assoc_environment copy (assoc_environment env);
int weak_hash (assoc_environment env);
bool weak_equal (assoc_environment env1, assoc_environment env2);

#endif // defined ASSOC_ENVIRONMENT_H
