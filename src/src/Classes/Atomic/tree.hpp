
/******************************************************************************
* MODULE     : tree.hpp
* DESCRIPTION: fixed size trees with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TREE_H
#define TREE_H
#include "tree_label.hpp"
#include "observer.hpp"
#include "array.hpp"

/******************************************************************************
* The tree class 'tree'
******************************************************************************/

class tree_rep;
class atomic_rep;
class compound_rep;
class tree {
  tree_rep* rep; // can be atomic or compound
public:
#ifdef OS_WIN32
  static const tree_label init; // used by hashmap<tree>() constructor
#else
  static const tree_label init = UNINIT; // used by hashmap<tree>() constructor
#endif

  inline tree (const tree& x);
  inline ~tree ();
  inline atomic_rep* operator -> ();
  inline tree& operator = (tree x);

  inline tree ();
  inline tree (string l);
  inline tree (char* l);
  inline tree (tree_label l, int n=0);
  inline tree (tree_label l, array<tree> a);
  inline tree (tree t, int n);
  tree (tree_label l, tree t1);
  tree (tree_label l, tree t1, tree t2);
  tree (tree_label l, tree t1, tree t2, tree t3);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6);
  inline tree& operator [] (int i);
  tree operator () (int start, int end);

  friend inline int N (tree t);
  friend inline int arity (tree t);
  friend inline tree_label L (tree t);
  friend inline array<tree> A (tree t);
  friend inline array<tree>& AR (tree t);
  friend inline bool is_atomic (tree t);
  friend inline bool is_compound (tree t);
  friend inline bool operator == (tree t, tree_label lab);
  friend inline bool operator != (tree t, tree_label lab);
  friend inline bool operator == (tree t, string s);
  friend inline bool operator != (tree t, string s);
  friend inline bool operator == (tree t, char* s);
  friend inline bool operator != (tree t, char* s);
  friend inline bool is_func (tree t, tree_label l);
  friend inline bool is_func (tree t, tree_label l, int i);

  friend tree copy (tree t);
  friend bool operator == (tree t, tree u);
  friend bool operator != (tree t, tree u);
  friend tree& operator << (tree& t, tree t2);
  friend tree& operator << (tree& t, array<tree> a);
  friend ostream& operator << (ostream& out, tree t);
  friend tree operator * (tree t1, tree t2);
  friend void print_tree (tree t, int tab=0);
};

class tree_rep: concrete_struct {
public:
  const tree_label op;
  observer obs;
  inline tree_rep (tree_label op2): op (op2) {}
  friend class tree;
};

class atomic_rep: public tree_rep {
public:
  string label;
  inline atomic_rep (string l): tree_rep (STRING), label (l) {}
  friend class tree;
};

class compound_rep: public tree_rep {
public:
  array<tree> a;
  inline compound_rep (tree_label l, array<tree> a2): tree_rep (l), a (a2) {}
  friend class tree;
};

typedef tree scheme_tree;

/******************************************************************************
* Routines for trees
******************************************************************************/

#ifdef debug_trees
#define CHECK_ATOMIC(t,s) \
  if (((t).rep)->op != STRING) { \
    cerr << "\nThe tree : " << (t) << "\n"; \
    fatal_error ("atomic tree expected", s); \
  }
#define CHECK_COMPOUND(t,s) \
  if (((t).rep)->op == STRING) { \
    cerr << "\nThe tree : " << (t) << "\n"; \
    fatal_error ("compound tree expected", s); \
  }
#else
#define CHECK_ATOMIC(t,s)
#define CHECK_COMPOUND(t,s)
#endif

void destroy_tree_rep (tree_rep* rep);
inline tree::tree (const tree& x): rep (x.rep) { rep->ref_count++; }
inline tree::~tree () {
  if ((--rep->ref_count)==0) { destroy_tree_rep (rep); rep= NULL; } }
inline atomic_rep* tree::operator -> () {
  CHECK_ATOMIC (*this, "tree::operator ->");
  return static_cast<atomic_rep*> (rep); }
inline tree& tree::operator = (tree x) {
  x.rep->ref_count++;
  if ((--rep->ref_count)==0) destroy_tree_rep (rep);
  rep= x.rep;
  return *this; }

inline tree::tree ():
  rep (new atomic_rep (string ())) {}
inline tree::tree (char *s):
  rep (new atomic_rep (s)) {}
inline tree::tree (string s):
  rep (new atomic_rep (s)) {}
inline tree::tree (tree_label l, int n):
  rep (new compound_rep (l, array<tree> (n))) {}
inline tree::tree (tree_label l, array<tree> a):
  rep (new compound_rep (l, a)) {}
inline tree::tree (tree t, int n):
  rep (new compound_rep (t.rep->op, array<tree> (n))) {
    CHECK_COMPOUND (t, "tree::tree (tree, int)"); }

inline tree& tree::operator [] (int i) {
  CHECK_COMPOUND (*this, "tree::operator []");
  return (static_cast<compound_rep*> (rep))->a[i]; }
inline int N (tree t) {
  CHECK_COMPOUND (t, "N (tree)");
  return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int arity (tree t) {
  if (t.rep->op == STRING) return 0;
  else return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int right_index (tree t) {
  return is_atomic (t)? N(t->label): 1; }
inline tree_label L (tree t) {
  return t.rep->op; }
inline array<tree> A (tree t) {
  CHECK_COMPOUND (t, "A (tree)");
  return (static_cast<compound_rep*> (t.rep))->a; }
inline array<tree>& AR (tree t) {
  CHECK_COMPOUND (t, "AR (tree)");
  return (static_cast<compound_rep*> (t.rep))->a; }

inline bool is_atomic (tree t) { return (t.rep->op == STRING); }
inline bool is_compound (tree t) { return (t.rep->op != STRING); }
inline string get_label (tree t) {
  return is_atomic (t)? t->label: copy (as_string (L(t))); }
inline bool operator == (tree t, tree_label lab) {
  return (t.rep->op == lab) && (N(t)==0); }
inline bool operator != (tree t, tree_label lab) {
  return (t.rep->op != lab) || (N(t)!=0); }
inline bool operator == (tree t, string s) {
  return (t.rep->op == STRING) && (t->label == s); }
inline bool operator != (tree t, string s) {
  return (t.rep->op != STRING) || (t->label != s); }
inline bool operator == (tree t, char* s) {
  return (t.rep->op == STRING) && (t->label == s); }
inline bool operator != (tree t, char* s) {
  return (t.rep->op != STRING) || (t->label != s); }

inline bool is_func (tree t, tree_label l) {
  return (t.rep->op==l) && (N(t)!=0); }
inline bool is_func (tree t, tree_label l, int i) {
  return (t.rep->op==l) && (N(t)==i); }

inline bool is_bool (tree t) { return is_atomic (t) && is_bool (t->label); }
inline bool is_int (tree t) { return is_atomic (t) && is_int (t->label); }
inline bool is_double (tree t) { return is_atomic (t) && is_double(t->label); }
inline bool is_string (tree t) { return is_atomic (t); }
inline bool as_bool (tree t) {
  if (is_atomic (t)) return as_bool (t->label);
  else return false; }
inline int as_int (tree t) {
  if (is_atomic (t)) return as_int (t->label);
  else return 0; }
inline double as_double (tree t) {
  if (is_atomic (t)) return as_double (t->label);
  else return 0.0; }
inline string as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else return ""; }
template<class T> inline tree as_tree(T x) { return (tree) x; }
template<> inline tree as_tree(pointer x) { return "?"; }

/******************************************************************************
* Data
******************************************************************************/

bool is_document (tree t);
bool is_concat (tree t);
bool is_format (tree t);
bool is_formatting (tree t);
bool is_table (tree t);
bool is_table_format (tree t);
bool is_multi_paragraph (tree t);
bool is_script (tree t);
bool is_script (tree t, bool& right);
bool is_prime (tree t);
bool is_mod_active (tree t);
bool is_mod_active_once (tree t);
bool is_empty (tree t);

inline bool
is_applicable (tree t) {
  return is_compound (t) && (N(t) >= 1) &&
    ((L(t) == MACRO) || (L(t) == FUNC) || (L(t) == XMACRO));
}

tree simplify_concat (tree t);
tree simplify_correct (tree t);

/******************************************************************************
* Compound trees
******************************************************************************/

tree compound (string s);
tree compound (string s, tree t1);
tree compound (string s, tree t1, tree t2);
tree compound (string s, tree t1, tree t2, tree t3);
tree compound (string s, tree t1, tree t2, tree t3, tree t4);
bool is_extension (tree t);
bool is_extension (tree t, int n);
bool is_compound (tree t, string s);
bool is_compound (tree t, string s, int n);

/******************************************************************************
* Tuples
******************************************************************************/

inline tree tuple () {
  return tree (TUPLE); }
inline tree tuple (tree t1) {
  return tree (TUPLE, t1); }
inline tree tuple (tree t1, tree t2) {
  return tree (TUPLE, t1, t2); }
inline tree tuple (tree t1, tree t2, tree t3) {
  return tree (TUPLE, t1, t2, t3); }
inline tree tuple (tree t1, tree t2, tree t3, tree t4) {
  return tree (TUPLE, t1, t2, t3, t4); }

inline bool is_tuple (tree t) {
  return (L(t) == TUPLE); }
inline bool is_tuple (tree t, string s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, char* s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, string s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }
inline bool is_tuple (tree t, char* s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }

/******************************************************************************
* Miscellaneous
******************************************************************************/

tree   correct (tree t);
int    hash (tree t);

template<class T>
array<T>::operator tree () {
  int i, n=rep->n;
  tree t (TUPLE, n);
  for (i=0; i<n; i++)
    t[i]= as_tree(rep->a[i]);
  return t;
}

#endif // defined TREE_H
