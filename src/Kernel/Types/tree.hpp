
/******************************************************************************
* MODULE     : tree.hpp
* DESCRIPTION: fixed size trees with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_H
#define TREE_H
#include "tree_label.hpp"
#include "observer.hpp"
#include "array.hpp"

/******************************************************************************
* The tree class 'tree'
******************************************************************************/

class tree;
class tree_rep;
class atomic_rep;
class compound_rep;
class generic_rep;
class blackbox;
tree copy (tree t);

class tree {
  tree_rep* rep; // can be atomic or compound or generic
  inline tree (tree_rep* rep2);

public:
  inline tree (const tree& x);
  inline ~tree ();
  inline atomic_rep* operator -> ();
  inline tree& operator = (tree x);

  inline tree ();
  inline tree (string l);
  inline tree (const char* l);
  inline tree (tree_label l, int n=0);
  inline tree (tree_label l, array<tree> a);
  inline tree (tree t, int n);
  tree (tree_label l, tree t1);
  tree (tree_label l, tree t1, tree t2);
  tree (tree_label l, tree t1, tree t2, tree t3);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4,
	              tree t5, tree t6, tree t7);
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4,
	              tree t5, tree t6, tree t7, tree t8);
  inline tree& operator [] (int i);
  tree operator () (int start, int end);

  friend inline int N (tree t);
  friend inline int arity (tree t);
  friend inline tree_label L (tree t);
  friend inline tree_label& LR (tree t);
  friend inline array<tree> A (tree t);
  friend inline array<tree>& AR (tree t);
  friend inline bool is_atomic (tree t);
  friend inline bool is_compound (tree t);
  friend inline bool is_generic (tree t);
  friend inline bool operator == (tree t, tree_label lab);
  friend inline bool operator != (tree t, tree_label lab);
  friend inline bool operator == (tree t, string s);
  friend inline bool operator != (tree t, string s);
  friend inline bool operator == (tree t, const char* s);
  friend inline bool operator != (tree t, const char* s);
  friend inline tree_rep* inside (tree t);
  friend inline bool strong_equal (tree t, tree u);
  friend inline bool is_func (tree t, tree_label l);
  friend inline bool is_func (tree t, tree_label l, int i);

  friend tree copy (tree t);
  friend tree freeze (tree t);
  friend bool operator == (tree t, tree u);
  friend bool operator != (tree t, tree u);
  friend tree& operator << (tree& t, tree t2);
  friend tree& operator << (tree& t, array<tree> a);
  friend tm_ostream& operator << (tm_ostream& out, tree t);
  friend tree operator * (tree t1, tree t2);
  friend void print_tree (tree t, int tab);
  friend list<tree> as_trees (list<pointer> l);
  friend class tree_pointer_rep;
  friend class tree_position_rep;
  friend class tree_addendum_rep;
  friend class edit_observer_rep;
  friend class undo_observer_rep;
  friend class tree_links_rep;
  friend class link_repository_rep;
#ifdef QTTEXMACS
  friend class QTMTreeModel;  // hack: wouldn't need it with a widget_observer
#endif
  friend blackbox as_blackbox (const tree& t);
};

class tree_rep: concrete_struct {
public:
  tree_label op;
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

// generic_rep in generic_tree.hpp

template<> struct type_helper<tree> {
  static int  id;
  static tree init;
  static inline tree init_val () { return tree (); }
};

typedef tree scheme_tree;

/******************************************************************************
* Routines for trees
******************************************************************************/

#ifdef debug_trees
#define CHECK_ATOMIC(t) \
  if (((t).rep)->op != STRING) { \
    failed_error << "The tree : " << (t) << "\n"; \
    FAILED ("atomic tree expected"); \
  }
#define CHECK_COMPOUND(t) \
  if (((t).rep)->op == STRING) { \
    failed_error << "The tree : " << (t) << "\n"; \
    FAILED ("compound tree expected"); \
  }
#else
#define CHECK_ATOMIC(t)
#define CHECK_COMPOUND(t)
#endif

void destroy_tree_rep (tree_rep* rep);
inline tree::tree (tree_rep* rep2): rep (rep2) { rep->ref_count++; }
inline tree::tree (const tree& x): rep (x.rep) { rep->ref_count++; }
inline tree::~tree () {
  if ((--rep->ref_count)==0) { destroy_tree_rep (rep); rep= NULL; } }
inline atomic_rep* tree::operator -> () {
  CHECK_ATOMIC (*this);
  return static_cast<atomic_rep*> (rep); }
inline tree& tree::operator = (tree x) {
  x.rep->ref_count++;
  if ((--rep->ref_count)==0) destroy_tree_rep (rep);
  rep= x.rep;
  return *this; }

inline tree::tree ():
  rep (tm_new<atomic_rep> (string ())) {}
inline tree::tree (const char *s):
  rep (tm_new<atomic_rep> (s)) {}
inline tree::tree (string s):
  rep (tm_new<atomic_rep> (s)) {}
inline tree::tree (tree_label l, int n):
  rep (tm_new<compound_rep> (l, array<tree> (n))) {}
inline tree::tree (tree_label l, array<tree> a):
  rep (tm_new<compound_rep> (l, a)) {}
inline tree::tree (tree t, int n):
  rep (tm_new<compound_rep> (t.rep->op, array<tree> (n))) {
    CHECK_COMPOUND (t); }

inline tree& tree::operator [] (int i) {
  CHECK_COMPOUND (*this);
  return (static_cast<compound_rep*> (rep))->a[i]; }
inline int N (tree t) {
  CHECK_COMPOUND (t);
  return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int arity (tree t) {
  if (t.rep->op == STRING) return 0;
  else return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int right_index (tree t) {
  return is_atomic (t)? N(t->label): 1; }
inline tree_label L (tree t) {
  return t.rep->op; }
inline tree_label& LR (tree t) {
  return t.rep->op; }
inline array<tree> A (tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a; }
inline array<tree>& AR (tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a; }

inline bool is_atomic (tree t) { return (((int) t.rep->op) == 0); }
inline bool is_compound (tree t) { return (((int) t.rep->op) > STRING); }
inline bool is_generic (tree t) { return ((int) t.rep->op) < 0; }
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
inline bool operator == (tree t, const char* s) {
  return (t.rep->op == STRING) && (t->label == s); }
inline bool operator != (tree t, const char* s) {
  return (t.rep->op != STRING) || (t->label != s); }
inline tree_rep* inside (tree t) {
  return t.rep; }
inline bool strong_equal (tree t, tree u) {
  return t.rep == u.rep; }

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
inline long int as_long_int (tree t) {
  if (is_atomic (t)) return as_long_int (t->label);
  else return 0; }
inline double as_double (tree t) {
  if (is_atomic (t)) return as_double (t->label);
  else return 0.0; }
inline string as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else return ""; }
string tree_as_string (tree t);
tree replace (tree t, tree w, tree b);
template<class T> inline tree as_tree (T x) { return (tree) x; }
template<> inline tree as_tree (int x) { return as_string (x); }
template<> inline tree as_tree (long int x) { return as_string (x); }
template<> inline tree as_tree (double x) { return as_string (x); }
template<> inline tree as_tree (pointer x) { (void) x; return "pointer"; }
inline tree bool_as_tree (bool f) {
  return (f? tree ("true"): tree ("false")); }

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
bool is_around (tree t);
bool is_script (tree t);
bool is_script (tree t, bool& right);
bool is_prime (tree t);
bool is_left_script_prime (tree t);
bool is_right_script_prime (tree t);
bool is_mod_active (tree t);
bool is_mod_active_once (tree t);
bool is_graphical_text (tree t);
bool is_empty (tree t);
bool is_multi_line (tree t);

inline bool
is_applicable (tree t) {
  return is_compound (t) && (N(t) >= 1) &&
    ((L(t) == MACRO) || (L(t) == FUNC) || (L(t) == XMACRO));
}

tree simplify_concat (tree t);
tree simplify_document (tree t);
tree simplify_correct (tree t);

/******************************************************************************
* Compound trees
******************************************************************************/

tree compound (string s);
tree compound (string s, tree t1);
tree compound (string s, tree t1, tree t2);
tree compound (string s, tree t1, tree t2, tree t3);
tree compound (string s, tree t1, tree t2, tree t3, tree t4);
tree compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5);
tree compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6);
tree compound (string s, array<tree> a);
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
inline tree tuple (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (TUPLE, t1, t2, t3, t4, t5); }

inline bool is_tuple (tree t) {
  return (L(t) == TUPLE); }
inline bool is_tuple (tree t, string s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, const char* s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, string s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }
inline bool is_tuple (tree t, const char* s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }

/******************************************************************************
* Other frequent markup
******************************************************************************/

inline tree concat () {
  return tree (CONCAT); }
inline tree concat (tree t1) {
  return tree (CONCAT, t1); }
inline tree concat (tree t1, tree t2) {
  return tree (CONCAT, t1, t2); }
inline tree concat (tree t1, tree t2, tree t3) {
  return tree (CONCAT, t1, t2, t3); }
inline tree concat (tree t1, tree t2, tree t3, tree t4) {
  return tree (CONCAT, t1, t2, t3, t4); }
inline tree concat (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (CONCAT, t1, t2, t3, t4, t5); }

inline tree document () {
  return tree (DOCUMENT); }
inline tree document (tree t1) {
  return tree (DOCUMENT, t1); }
inline tree document (tree t1, tree t2) {
  return tree (DOCUMENT, t1, t2); }
inline tree document (tree t1, tree t2, tree t3) {
  return tree (DOCUMENT, t1, t2, t3); }
inline tree document (tree t1, tree t2, tree t3, tree t4) {
  return tree (DOCUMENT, t1, t2, t3, t4); }
inline tree document (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (DOCUMENT, t1, t2, t3, t4, t5); }

inline tree verbatim (tree t1) {
  return compound ("verbatim", t1); }

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

class formatted {
public:
  tree rep;
  inline formatted (tree t): rep (t) {}
  inline formatted (const formatted& f): rep (f.rep) {}
};

void print_tree (tree t, int tab=0);

#endif // defined TREE_H
