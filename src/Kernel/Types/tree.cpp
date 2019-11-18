
/******************************************************************************
* MODULE     : tree.cpp
* DESCRIPTION: fixed size trees with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "generic_tree.hpp"
#include "drd_std.hpp"
#include "hashset.hpp"

/******************************************************************************
* Main routines for trees
******************************************************************************/

tree type_helper<tree>::init (UNINIT);
int type_helper<tree>::id  = new_type_identifier ();

void
destroy_tree_rep (tree_rep* rep) {
  if (((int) rep->op) == 0) tm_delete (static_cast<atomic_rep*> (rep));
  else if (((int) rep->op) > 0) tm_delete (static_cast<compound_rep*>(rep));
  else tm_delete (static_cast<generic_rep*>(rep));
}

tree::tree (tree_label l, tree t1):
  rep (tm_new<compound_rep> (l, array<tree> (1)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
}

tree::tree (tree_label l, tree t1, tree t2):
  rep (tm_new<compound_rep> (l, array<tree> (2)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3):
  rep (tm_new<compound_rep> (l, array<tree> (3)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3, tree t4):
  rep (tm_new<compound_rep> (l, array<tree> (4)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
}

tree::tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5):
  rep (tm_new<compound_rep> (l, array<tree> (5)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
}

tree::tree (tree_label l,
	    tree t1, tree t2, tree t3, tree t4, tree t5, tree t6):
  rep (tm_new<compound_rep> (l, array<tree> (6)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
}

tree::tree (tree_label l,
	    tree t1, tree t2, tree t3, tree t4, tree t5, tree t6, tree t7):
  rep (tm_new<compound_rep> (l, array<tree> (7)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
  (static_cast<compound_rep*> (rep))->a[6]=t7;
}

tree::tree (tree_label l,
	    tree t1, tree t2, tree t3, tree t4,
	    tree t5, tree t6, tree t7, tree t8):
  rep (tm_new<compound_rep> (l, array<tree> (8)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
  (static_cast<compound_rep*> (rep))->a[6]=t7;
  (static_cast<compound_rep*> (rep))->a[7]=t8;
}

tree
tree::operator () (int begin, int end) {
  int i;
  tree r (rep->op, end-begin);
  for (i=begin; i<end; i++)
    r[i-begin]= (static_cast<compound_rep*> (rep))->a[i];
  return r;
}

bool
operator == (tree t, tree u) {
  if (strong_equal (t, u)) return true;
  return (L(t)==L(u)) &&
    (L(t)==STRING? (t->label==u->label): (A(t)==A(u)));
}

bool
operator != (tree t, tree u) {
  if (strong_equal (t, u)) return false;
  return (L(t)!=L(u)) ||
    (L(t)==STRING? (t->label!=u->label): (A(t)!=A(u)));
}

tree
copy (tree t) {
  if (is_atomic (t)) return tree (copy (t->label));
  else {
    int i, n= N(t);
    tree t2 (t, n);
    for (i=0; i<n; i++) t2[i]= copy (t[i]);
    return t2;
  }
}

tree
freeze (tree t) {
  if (is_atomic (t)) return copy (t->label);
  if (is_func (t, UNFREEZE, 1)) return t[0];
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= freeze (t[i]);
    return r;
  }
}

tree
operator * (tree t1, tree t2) {
  int i;
  if (is_atomic (t1)) t1= tree (L(t2), t1);
  if (is_atomic (t2)) t2= tree (L(t1), t2);
  tree r (t1, N(t1)+N(t2));
  for (i=0; i<N(t1); i++) r[i]= t1[i];
  for (i=0; i<N(t2); i++) r[i+N(t1)]= t2[i];
  return r;
}

tree&
operator << (tree& t, tree t2) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << t2;
  return t;
}

tree&
operator << (tree& t, array<tree> a) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << a;
  return t;
}

tm_ostream&
operator << (tm_ostream& out, tree t) {
  if (is_atomic (t)) return out << t->label;
  else if (is_compound (t)) {
    int i, n= N(t);
    out << as_string (L(t));
    if (n==0) return out << "()";
    out << " (";
    for (i=0; i< n-1; i++)
      out << t[i] << ", ";
    out << t[i] << ")";
    return out;
  }
  else out << as_blackbox (t);
  return out;
}

void
print_tree (tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) cout << " ";
  if (is_atomic (t)) cout << t->label << "\n";
  else {
    cout << as_string (L(t)) << "\n";
    for (i=0; i<N(t); i++) print_tree (t[i], tab+2);
  }
}

int
hash (array<tree> a) {
  int i, h=0, n=N(a);
  for (i=0; i<n; i++) {
    h=(h<<7) + (h>>25);
    h=h + hash(a[i]);
  }
  return h;
}

int
hash (tree t) {
  if (is_atomic (t)) return hash (t->label);
  else return ((int) L(t)) ^ hash (A(t));
}

string
tree_as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else if (is_concat (t) || is_document (t)) {
    int i, n= N(t);
    string cumul;
    for (i=0; i<n; i++)
      cumul << tree_as_string (t[i]);
    return cumul;
  }
  else if (is_func (t, WITH))
    return tree_as_string (t[N(t)-1]);
  else if (is_compound (t, "nbsp", 0))
    return " ";
  return "";
}

tree
replace (tree t, tree w, tree b) {
  if (t == w) return b;
  else if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= replace (t[i], w, b);
    return r;
  }
}

/******************************************************************************
* Tree predicates
******************************************************************************/

bool
is_document (tree t) {
  return L(t) == DOCUMENT;
}

bool
is_concat (tree t) {
  return L(t) == CONCAT;
}

bool
is_format (tree t) {
  return is_document (t) || is_concat (t);
}

bool
is_formatting (tree t) {
  return (L(t)>=WITH_LIMITS) && (L(t)<=NEW_DPAGE);
}

bool
is_table (tree t) {
  return
    is_func (t, TABLE) || is_func (t, SUBTABLE) ||
    is_func (t, ROW) || is_func (t, CELL);
}

bool
is_table_format (tree t) {
  return is_func (t, TFORMAT);
}

bool
is_multi_paragraph (tree t) {
  switch (L(t)) {
  case DOCUMENT:
    return true;
  case SURROUND:
    return is_multi_paragraph (t[2]);
  case DATOMS:
  case DLINES:
  case DPAGES:
  case WITH:
  case MARK:
  case EXPAND_AS:
  case STYLE_WITH:
  case VAR_STYLE_WITH:
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return is_multi_paragraph (t[N(t)-1]);
  case VAR_INCLUDE:
    return true;
  case WITH_PACKAGE:
    return is_multi_paragraph (t[N(t)-1]);
  case LOCUS:
  case CANVAS:
    return is_multi_paragraph (t[N(t)-1]);
  default:
    {
      static hashset<tree_label> inline_set; // FIXME: use drd
      if (N(inline_set) == 0) {
	inline_set->insert (make_tree_label ("footnote"));
	inline_set->insert (make_tree_label ("footnote-anchor"));
	inline_set->insert (make_tree_label ("note-footnote"));
	inline_set->insert (make_tree_label ("note-footnote*"));
	inline_set->insert (make_tree_label ("script-input"));
	inline_set->insert (make_tree_label ("converter-input"));
      }
      if (L(t) < START_EXTENSIONS) return false;
      else if (inline_set->contains (L(t))) return false;
      else {
	int i, n= N(t);
	for (i=0; i<n; i++)
	  if (is_multi_paragraph (t[i]))
	    return true;
	return false;
      }
    }
  }
}

bool
is_around (tree t) {
  return is_func (t, AROUND, 3) || is_func (t, VAR_AROUND, 3);
}

bool
is_script (tree t) {
  return
    is_func (t, LSUB) || is_func (t, LSUP) ||
    is_func (t, RSUB) || is_func (t, RSUP);
}

bool
is_script (tree t, bool& right) {
  if (is_func (t, LSUB) ||
      is_func (t, LSUP)) { right=false; return true; }
  if (is_func (t, RSUB) ||
      is_func (t, RSUP)) { right=true; return true; }
  return false;
}

bool
is_prime (tree t) {
  return ((L(t) == LPRIME) || (L(t) == RPRIME)) && (N(t) == 1);
}

bool
is_left_script_prime (tree t) {
  return is_func (t, LSUB, 1) || is_func (t, LSUP, 1) ||
         is_func (t, LPRIME, 1);
}

bool
is_right_script_prime (tree t) {
  return is_func (t, RSUB, 1) || is_func (t, RSUP, 1) ||
         is_func (t, RPRIME, 1);
}

bool
is_mod_active (tree t) {
  return (N(t) == 1) && (L(t) >= STYLE_ONLY) && (L(t) <= VAR_INACTIVE);
}

bool
is_mod_active_once (tree t) {
  return (N(t) == 1) &&
    ((L(t) == STYLE_ONLY) || (L(t) == ACTIVE) || (L(t) == INACTIVE));
}

bool
is_graphical_text (tree t) {
  return
    is_func (t, TEXT_AT) ||
    is_func (t, MATH_AT) ||
    is_func (t, DOCUMENT_AT);
}

bool
is_empty (tree t) {
  if (is_atomic (t)) return (t == "");
  if (is_document (t) || is_concat (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (!is_empty (t[i])) return false;
    return is_concat (t) || (n<=1);
  }
  return is_compound (t, "suppressed");
}

bool
is_multi_line (tree t) {
  if (is_atomic (t)) return false;
  else if (is_func (t, DOCUMENT)) return true;
  else if (is_func (t, CONCAT) || is_func (t, TABLE)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_multi_line (t[i])) return true;
    return false;
  }
}

/******************************************************************************
* Compound trees
******************************************************************************/

tree
compound (string s) {
  return tree (make_tree_label (s));
}

tree
compound (string s, tree t1) {
  return tree (make_tree_label (s), t1);
}

tree
compound (string s, tree t1, tree t2) {
  return tree (make_tree_label (s), t1, t2);
}

tree
compound (string s, tree t1, tree t2, tree t3) {
  return tree (make_tree_label (s), t1, t2, t3);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4) {
  return tree (make_tree_label (s), t1, t2, t3, t4);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (make_tree_label (s), t1, t2, t3, t4, t5);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6) {
  return tree (make_tree_label (s), t1, t2, t3, t4, t5, t6);
}

tree
compound (string s, array<tree> a) {
  return tree (make_tree_label (s), a);
}

bool
is_extension(tree_label l) {
  return l >= START_EXTENSIONS;
}

bool
is_extension (tree t) {
  return L(t) >= START_EXTENSIONS;
}

bool
is_extension (tree t, int n) {
  return (L(t) >= START_EXTENSIONS) && (N(t) == n);
}

bool
is_compound (tree t, string s) {
  return as_string (L(t)) == s;
}

bool
is_compound (tree t, string s, int n) {
  return (as_string (L(t)) == s) && (N(t) == n);
}

/******************************************************************************
* Routines for simplification and correction
******************************************************************************/

static void
simplify_concat (tree& r, tree t) {
  if (is_atomic (t)) {
   r= concat (t);
   return;
  }
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_concat (t[i])) simplify_concat (r, t[i]);
    else if (t[i] == "");
    else if (is_atomic (t[i]) && (N(r)>0) && is_atomic (r[N(r)-1]))
      r[N(r)-1]= tree (r[N(r)-1]->label * t[i]->label);
    else r << t[i];
}

tree
simplify_concat (tree t) {
  if (is_atomic (t)) return t;
  tree r (CONCAT);
  simplify_concat (r, t);
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

static void
simplify_document (tree& r, tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_document (t[i])) simplify_document (r, t[i]);
    else r << t[i];
}

tree
simplify_document (tree t) {
  if (!is_document (t)) return t;
  tree r (DOCUMENT);
  simplify_document (r, t);
  return r;
}

tree
simplify_correct (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, QUOTE, 1) && (is_atomic (t[0]))) return t[0];
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= simplify_correct (t[i]);
  if (is_concat (r)) r= simplify_concat (r);
  if (is_document (r)) r= simplify_document (r);
  return r;
}
