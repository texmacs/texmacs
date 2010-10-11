
/******************************************************************************
* MODULE     : tree_correct.cpp
* DESCRIPTION: make a tree syntactically match a drd
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_correct.hpp"
#include "tree_analyze.hpp"
#include "Scheme/object.hpp"

/******************************************************************************
* DRD based correction
******************************************************************************/

tree
drd_correct (drd_info drd, tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    if (drd->contains (as_string (L(t))) &&
	!drd->correct_arity (L(t), n))
      return "";
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= drd_correct (drd, t[i]);
    return r;
  }
}

/******************************************************************************
* Subroutines for WITH-like macros
******************************************************************************/

bool
is_with_like (tree w) {
  return
    is_func (w, WITH) ||
    is_compound (w, "math", 1) ||
    is_compound (w, "text", 1);
}

tree&
with_body (tree w) {
  return w[N(w)-1];
}

bool
with_same_type (tree w1, tree w2) {
  ASSERT (is_with_like (w1) && is_with_like (w2), "with-like trees expected");
  return w1 (0, N(w1)-1) == w2 (0, N(w2)-1);
}

bool
with_similar_type (tree w1, tree w2) {
  ASSERT (is_with_like (w1) && is_with_like (w2), "with-like trees expected");
  if (is_compound (w1, "math") || is_compound (w1, "text"))
    return is_compound (w2, "math") || is_compound (w2, "text");
  if (!is_func (w1, WITH) || !is_func (w2, WITH))
    return with_same_type (w1, w2);
  if (N(w1) != N(w2)) return false;
  for (int i=0; i<N(w1)-1; i+=2)
    if (w1[i] != w2[i]) return false;
  return true;
}

array<tree>
with_decompose (tree w, tree t) {
  array<tree> r;
  if (t == "");
  else if (is_atomic (t)) r << t;
  else if (is_concat (t))
    for (int i=0; i<N(t); i++)
      r << with_decompose (w, t[i]);
  else if (is_with_like (t) && with_same_type (w, t))
    r << with_decompose (w, with_body (t));
  else r << t;
  return r;
}

tree
with_recompose (tree w, array<tree> a) {
  tree r= w (0, N(w));
  with_body (r)= concat_recompose (a);
  return r;
}

/******************************************************************************
* Correct WITHs or WITH-like macros
******************************************************************************/

tree
with_correct_bis (tree t) {
  if (is_atomic (t)) return t;
  else {
    //cout << "Correcting " << t << LF << INDENT;
    tree u (t, N(t));
    for (int k=0; k<N(t); k++)
      u[k]= with_correct_bis (t[k]);
    array<tree> a= concat_decompose (u);
    int i, n= N(a);
    array<tree> r;
    for (i=0; i<n; i++) {
      if (is_with_like (a[i])) {
	array<tree> b= with_decompose (a[i], with_body (a[i]));
	int p= N(b), k1, k2;
	for (k1=0; k1<p ; k1++)
	  if (is_with_like (b[k1]) && with_similar_type (a[i], b[k1]));
	  else break;
	for (k2=p; k2>k1; k2--)
	  if (is_with_like (b[k2-1]) && with_similar_type (a[i], b[k2-1]));
	  else break;
	array<tree> x;
	if (0  < k1) x << range (b, 0, k1);
	if (k1 < k2) x << with_recompose (a[i], range (b, k1, k2));
	if (k2 < p ) x << range (b, k2, p);
	if (N(x) == 0) continue;
	if (N(r) != 0 &&
	    is_with_like (r[N(r)-1]) &&
	    with_same_type (r[N(r)-1], x[0]))
	  {
	    array<tree> c= concat_decompose (with_body (r[N(r)-1]));
	    c << concat_decompose (with_body (x[0]));
	    r[N(r)-1]= with_recompose (x[0], c);
	    r << range (x, 1, N(x));
	  }
	else r << x;
      }
      else r << a[i];
    }
    //cout << UNINDENT << "Corrected " << t << " -> "
    //<< concat_recompose (r) << LF;
    return concat_recompose (r);
  }
}

tree
with_correct (tree t) {
  if (call ("get-preference", "with correct") == object ("on"))
    return with_correct_bis (t);
  else return t;
}

tree
superfluous_with_correct (drd_info drd, tree t, tree env) {
  if (is_atomic (t)) return t;
  else {
    //cout << "Superfluous correcting " << t << ", " << env << LF;
    if (is_compound (t, "body", 1))
      return compound ("body", superfluous_with_correct (drd, t[0], env));
    tree r (t, N(t));
    for (int i=0; i<N(t); i++)
      r[i]= superfluous_with_correct
	      (drd, t[i], drd->get_env_child (t, i, env));
    if (is_compound (r, "math", 1) && drd_env_read (env, MODE) == "math")
      return r[0];
    else if (is_compound (r, "text", 1) && drd_env_read (env, MODE) == "text")
      return r[0];
    else if (is_func (r, WITH)) {
      for (int i=0; i+1<N(r); i+=2)
	if (!is_atomic (r[i])) return r;
	else if (drd_env_read (env, r[i]->label) != r[i+1]) return r;
      return r[N(r)-1];
    }
    else if (is_func (r, CONCAT)) {
      array<tree> a= concat_decompose (r);
      return concat_recompose (a);
    }
    return r;
  }
}

tree
superfluous_with_correct (tree t) {
  if (call ("get-preference", "with correct") == object ("on")) {
    drd_info drd= get_style_drd (tree (TUPLE, "generic"));
    return superfluous_with_correct (drd, t, tree (WITH, MODE, "text"));
  }
  else return t;
}

/******************************************************************************
* Remove incorrect spaces and multiplications
******************************************************************************/

array<tree>
superfluous_invisible_correct (array<tree> a) {
  array<int>  tp= symbol_types (a);
  array<tree> r;
  cout << a << ", " << tp << "\n";
  for (int i=0; i<N(a); i++)
    if (a[i] == " " || a[i] == "*") {
      int j1, j2;
      for (j1= i-1; j1>=0; j1--)
	if (tp[j1] != SYMBOL_SKIP && tp[j1] != SYMBOL_SCRIPT) break;
	else if (a[j1] == " ") break;
      for (j2= i+1; j2<N(a); j2++)
	if (tp[j2] != SYMBOL_SKIP && tp[j2] != SYMBOL_SCRIPT)
	  if (a[j2] != " " && a[j2] != "*") break;
      cout << "  " << i << ": " << j1 << ", " << j2
	   << "; " << tp[j1] << ", " << tp[j2] << "\n";
      if (j1 < 0 || j2 >= N(a));
      else if (a[j1] == " " || a[j1] == "*");
      else if (tp[j1] == SYMBOL_PREFIX ||
	       tp[j1] == SYMBOL_INFIX ||
	       tp[j1] == SYMBOL_SEPARATOR);
      else if (tp[j2] == SYMBOL_POSTFIX ||
	       tp[j2] == SYMBOL_INFIX ||
	       tp[j2] == SYMBOL_SEPARATOR);
      else r << a[i];
    }
    else r << a[i];
  return r;
}

tree
superfluous_invisible_correct (drd_info drd, tree t, string mode) {
  cout << "Correct " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      tree tmode= drd->get_env_child (t, i, MODE, mode);
      string smode= (is_atomic (tmode)? tmode->label: string ("text"));
      int type= drd->get_type_child (t, i);
      //cout << "  " << i << ": " << type << ", " << smode << "\n";
      if (is_compound (t, "body", 1)) type= TYPE_REGULAR;
      switch (type) {
      case TYPE_INVALID:
      case TYPE_REGULAR:
      case TYPE_GRAPHICAL:
      case TYPE_ANIMATION:
      case TYPE_UNKNOWN:
	r[i]= superfluous_invisible_correct (drd, t[i], smode);
	break;
      default:
	r[i]= t[i];
	break;
      }
    }
  }
  
  if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= superfluous_invisible_correct (a);
    return concat_recompose (a);
  }
  else return r;
}

tree
superfluous_invisible_correct (tree t, string mode) {
  return t;
  /*
  if (call ("get-preference", "invisible correct") == object ("on")) {
    drd_info drd= get_style_drd (tree (TUPLE, "generic"));
    return superfluous_invisible_correct (drd, t, mode);
  }
  else return t;
  */
}
