
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

array<tree> concat_decompose (tree t);
tree concat_recompose (array<tree> a);

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
* Correct superfluous WITHs or WITH-like macros
******************************************************************************/

tree
with_correct (tree t) {
  if (is_atomic (t)) return t;
  else {
    cout << "Correcting " << t << LF << INDENT;
    tree u (t, N(t));
    for (int k=0; k<N(t); k++)
      u[k]= with_correct (t[k]);
    cout << "u= " << u << LF;
    array<tree> a= concat_decompose (u);
    int i, n= N(a);
    array<tree> r;
    cout << "a= " << a << LF;
    for (i=0; i<n; i++) {
      if (is_with_like (a[i])) {
	cout << "a[" << i << "]= " << a[i] << LF;
	array<tree> b= with_decompose (a[i], with_body (a[i]));
	cout << "b= " << b << LF;
	int p= N(b), k1, k2;
	cout << "p= " << p << LF;
	for (k1=0; k1<p ; k1++)
	  if (is_with_like (b[k1]) && with_similar_type (a[i], b[k1]));
	  else break;
	for (k2=p; k2>k1; k2--)
	  if (is_with_like (b[k2-1]) && with_similar_type (a[i], b[k2-1]));
	  else break;
	cout << "k1= " << k1 << LF;
	cout << "k2= " << k2 << LF;
	array<tree> x;
	if (0  < k1) x << range (b, 0, k1);
	if (k1 < k2) x << with_recompose (a[i], range (b, k1, k2));
	if (k2 < p ) x << range (b, k2, p);
	cout << "x= " << x << LF;
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
    cout << "r= " << r << LF;
    cout << UNINDENT << "Corrected " << t << " -> "
	 << concat_recompose (r) << LF;
    return concat_recompose (r);
  }
}

// Remove top-level text and with-mode-text tags
// Replace with-mode-text by text inside math
// Trailing and double white-space
