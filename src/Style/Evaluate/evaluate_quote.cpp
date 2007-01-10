
/******************************************************************************
* MODULE     : evaluate_quote.cpp
* DESCRIPTION: quoting and evaluation primitives
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"

/******************************************************************************
* Tree rewriting before evaluation
******************************************************************************/

tree
evaluate_rewrite (tree t) {
  (void) t; return "";
  // FIXME: not yet implemented
}

/******************************************************************************
* The eval-args primitive
******************************************************************************/

tree
evaluate_eval_args (tree t) {
  if (macro_top_level (std_env) || !is_atomic (t[0]))
    return evaluate_error ("undefined", t[0]);
  basic_environment local= macro_arguments (std_env);
  int key= make_tree_label (t[0]->label);
  if (!local->contains (key))
    return evaluate_error ("undefined", t[0]);
  tree u= local [key];
  if (is_atomic (u)) return evaluate_error ("bad eval-args");

  macro_up (std_env);
  int i, n= N(u);
  tree r (u, n);
  for (i=0; i<n; i++)
    r[i]= evaluate (u[i]);
  macro_redown (std_env, local);

  return r;
}

/******************************************************************************
* Quasiquote
******************************************************************************/

tree
evaluate_quasiquote (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, UNQUOTE, 1)) return evaluate (t[0]);
  else {
    int i, n= N(t);
    tree r (L(t));
    for (i=0; i<n; i++) {
      if (is_func (t[i], VAR_UNQUOTE, 1)) {
	tree ins= evaluate (t[i]);
	if (is_compound (ins)) r << A(ins);
	else r << evaluate_error ("bad unquote*");
      }
      else r << evaluate_quasiquote (t[i]);
    }
    return r;
  }
}
