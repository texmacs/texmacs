
/******************************************************************************
* MODULE     : evaluate_main.cpp
* DESCRIPTION: evaluation of macro constructs
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "drd_mode.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Environment changes
******************************************************************************/

tree
evaluate_assign (tree t) {
  int i, n=N(t), k=n>>1;
  assoc_environment local (k);
  for (i=0; i<k; i++) {
    string var= as_string (evaluate (t[i<<1]));
    tree   val= evaluate (t[(i<<1)+1]);
    local->raw_write (i, var, val);
  }
  assign (std_env, local);
  return "";
}

tree
evaluate_with (tree t) {
  int i, n=N(t), k=(n-1)>>1;
  assoc_environment local (k);
  tree w (WITH);
  for (i=0; i<k; i++) {
    string var= as_string (evaluate (t[i<<1]));
    tree   val= evaluate (t[(i<<1)+1]);
    local->raw_write (i, var, val);
    w << var << val; // FIXME: don't add non-typesetter variables (?)
  }
  begin_with (std_env, local);
  tree r= evaluate (t[n-1]);
  end_with (std_env);
  w << r;
  return w;
}

tree
evaluate_provides (tree t) {
  tree r= evaluate (t[0]);
  if (is_compound (r)) return evaluate_error ("bad provides");
  if (std_env->contains (r->label)) return "true"; else return "false";  
}

tree
evaluate_value (tree t) {
  tree r= evaluate (t[0]);
  if (is_compound (r)) return evaluate_error ("bad value");
  int key= make_tree_label (r->label);
  if (!std_env->contains (key)) return evaluate_error ("undefined", r);
  return evaluate (std_env[key]);
}

tree
evaluate_quote_value (tree t) {
  tree r= evaluate (t[0]);
  if (is_compound (r)) return evaluate_error ("bad quoted value");
  int key= make_tree_label (r->label);
  if (!std_env->contains (key)) return evaluate_error ("undefined", r);
  return std_env[key];
}

/******************************************************************************
* DRD properties
******************************************************************************/

tree
evaluate_drd_props (tree t) {
  (void) t; return "";
  // FIXME: not yet implemented
}

/******************************************************************************
* Macro calls
******************************************************************************/

tree
evaluate_compound (tree t) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= evaluate (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!std_env->contains (var)) return evaluate_error ("undefined", var);
      f= std_env [var];
    }
  }
  else {
    string var= as_string (L(t));
    if (!std_env->contains (var)) return evaluate_error ("undefined", var);
    d= 0;
    f= std_env [var];
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    assoc_environment local (L(f)==XMACRO? 1: n);
    if (L(f) == XMACRO)
      local->raw_write (0, as_string (f[0]), t);
    else {
      static tree uninit (UNINIT);
      for (i=0; i<n; i++)
	local->raw_write (i, as_string (f[i]), i<m? t[i+d]: uninit);
      //local->print ("");
    }
    macro_down (std_env, local);
    tree r= evaluate (f[n]);
    macro_up (std_env);
    return r;
  }
  else return evaluate (f);
}

tree
evaluate_arg (tree t) {
  tree r= t[0];
  if (is_compound (r)) return evaluate_error ("bad arg");
  int key= make_tree_label (r->label);
  if (macro_top_level (std_env)) return evaluate_error ("undefined", r);
  basic_environment local= macro_arguments (std_env);
  //local->print ("");
  if (!local->contains (key)) return evaluate_error ("undefined", r);
  macro_up (std_env);
  r= local[key];
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= evaluate (t[i]);
      if (!is_int (u)) break;
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
      r= r[nr];
    }
  }
  r= evaluate (r);
  macro_redown (std_env, local);
  return r;
}

tree
evaluate_quote_arg (tree t) {
  tree r= t[0];
  if (is_compound (r)) return evaluate_error ("bad quoted arg");
  int key= make_tree_label (r->label);
  if (macro_top_level (std_env)) return evaluate_error ("undefined", r);
  basic_environment local= macro_arguments (std_env);
  if (!local->contains (key)) return evaluate_error ("undefined", r);
  r= local[key];
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= evaluate (t[i]);
      if (!is_int (u)) break;
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
      r= r[nr];
    }
  }
  return r;
}

/******************************************************************************
* Argument expansion
******************************************************************************/

#define is_accessible(p) ((nil (p)) || ((p)->item >= 0))

tree
expand (tree t, bool search_accessible) {
  if (is_atomic (t) || macro_top_level (std_env)) return t;
  else if (is_func (t, ARG) || is_func (t, QUOTE_ARG)) {
    tree r= t[0];
    if (is_compound (r)) return evaluate_error ("bad arg");
    int key= make_tree_label (r->label);
    basic_environment local= macro_arguments (std_env);
    if (!local->contains (key)) return evaluate_error ("undefined", r);
    macro_up (std_env);
    r= local[key];
    if (N(t) > 1) {
      int i, n= N(t);
      for (i=1; i<n; i++) {
	tree u= evaluate (t[i]);
	if (!is_int (u)) break;
	int nr= as_int (u);
	if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
	r= r[nr];
      }
    }
    if (is_func (t, ARG))
      r= expand (r, search_accessible);
    macro_redown (std_env, local);
    return r;
  }
  else if (is_func (t, EXPAND_AS, 2))
    return expand (t[0], search_accessible);
  else if (search_accessible && is_accessible (obtain_ip (t)))
    return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= expand (t[i], search_accessible);
      if (search_accessible &&
	  is_accessible (obtain_ip (r[i])) &&
	  the_drd->is_accessible_child (t, i)
	  // FIXME: should be drd->is_accessible_child (t, i)
	  )
	return r[i];
    }
    if (search_accessible) return t;
    return r;
  }
}

/******************************************************************************
* Syntactic decomposition
******************************************************************************/

tree
evaluate_get_label (tree t) {
  tree r= evaluate (t[0]);
  return copy (as_string (L(r)));  
}

tree
evaluate_get_arity (tree t) {
  tree r= evaluate (t[0]);
  return as_string (arity (r));
}
