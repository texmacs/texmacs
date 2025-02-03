
/******************************************************************************
* MODULE     : evaluate_rewrite.cpp
* DESCRIPTION: tree rewriting before evaluation
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "memorizer.hpp"
#include "std_environment.hpp"
#include "vars.hpp"
#include "scheme.hpp"

extern int script_status;

/******************************************************************************
* Memorizing rewritings
******************************************************************************/

static tree no_tree (UNINIT);

class memorizer;
class rewrite_memorizer_rep: public compound_memorizer_rep {
  environment env_in;
  tree t_in;
  environment env_out;
  tree t_out;

public:
  inline rewrite_memorizer_rep (environment env, tree t):
    env_in (env), t_in (t), env_out (), t_out (no_tree) {}
  void print (tm_ostream& out) {
    out << "rewrite_memorizer (" << t_in << ")"; }

  int type () { return MEMORIZE_REWRITE; }
  int hash () { return weak_hash (env_in) ^ weak_hash (t_in); }
  bool equal (memorizer_rep* mem) {
    rewrite_memorizer_rep* rep= (rewrite_memorizer_rep*) mem;
    return weak_equal (env_in, rep->env_in) && weak_equal (t_in, rep->t_in); }

  void set_environment (environment env) { env_out= env; }
  environment get_environment () { return env_out; }
  void set_tree (tree t) { t_out= t; }
  tree get_tree () { return t_out; }
};

inline memorizer
rewrite_memorizer (environment env, tree t) {
  return (memorizer_rep*) tm_new<rewrite_memorizer_rep> (env, t);
}

/******************************************************************************
* Reentrant evaluations
******************************************************************************/

// Hack to transmit the current environment back to C++
// across the Scheme level, and to maintain reentrancy.
static environment reenter_rewrite_env;

tree
texmacs_evaluate (environment env, tree t) {
  // re-entrancy
  if (!is_nil (reenter_rewrite_env)) env= reenter_rewrite_env;
  environment old_env= std_env;
  std_env= env;
  tree r= evaluate (t);
  std_env= old_env;
  return r;
}

/******************************************************************************
* Rewriting (scheme-like macro expansion)
******************************************************************************/

tree
rewrite_impl (tree t) {
  switch (L(t)) {
  case EXTERN:
    {
      int i, n= N(t);
      tree r (TUPLE, n);
      for (i=0; i<n; i++)
	r[i]= evaluate (t[i]);
      object expr= null_object ();
      for (i=n-1; i>0; i--)
	expr= cons (object (r[i]), expr);
      string fun= evaluate_string (t[0]);
      expr= cons (string_to_object (fun), expr);
      bool secure= as_bool (std_env ["secure"]);
      if (!secure && script_status < 2) {
	if (!as_bool (call ("secure?", expr)))
	  return tree (TMERROR, "insecure script");
      }
      environment old_env= reenter_rewrite_env;
      reenter_rewrite_env= std_env;
      object o= eval (expr);
      reenter_rewrite_env= old_env;
      return content_to_tree (o);
    }
#ifdef CLASSICAL_MACRO_EXPANSION
  case MAP_ARGS:
    {
      if (!(is_atomic (t[0]) && is_atomic (t[1]) && is_atomic (t[2])))
	return evaluate_error ("invalid map-args");
      if (macro_top_level (std_env))
	return evaluate_error ("undefined", t[2]);
      basic_environment local= macro_arguments (std_env);
      int key= make_tree_label (t[2]->label);
      if (!local->contains (key))
	return evaluate_error ("undefined", t[2]);
      tree v= local [key];
      if (is_atomic (v))
	return evaluate_error ("invalid-map-args");
      macro_up (std_env);

      int start= 0, end= N(v);
      if (N(t)>=4) start= as_int (evaluate (t[3]));
      if (N(t)>=5) end  = as_int (evaluate (t[4]));
      int i, n= max (0, end-start);
      tree r (make_tree_label (t[1]->label), n);
      for (i=0; i<n; i++)
	r[i]= tree (make_tree_label (t[0]->label),
		    tree (ARG, copy (t[2]), as_string (start+i)),
		    as_string (start+i));

      macro_redown (std_env, local);
      return r;
    }
#endif // CLASSICAL_MACRO_EXPANSION
  case VAR_INCLUDE:
    {
      url base_file_name (as_string (std_env ["base-file-name"]));
      url file_name= url_system (evaluate_string (t[0]));
      return load_inclusion (relative (base_file_name, file_name));
    }
  case WITH_PACKAGE:
    {
      string file_name= exec_string (t[0]);
      return with_package_definitions (file_name, t[1]);
    }
  case REWRITE_INACTIVE:
    {
#ifdef CLASSICAL_MACRO_EXPANSION      
      if ((!is_func (t[0], ARG)) || is_compound (t[0][0]))
	return evaluate_error ("invalid rewrite-inactive");
      if (macro_top_level (std_env))
	return evaluate_error ("undefined", t[0][0]);
      basic_environment local= macro_arguments (std_env);
      int key= make_tree_label (t[0][0]->label);
      if (!local->contains (key))
	return evaluate_error ("undefined", t[0][0]);
      tree val= local [key];
      int i, n= N(t[0]);
      for (i=1; i<n; i++) {
	int j= as_int (t[0][i]);
	if ((j>=0) && (j<N(val))) val= val[j];
	else return evaluate_error ("invalid rewrite-inactive");
      }
#else
      tree val= t[0];
#endif
      int inactive_mode= INACTIVE_INLINE_RECURSE;
      if (t[1] == "recurse") inactive_mode= INACTIVE_INLINE_RECURSE;
      else if (t[1] == "recurse*") inactive_mode= INACTIVE_BLOCK_RECURSE;
      else if (t[1] == "once") inactive_mode= INACTIVE_INLINE_ONCE;
      else if (t[1] == "once*") inactive_mode= INACTIVE_BLOCK_ONCE;
      else if (t[1] == "error") inactive_mode= INACTIVE_INLINE_ERROR;
      else if (t[1] == "error*") inactive_mode= INACTIVE_BLOCK_ERROR;
      return rewrite_inactive (val, inactive_mode);
    }
  default:
    return t;
  }
}

/******************************************************************************
* Main rewriting routines
******************************************************************************/

tree
rewrite (tree t) {
  cout << "Rewrite "
    //<< "[" << (t.operator -> ())
    //<< ", " << (std_env.operator -> ()) << "] "
       << t << INDENT << LF;
  memorizer mem= rewrite_memorizer (std_env, t);
  if (is_memorized (mem)) {
    cout << UNINDENT << "Memorized " << mem->get_tree () << LF;
    std_env= mem->get_environment ();
    return mem->get_tree ();
  }
  memorize_start ();
  tree r= rewrite_impl (t);
  decorate_ip (t, r);
  mem->set_tree (r);
  mem->set_environment (std_env);
  memorize_end ();
  cout << UNINDENT << "Rewritten as " << mem->get_tree () << LF;
  return mem->get_tree ();
}

tree
evaluate_rewrite (tree t) {
  return evaluate (rewrite (t));
}
