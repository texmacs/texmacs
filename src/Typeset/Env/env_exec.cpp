
/******************************************************************************
* MODULE     : env_exec.cpp
* DESCRIPTION: evaluation of trees w.r.t. the environment
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "env.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "PsDevice/page_type.hpp"

extern int script_status;

/******************************************************************************
* Subroutines
******************************************************************************/

string
edit_env_rep::exec_string (tree t) {
  tree r= exec (t);
  if (is_atomic (r)) return r->label;
  else return "";
}

/******************************************************************************
* Rewriting (scheme-like macro expansion)
******************************************************************************/

tree
edit_env_rep::rewrite (tree t) {
  if (L(t) == EXTERN) {
    int i, n= N(t);
    string s= "(" * as_string (exec (t[0]));
    for (i=1; i<n; i++)
      s << " '" << tree_to_scheme (exec (t[i]));
    s << ")";
    if (script_status < 2) {
      if (!as_bool (eval ("(secure? '" * s * ")")))
	return tree (ERROR, "insecure script");
    }
    return object_to_tree (eval (s));
  }
  else if (L(t) == MAP_ARGS) {
    if (!(is_atomic (t[0]) && is_atomic (t[1]) && is_atomic (t[2])))
      return tree (ERROR, "invalid map arguments");
    if (nil (macro_arg) || (!macro_arg->item->contains (t[2]->label)))
      return tree (ERROR, "map arguments " * t[2]->label);
    tree v= macro_arg->item [t[2]->label];
    if (is_atomic (v))
      return tree (ERROR, "map arguments " * t[2]->label);
    list<hashmap<string,tree> > old_var= macro_arg;
    list<hashmap<string,path> > old_src= macro_src;
    if (!nil (macro_arg)) macro_arg= macro_arg->next;
    if (!nil (macro_src)) macro_src= macro_src->next;

    int start= 0, end= N(v);
    if (N(t)>=4) start= as_int (exec (t[3]));
    if (N(t)>=5) end  = as_int (exec (t[4]));
    int i, n= max (0, end-start);
    tree r (make_tree_label (t[1]->label), n);
    for (i=0; i<n; i++)
      r[i]= tree (make_tree_label (t[0]->label),
		  tree (ARGUMENT, copy (t[2]), as_string (start+i)));

    macro_arg= old_var;
    macro_src= old_src;
    return r;
  }
  else if (L(t) == INCLUDE) {
    url file_name= as_string (t[0]);
    return load_inclusion (relative (base_file_name, file_name));
  }
  return t;
}

tree
edit_env_rep::exec_rewrite (tree t) {
  return exec (rewrite (t));
}

/******************************************************************************
* Evaluation of trees
******************************************************************************/

bool see= false;

tree
edit_env_rep::exec (tree t) {
  // cout << "Execute: " << t << "\n";
  if (is_atomic (t)) return t;
  if (preamble && ((!is_func (t, ASSIGN)) || (t[0] != MODE))) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) r[i]= exec (t[i]);
    return r;
  }

  switch (L(t)) {
  case DECORATE_ATOMS:
    return exec_formatting (t, ATOM_DECORATIONS);
  case DECORATE_LINES:
    return exec_formatting (t, LINE_DECORATIONS);
  case DECORATE_PAGES:
    return exec_formatting (t, PAGE_DECORATIONS);
  case TABLE_FORMAT:
    return exec_formatting (t, CELL_FORMAT);
  case TABLE:
    return exec_table (t);
  case ASSIGN:
    return exec_assign (t);
  case WITH:
    return exec_with (t);
  case PROVIDES:
    return exec_provides (t);
  case VALUE:
    return exec_value (t);
  case MACRO:
    return copy (t);
  case DRD_PROPS:
    return exec_drd_props (t);
  case ARGUMENT:
    return exec_argument (t);
  case COMPOUND:
    return exec_compound (t);
  case XMACRO:
    return copy (t);
  case GET_LABEL:
    return exec_get_label (t);
  case GET_ARITY:
    return exec_get_arity (t);
  case MAP_ARGS:
    return exec_rewrite (t);
  case EVAL_ARGS:
    return exec_eval_args (t);
  case EVAL:
    return exec (exec (t[0]));
  case QUOTE:
    return copy (t[0]);
  case DELAY:
    return exec_delay (t);
  case HOLD:
    return exec_quasiquoted (t[0]);
  case RELEASE:
    return exec (t[0]);
  case EXTERN:
    return exec_rewrite (t);
  case INCLUDE:
    return exec_rewrite (t);

  case OR:
    return exec_or (t);
  case XOR:
    return exec_xor (t);
  case AND:
    return exec_and (t);
  case NOT:
    return exec_not (t);
  case PLUS:
    return exec_plus (t);
  case MINUS:
    return exec_minus (t);
  case TIMES:
    return exec_times (t);
  case OVER:
    return exec_over (t);
  case DIVIDE:
    return exec_divide (t);
  case MODULO:
    return exec_modulo (t);
  case MERGE:
    return exec_merge (t);
  case LENGTH:
    return exec_length (t);
  case RANGE:
    return exec_range (t);
  case NUMBER:
    return exec_number (t);
  case _DATE:
    return exec_date (t);
  case TRANSLATE:
    return exec_translate (t);
  case FIND_FILE:
    return exec_find_file (t);
  case IS_TUPLE:
    return exec_is_tuple (t);
  case LOOK_UP:
    return exec_lookup (t);
  case EQUAL:
    return exec_equal (t);
  case UNEQUAL:
    return exec_unequal (t);
  case LESS:
    return exec_less (t);
  case LESSEQ:
    return exec_lesseq (t);
  case GREATER:
    return exec_greater (t);
  case GREATEREQ:
    return exec_greatereq (t);
  case IF:
  case VAR_IF:
    return exec_if (t);
  case CASE:
    return exec_case (t);
  case WHILE:
    return exec_while (t);

  case INACTIVE:
    return exec_mod_active (t, INACTIVE);
  case ACTIVE:
    return exec_mod_active (t, ACTIVE);
  case VAR_INACTIVE:
    return exec_mod_active (t, VAR_INACTIVE);
  case VAR_ACTIVE:
    return exec_mod_active (t, VAR_ACTIVE);

  case _POINT:
    return exec_point (t);

  default:
    if (L(t) < START_EXTENSIONS) {
      int i, n= N(t);
      // cout << "Executing " << t << "\n";
      tree r (t, n);
      for (i=0; i<n; i++) r[i]= exec (t[i]);
      // cout << "Executed " << t << " -> " << r << "\n";
      return r;
    }
    else return exec_compound (t);
  }
}

tree
edit_env_rep::exec_formatting (tree t, string v) {
  int n= N(t);
  tree oldv= read (v);
  tree newv= join (oldv, t (0, n-1));
  // monitored_write_update (v, newv);
  write_update (v, newv);
  tree r= exec (t[n-1]);
  write_update (v, oldv);
  return join (t (0, n-1), tree (TABLE_FORMAT, r));
}

tree
edit_env_rep::exec_table (tree t) {
  tree oldv= read (CELL_FORMAT);
  // should execute values in oldv
  // monitored_write_update (CELL_FORMAT, tree (TABLE_FORMAT));
  write_update (CELL_FORMAT, tree (TABLE_FORMAT));
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= exec (t[i]);
  write_update (CELL_FORMAT, oldv);
  return r;
}

tree
edit_env_rep::exec_assign (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad assignment");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad assignment");
  assign (r->label, t[1]);
  return tree (ASSIGN, r, tree (QUOTE, read (r->label)));
}

tree
edit_env_rep::exec_with (tree t) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) return tree (ERROR, "bad with");
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(oldv,tree,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      oldv[i]= read (var);
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return tree (ERROR, "bad with");
    }
  }

  // for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  for (i=0; i<k; i++) write_update (vars[i], newv[i]);
  tree r= exec (t[n-1]);
  for (i=k-1; i>=0; i--) write_update (vars[i], oldv[i]);

  tree u (WITH, n);
  for (i=0; i<k; i++) {
    u[i<<1]    = vars[i];
    u[(i<<1)+1]= tree (QUOTE, newv[i]);
  }
  u[n-1]= r;
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
  return u;
}

tree
edit_env_rep::exec_compound (tree t) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!provides (var)) return tree (ERROR, "compound " * var);
      f= read (var);
    }
  }
  else {
    string var= as_string (L(t));
    if (!provides (var)) return tree (ERROR, "compound " * var);
    d= 0;
    f= read (var);
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0]))
	macro_arg->item (f[0]->label)= t;
    }
    else for (i=0; i<n; i++)
      if (is_atomic (f[i]))
	macro_arg->item (f[i]->label)= i<m? t[i+d]: tree("");
    tree r= exec (f[n]);
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
    return r;
  }
  else return exec (f);
}

tree
edit_env_rep::exec_extra_list (tree t, int pos) {
  if (pos == N(t)) return "";
  else {
    tree u= exec (t[pos]);
    tree v= exec_extra_list (t, pos+1);
    return tuple (u, v);
  }
}

tree
edit_env_rep::exec_extra_tuple (tree t, int pos) {
  int i, n= N(t);
  tree u (TUPLE, n-pos);
  for (i=pos; i<n; i++)
    u[i-pos]= exec (t[i]);
  return u;
}

tree
edit_env_rep::exec_drd_props (tree t) {
  int i, n= N(t);
  if ((n>=3) && is_atomic (t[0]))
    for (i=1; i<n-1; i+=2) {
      if (!is_atomic (t[i])) continue;
      string var  = t[0]->label;
      string prop = t[i]->label;
      tree   val  = t[i+1];
      tree_label l= make_tree_label (var);
      if (prop == "arity") {
	drd->set_arity (l, as_int (val), 0, ARITY_NORMAL, CHILD_DETAILED);
	drd->freeze_arity (l);
      }
      if (prop == "accessible") {
	if (val == "none") {
	  int i, n= drd->get_nr_indices (l);
	  for (i=0; i<n; i++) {
	    drd->set_accessible (l, i, false);
	    drd->freeze_accessible (l, i);
	  }
	}
	if (val == "all") {
	  int i, n= drd->get_nr_indices (l);
	  for (i=0; i<n; i++) {
	    drd->set_accessible (l, i, true);
	    drd->freeze_accessible (l, i);
	  }
	}
      }
    }
  return t;
}

tree
edit_env_rep::exec_provides (tree t) {
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad provides");
  if (provides (r->label)) return "true"; else return "false";
}

tree
edit_env_rep::exec_value (tree t) {
  tree r= t[0];
  if (is_compound (r)) return tree (ERROR, "bad value");
  return exec (read (r->label));
}

tree
edit_env_rep::exec_argument (tree t) {
  tree r= t[0];
  if (is_compound (r))
    return tree (ERROR, "bad argument application");
  if (nil (macro_arg) || (!macro_arg->item->contains (r->label)))
    return tree (ERROR, "argument " * r->label);
  r= macro_arg->item [r->label];
  list<hashmap<string,tree> > old_var= macro_arg;
  list<hashmap<string,path> > old_src= macro_src;
  if (!nil (macro_arg)) macro_arg= macro_arg->next;
  if (!nil (macro_src)) macro_src= macro_src->next;
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= exec (t[i]);
      if (!is_int (u)) break;
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
      r= r[nr];
    }
  }
  r= exec (r);
  macro_arg= old_var;
  macro_src= old_src;
  return r;
}

tree
edit_env_rep::exec_get_label (tree t) {
  tree r;
  if (is_func (t[0], ARGUMENT, 1)) {
    if (nil (macro_arg))
      return tree (ERROR, "Bad get_label argument " * as_string (t[0][0]));
    r= macro_arg->item [as_string (t[0][0])];
  }
  else r= exec (t[0]);
  return copy (as_string (L(r)));
}

tree
edit_env_rep::exec_get_arity (tree t) {
  tree r;
  if (is_func (t[0], ARGUMENT, 1)) {
    if (nil (macro_arg))
      return tree (ERROR, "Bad get_label argument " * as_string (t[0][0]));
    r= macro_arg->item [as_string (t[0][0])];
  }
  else r= exec (t[0]);
  return as_string (arity (r));
}

tree
edit_env_rep::exec_eval_args (tree t) {
  tree v= macro_arg->item [as_string (t[0])];
  if (is_atomic (v) || nil (macro_arg))
    return tree (ERROR, "eval arguments " * t[0]->label);
  list<hashmap<string,tree> > old_var= macro_arg;
  list<hashmap<string,path> > old_src= macro_src;
  if (!nil (macro_arg)) macro_arg= macro_arg->next;
  if (!nil (macro_src)) macro_src= macro_src->next;

  int i, n= N(v);
  tree r (v, n);
  for (i=0; i<n; i++)
    r[i]= exec (v[i]);

  macro_arg= old_var;
  macro_src= old_src;
  return r;
}

tree
edit_env_rep::exec_delay (tree t) {
  int i, n= N(t[0]);
  tree u (t[0], n);
  for (i=0; i<n; i++)
    u[i]= exec (t[0][i]);
  return u;
}

tree
edit_env_rep::exec_quasiquoted (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, RELEASE, 1)) return exec (t[0]);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) r[i]= exec_quasiquoted (t[i]);
    return r;
  }
}


tree
edit_env_rep::exec_or (tree t) {
  if (N(t)<2) return tree (ERROR, "bad or");
  for (int i=0; i<N(t); i++) {
    tree ti= exec (t[i]);
    if (is_compound (ti)) return tree (ERROR, "bad or");
    if (! is_bool (ti->label)) return tree (ERROR, "bad or");
    if (as_bool (ti->label)) return as_string_bool (true);
  }
  return as_string_bool (false);
}

tree
edit_env_rep::exec_xor (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad xor");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2)) return tree (ERROR, "bad xor");
  if (!is_bool (t1->label) || !is_bool (t2->label))
    return tree (ERROR, "bad xor");
  return as_string_bool (as_bool (t1->label) ^ as_bool (t2->label));
}

tree
edit_env_rep::exec_and (tree t) {
  if (N(t)<2) return tree (ERROR, "bad and");
  for (int i=0; i<N(t); i++) {
    tree ti= exec (t[i]);
    if (is_compound (ti)) return tree (ERROR, "bad and");
    if (! is_bool (ti->label)) return tree (ERROR, "bad and");
    if (! as_bool (ti->label)) return as_string_bool (false);
  }
  return as_string_bool (true);
}

tree
edit_env_rep::exec_not (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad not");
  tree tt= exec(t[0]);
  if (is_compound (tt)) return tree (ERROR, "bad not");
  if (! is_bool (tt->label)) return tree (ERROR, "bad not");
  return as_string_bool (! as_bool (tt->label));
}

tree
edit_env_rep::exec_plus (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad plus");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad plus");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string (as_double (s1)+ as_double (s2));
  if (is_length (s1) && is_length (s2))
    return add_lengths (s1, s2);
  return tree (ERROR, "bad plus");
}

tree
edit_env_rep::exec_minus (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad minus");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad minus");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string (as_double (s1)- as_double (s2));
  if (is_length (s1) && is_length (s2))
    return add_lengths (s1, "-" * s2);
  return tree (ERROR, "bad minus");
}

tree
edit_env_rep::exec_times (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad times");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad times");
  string s1 = t1->label;
  string s2 = t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string (as_double (s1) * as_double (s2));
  if (is_double (s1) && is_length (s2))
    return multiply_length (as_double (s1), s2);
  if (is_length (s1) && is_double (s2))
    return multiply_length (as_double (s2), s1);
  return tree (ERROR, "bad times");
}

tree
edit_env_rep::exec_over (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad over");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad over");
  string s1 = t1->label;
  string s2 = t2->label;
  if (is_double (s1) && is_double (s2)) {
    double den= as_double (s2);
    if (den == 0) return tree (ERROR, "division by zero");
    return as_string (as_double (s1) / den);
  }
  if (is_length (s1) && is_double (s2)) {
    double den= as_double (s2);
    if (den == 0) return tree (ERROR, "division by zero");
    return multiply_length (1/den, s1);
  }
  if (is_length (s1) && is_length (s2)) {
    if (decode_length (s2) == 0) return tree (ERROR, "division by zero");
    return as_string (divide_lengths(s1, s2));
  }
  return tree (ERROR, "bad over");
}

tree
edit_env_rep::exec_divide (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad divide");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad divide");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return tree (ERROR, "division by zero");
    return as_string (as_int (t1->label) / den);
  }
  return tree (ERROR, "bad divide");
}

tree
edit_env_rep::exec_modulo (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad modulo");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad modulo");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return tree (ERROR, "modulo zero");
    return as_string (as_int (t1->label) % den);
  }
  return tree (ERROR, "bad modulo");
}

tree
edit_env_rep::exec_merge (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad merge");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2)) {
    if (is_tuple (t1) && is_tuple (t2)) return join (t1, t2);
    if (is_func (t1, MACRO) && is_func (t2, MACRO) &&
	(N(t1) == N(t2)) && (t1 (0, N(t1)-1) == t2 (0, N(t2)-1)))
      {
	tree r = copy (t1);
	tree u1= copy (t1[N(t1)-1]);
	tree u2= copy (t2[N(t2)-1]);
	tree u (CONCAT, u1, u2);
	if (u1 == "") u= u2;
	else if (u2 == "") u= u1;
	else if (is_atomic (u1) && is_atomic (u2))
	  u= u1->label * u2->label;
	r[N(r)-1]= u;
	return r;
      }
    return tree (ERROR, "bad merge");
  }
  return t1->label * t2->label;
}

tree
edit_env_rep::exec_length (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad length");
  tree t1= exec (t[0]);
  if (is_compound (t1)) {
    if (is_tuple (t1)) return as_string (N (t1));
    return tree (ERROR, "bad length");
  }
  return as_string (N (t1->label));
}

tree
edit_env_rep::exec_range (tree t) {
  if (N(t)!=3) return tree (ERROR, "bad range");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  tree t3= exec (t[2]);
  if (!(is_int (t2) && is_int (t3))) return tree (ERROR, "bad range");
  if (is_compound (t1)) {
    if (is_tuple (t1)) {
      int i1= max (0, as_int (t2));
      int i2= min (N (t1), as_int (t3));
      i2 = max (i1, i2);
      return t1 (i1, i2);
    }
    return tree (ERROR, "bad range");
  }
  int i1= max (0, as_int (t2));
  int i2= min (N(t1->label), as_int (t3));
  i2 = max (i1, i2);
  return t1->label (i1, i2);
}

tree
edit_env_rep::exec_number (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad number");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad number");
  string s1= t1->label;
  string s2= t2->label;
  int nr= as_int (s1);
  if (s2 == "roman") return roman_nr (nr);
  if (s2 == "Roman") return Roman_nr (nr);
  if (s2 == "alpha") return alpha_nr (nr);
  if (s2 == "Alpha") return Alpha_nr (nr);
  return tree (ERROR, "bad number");
}

tree
edit_env_rep::exec_date (tree t) {
  if (N(t)>2) return tree (ERROR, "bad date");
  string lan= get_string (TEXT_LANGUAGE);
  if (N(t) == 2) {
    tree u= exec (t[1]);
    if (is_compound (u)) return tree (ERROR, "bad date");
    lan= u->label;
  }
  string fm;
  if (N(t) != 0) {
    tree u= exec (t[0]);
    if (is_compound (u)) return tree (ERROR, "bad date");
    fm= u->label;
  }
  return get_date (lan, fm);
}

tree
edit_env_rep::exec_translate (tree t) {
  if (N(t)!=3) return tree (ERROR, "bad translate");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  tree t3= exec (t[2]);
  if (is_compound (t1) || is_compound (t2) || is_compound (t3))
    return tree (ERROR, "bad translate");
  return dis->translate (t1->label, t2->label, t3->label);
}

tree
edit_env_rep::exec_find_file (tree t) {
  int i, n=N(t);
  array<tree> r (n);
  for (i=0; i<n; i++) {
    r[i]= exec (t[i]);
    if (is_compound (r[i]))
      return tree (ERROR, "bad find file");
  }
  for (i=0; i<(n-1); i++) {
    url u= resolve (url (r[i]->label, r[n-1]->label));
    if (!is_none (u)) {
      if (is_rooted (u, "default")) u= reroot (u, "file");
      return as_string (u);
    }
  }
  url u= resolve (base_file_name * url_parent () * r[n-1]->label);
  if (!is_none (u)) {
    if (is_rooted (u, "default")) u= reroot (u, "file");
    return as_string (u);
  }
  return "false";
}

tree
edit_env_rep::exec_is_tuple (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad tuple query");
  return as_string_bool(is_tuple (exec (t[0])));
}

tree
edit_env_rep::exec_lookup (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad look up");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (!(is_tuple (t1) && is_int (t2))) return tree (ERROR, "bad look up");
  int i= max (0, min (N(t1)-1, as_int (t2)));
  return t1[i];
}

tree
edit_env_rep::exec_equal (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_atomic (t1) && is_atomic (t2)
      && is_length (t1->label) && is_length (t2->label))
    return as_string_bool
      (decode_length (t1->label) == decode_length (t2->label));
  return as_string_bool (t1 == t2);
}

tree
edit_env_rep::exec_unequal (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad unequal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_atomic(t1) && is_atomic(t2)
      && is_length(t1->label) && is_length(t2->label))
    return as_string_bool
      (decode_length(t1->label) != decode_length(t2->label));
  return as_string_bool (t1 != t2);
}

tree
edit_env_rep::exec_less (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad less");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad less");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string_bool (as_double (s1) < as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (decode_length (s1) < decode_length (s2));
  return tree (ERROR, "bad less");
}

tree
edit_env_rep::exec_lesseq (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad less or equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad less or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) <= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (decode_length (s1) <= decode_length (s2));
  return tree (ERROR, "bad less or equal");
}

tree
edit_env_rep::exec_greater (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad greater");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad greater");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) > as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (decode_length (s1) > decode_length (s2));
  return tree (ERROR, "bad greater");
}

tree
edit_env_rep::exec_greatereq (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad greater or equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad greater or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) >= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (decode_length (s1) >= decode_length (s2));
  return tree (ERROR, "bad greater or equal");
}

tree
edit_env_rep::exec_if (tree t) {
  // This case must be kept consistent with
  // concater_rep::typeset_if(tree, path)
  // in ../Concat/concat_active.cpp
  if ((N(t)!=2) && (N(t)!=3)) return tree (ERROR, "bad if");
  tree tt= exec (t[0]);
  if (is_compound (tt) || !is_bool (tt->label))
    return tree (ERROR, "bad if");
  if (as_bool (tt->label)) return exec (t[1]);
  if (N(t)==3) return exec (t[2]);
  return "";
}

tree
edit_env_rep::exec_case (tree t) {
  // This case must be kept consistent with
  // concater_rep::typeset_case(tree, path)
  // in ../Concat/concat_active.cpp
  if (N(t)<2) return tree (ERROR, "bad case");
  int i, n= N(t);
  for (i=0; i<(n-1); i+=2) {
    tree tt= exec (t[i]);
    if (is_compound (tt) || ! is_bool (tt->label))
      return tree (ERROR, "bad case");
    if (as_bool (tt->label)) return exec (t[i+1]);
  }
  if (i<n) return exec (t[i]);
  return "";
}

tree
edit_env_rep::exec_while (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad while");
  tree r (CONCAT);
  while (1) {
    tree tt= exec (t[0]);
    if (is_compound (tt)) return tree (ERROR, "bad while");
    if (! is_bool (tt->label)) return tree (ERROR, "bad while");
    if (! as_bool(tt->label)) break;
    r << exec (t[1]);
  }
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
edit_env_rep::exec_mod_active (tree t, tree_label which) {
  tree u= t[0];
  if (is_atomic (u)) return u;
  int i, n= N(u);
  tree r (u, n);
  for (i=0; i<n; i++) r[i]= exec (u[i]);
  return tree (which, u);
}

tree
edit_env_rep::exec_point (tree t) {
  int i, n= N(t);
  tree u (TUPLE, N(t));
  for (i=0; i<n; i++)
    u[i]= exec (t[i]);
  return u;
}

/******************************************************************************
* Partial evaluation of trees
******************************************************************************/

void
edit_env_rep::exec_until (tree t, path p) {
  // cout << "Execute " << t << " until " << p << "\n";
  if (nil (p)) return;
  if (atom (p)) {
    if (p->item!=0)
      (void) exec (t);
    return;
  }

  switch (L(t)) {
  case DECORATE_ATOMS:
    exec_until_formatting (t, p, ATOM_DECORATIONS);
    return;
  case DECORATE_LINES:
    exec_until_formatting (t, p, LINE_DECORATIONS);
    return;
  case DECORATE_PAGES:
    exec_until_formatting (t, p, PAGE_DECORATIONS);
    return;
  case TABLE_FORMAT:
    exec_until_formatting (t, p, CELL_FORMAT);
    return;
  case TABLE:
    exec_until_table (t, p);
    return;
  case WITH:
    exec_until_with (t, p);
    return;
  case COMPOUND:
    exec_until_compound (t, p);
    return;
  case INACTIVE:
  case ACTIVE:
  case VAR_INACTIVE:
  case VAR_ACTIVE:
    exec_until_mod_active (t, p);
    return;
  default:
    if (L(t) < START_EXTENSIONS) {
      int i;
      for (i=0; i<p->item; i++) (void) exec (t[i]);
      exec_until (t[p->item], p->next);
      return;
    }
    else return exec_until_compound (t, p);
  }
}

void
edit_env_rep::exec_until_formatting (tree t, path p, string v) {
  int n= N(t);
  if (p->item != n-1) return;
  tree oldv= read (v);
  tree newv= join (oldv, t (0, n-1));
  monitored_write_update (v, newv);
  exec_until (t[n-1], p->next);
}

void
edit_env_rep::exec_until_table (tree t, path p) {
  // should execute values in oldv
  monitored_write_update (CELL_FORMAT, tree (TABLE_FORMAT));
  int i;
  for (i=0; i<p->item; i++)
    (void) exec (t[i]);
  exec_until (t[p->item], p->next);
  return;
}

void
edit_env_rep::exec_until_with (tree t, path p) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if (((n&1) != 1) || (p->item != n-1)) return;
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(newv);
      return;
    }
  }
  for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  exec_until (t[n-1], p->next);
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(newv);
  return;
}

void
edit_env_rep::exec_until_compound (tree t, path p) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_compound (f)) return;
    string fname= f->label;
    if (!provides (fname)) return;
    f= read (fname);
  }
  else {
    string fname= as_string (L(t));
    if (!provides (fname)) return;
    d= 0;
    f= read (fname);
  }

  if ((p->item < d) || (p->item >= N(f)) ||
      is_compound (f[p->item-d])) return;
  string var= f[p->item-d]->label;

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> >
      (hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> >
      (hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0]))
	macro_arg->item (f[0]->label)= t;
    }
    else for (i=0; i<n; i++)
      if (is_atomic (f[i]))
	macro_arg->item (f[i]->label)= i<m? t[i+d]: tree("");
    (void) exec_until (f[n], p->next, var, 0);
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
  }
}

void
edit_env_rep::exec_until_mod_active (tree t, path p) {
  {
    if (p->item != 0) return;
    t= t[0];
    p= p->next;
    if (atom (p)) {
      if (p->item!=0)
	(void) exec (t);
    }
    else {
      int i;
      for (i=0; i<p->item; i++)
	(void) exec (t[i]);
      exec_until (t[p->item], p->next);
    }
    return;
  }
  /*
  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-1; // is n=0 allowed ?
    tree old_value  [n];
    for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var   = f[i]->label;
	old_value [i]= read (var);
	monitored_write (var, i<m? t[i+1]: tree(""));
      }
    (void) exec_until (f[n], var, p->next);
    for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var= f[i]->label;
	write (var, old_value[i]);
      }
  }
  */
}

bool
edit_env_rep::exec_until (tree t, path p, string var, int level) {
  /*
  cout << "Execute " << t << " (" << var << ", "
       << level << ") until " << p << "\n";
  */

  if (is_atomic (t) || preamble) return false;
  switch (L(t)) {
  case DECORATE_ATOMS:
    return exec_until_formatting (t, p, var, level, ATOM_DECORATIONS);
  case DECORATE_LINES:
    return exec_until_formatting (t, p, var, level, LINE_DECORATIONS);
  case DECORATE_PAGES:
    return exec_until_formatting (t, p, var, level, PAGE_DECORATIONS);
  case TABLE_FORMAT:
    return exec_until_formatting (t, p, var, level, CELL_FORMAT);
  case TABLE:
    return exec_until_table (t, p, var, level);
  case ASSIGN:
    (void) exec (t);
    return false;
  case WITH:
    return exec_until_with (t, p, var, level);
  case PROVIDES:
    (void) exec (t);
    return false;
  case VALUE:
    /*
    {
      tree r= t[0];
      if (is_compound (r)) r= exec (r);
      if (is_atomic (r) && (r->label == var)) {
	exec_until (read (r->label), p);
	return true;
      }
    }
    */
    (void) exec (t);
    return false;
  case MACRO:
  case DRD_PROPS:
    (void) exec (t);
    return false;
  case ARGUMENT:
    return exec_until_argument (t, p, var, level);
  case COMPOUND:
    return exec_until_compound (t, p, var, level);
  case XMACRO:
  case GET_LABEL:
  case GET_ARITY:
  case MAP_ARGS: // FIXME: is this OK?
  case EVAL_ARGS: // FIXME: is this OK?
  case EVAL:
  case QUOTE:
  case DELAY:
  case EXTERN: // FIXME: is this OK?
  case INCLUDE: // FIXME: is this OK?
  case OR:
  case XOR:
  case AND:
  case NOT:
  case PLUS:
  case MINUS:
  case TIMES:
  case OVER:
  case DIVIDE:
  case MODULO:
  case MERGE:
  case LENGTH:
  case RANGE:
  case NUMBER:
  case _DATE:
  case TRANSLATE:
  case FIND_FILE:
  case IS_TUPLE:
  case LOOK_UP:
  case EQUAL:
  case UNEQUAL:
  case LESS:
  case LESSEQ:
  case GREATER:
  case GREATEREQ:
  case IF:
  case VAR_IF:
  case CASE:
  case WHILE:
    (void) exec (t);
    return false;
  case INACTIVE:
  case ACTIVE:
  case VAR_INACTIVE:
  case VAR_ACTIVE:
    return exec_until_mod_active (t, p, var, level);
  default:
    if (L(t) < START_EXTENSIONS) {
      int i, n= N(t);
      for (i=0; i<n; i++)
	if (exec_until (t[i], p, var, level))
	  return true;
      return false;
    }
    else return exec_until_compound (t, p, var, level);
  }
}

bool
edit_env_rep::exec_until_formatting (
  tree t, path p, string var, int level, string v)
{
  int n= N(t);
  tree oldv= read (v);
  tree newv= join (oldv, t (0, n-1));
  monitored_write_update (v, newv);
  if (exec_until (t[n-1], p, var, level)) return true;
  monitored_write_update (v, oldv);
  return false;
}

bool
edit_env_rep::exec_until_table (tree t, path p, string var, int level) {
  tree oldv= read (CELL_FORMAT);
  // should execute values in oldv
  monitored_write_update (CELL_FORMAT, tree (TABLE_FORMAT));
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (exec_until (t[i], p, var, level))
      return true;
  monitored_write_update (CELL_FORMAT, oldv);
  return false;
}

bool
edit_env_rep::exec_until_with (tree t, path p, string var, int level) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) return false;
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(oldv,tree,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      oldv[i]= read (var);
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return false;
    }
  }

  for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  if (exec_until (t[n-1], p, var, level)) {
    STACK_DELETE_ARRAY(vars);
    STACK_DELETE_ARRAY(oldv);
    STACK_DELETE_ARRAY(newv);
    return true;
  }
  for (i=k-1; i>=0; i--) write_update (vars[i], oldv[i]);
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
  return false;
}

bool
edit_env_rep::exec_until_compound (tree t, path p, string var, int level) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!provides (var)) return false;
      f= read (var);
    }
  }
  else {
    string fname= as_string (L(t));
    if (!provides (fname)) return false;
    d= 0;
    f= read (fname);
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> >
      (hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> >
      (hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0]))
	macro_arg->item (f[0]->label)= t;
    }
    for (i=0; i<n; i++)
      if (is_atomic (f[i]))
	macro_arg->item (f[i]->label)= i<m? t[i+d]: tree("");
    bool done= exec_until (f[n], p, var, level+1);
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
    return done;
  }
  return false;
}

bool
edit_env_rep::exec_until_argument (tree t, path p, string var, int level) {
  // cout << "  " << macro_arg << "\n";
  tree r= t[0];
  if (is_atomic (r) && (!nil (macro_arg)) &&
      macro_arg->item->contains (r->label))
    {
      bool found;
      tree arg= macro_arg->item [r->label];
      list<hashmap<string,tree> > old_var= macro_arg;
      list<hashmap<string,path> > old_src= macro_src;
      if (!nil (macro_arg)) macro_arg= macro_arg->next;
      if (!nil (macro_src)) macro_src= macro_src->next;
      if (level == 0) {
	found= (r->label == var);
	if ((N(t) > 1) && found) {
	  int i, n= N(t);
	  for (i=1; i<n; i++) {
	    tree u= exec (t[i]);
	    if (!is_int (u)) { found= false; break; }
	    int nr= as_int (u);
	    if ((!is_compound (arg)) || (nr<0) || (nr>=N(arg)) ||
		nil (p) || (p->item != nr)) { found= false; break; }
	    arg= arg[nr];
	    p  = p->next;
	  }
	}
	if (found) exec_until (arg, p);
	else exec (arg);
      }
      else found= exec_until (arg, p, var, level-1);
      macro_arg= old_var;
      macro_src= old_src;
      return found;
    }
  else return false;
  /*
  cout << "  " << macro_arg << "\n";
  tree r= t[0];
  if (is_atomic (r) && (r->label == var) && (!nil (macro_arg))) {
    bool found= (level == 0) && macro_arg->item->contains (r->label);
    tree arg  = macro_arg->item [var];
    list<hashmap<string,tree> > old_var= macro_arg;
    list<hashmap<string,path> > old_src= macro_src;
    if (!nil (macro_arg)) macro_arg= macro_arg->next;
    if (!nil (macro_src)) macro_src= macro_src->next;
    if (found) exec_until (arg, p);
    else found= exec_until (arg, p, var, level-1);
    macro_arg= old_var;
    macro_src= old_src;
    return found;
  }
  */
}

bool
edit_env_rep::exec_until_mod_active (
  tree t, path p, string var, int level)
{
  if (p->item != 0) return false;
  t= t[0];
  p= p->next;
  if (is_atomic (t) || preamble) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (exec_until (t[i], p, var, level))
	return true;
    return false;
  }
}


/******************************************************************************
* Extra routines for macro expansion and function application
******************************************************************************/

tree
edit_env_rep::expand (tree t) {
  if (is_atomic (t) || nil (macro_arg)) return t;
  else if (is_func (t, ARGUMENT)) {
    if (is_compound (t[0]))
      return tree (ERROR, "bad argument application");
    if (!macro_arg->item->contains (t[0]->label))
      return tree (ERROR, "argument " * t[0]->label);
    tree r= macro_arg->item [r->label];
    list<hashmap<string,tree> > old_var= macro_arg;
    list<hashmap<string,path> > old_src= macro_src;
    if (!nil (macro_arg)) macro_arg= macro_arg->next;
    if (!nil (macro_src)) macro_src= macro_src->next;
    if (N(t) > 1) {
      int i, n= N(t);
      for (i=1; i<n; i++) {
	tree u= exec (t[i]);
	if (!is_int (u)) break;
	int nr= as_int (u);
	if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
	r= r[nr];
      }
    }
    r= expand (r);
    macro_arg= old_var;
    macro_src= old_src;
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= expand (t[i]);
    return r;
  }
}

bool
edit_env_rep::depends (tree t, string s, int level) {
  /*
  cout << "Depends? " << t << ", " << s << ", " << level
       << " " << macro_arg << "\n";
  */

  if (is_atomic (t) || nil (macro_arg)) return false;
  else if (is_func (t, ARGUMENT) ||
	   is_func (t, MAP_ARGS) ||
	   is_func (t, EVAL_ARGS))
    {
      // FIXME: this does not handle more complex dependencies,
      // like those encountered after rewritings (INCLUDE, EXTERN, etc.)
      tree v= (L(t) == MAP_ARGS? t[2]: t[0]);
      if (is_compound (v)) return false;
      if (!macro_arg->item->contains (v->label)) return false;
      if (level == 0) return v->label == s;
      tree r= macro_arg->item [v->label];
      list<hashmap<string,tree> > old_var= macro_arg;
      list<hashmap<string,path> > old_src= macro_src;
      if (!nil (macro_arg)) macro_arg= macro_arg->next;
      if (!nil (macro_src)) macro_src= macro_src->next;
      bool dep= depends (r, s, level-1);
      macro_arg= old_var;
      macro_src= old_src;
      return dep;
    }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (depends (t[i], s, level))
	return true;
    return false;
  }
}

/******************************************************************************
* Decoding and adding lengths
******************************************************************************/

SI
edit_env_rep::decode_length (string s) {
  while ((N(s) >= 2) && (s[0]=='-') && (s[1]=='-')) s= s (2, N(s));

  int i;
  for (i=0; (i<N(s)) && ((s[i]<'a') || (s[i]>'z')); i++);
  string s1 = s(0,i);
  string s2 = s(i,N(s));
  double x  = as_double (s1);
  double in = ((double) dpi*PIXEL);
  double cm = in/2.54;
  double f  = (get_int(FONT_BASE_SIZE)*magn*in*get_double(FONT_SIZE))/72.0;

  string s3= s2;
  int n= N(s3);
  if ((n>0) && ((s3[n-1] == '-') || (s3[n-1] == '+'))) s3= s3 (0, n-1);
  if (s3 == "unit") { return (SI) (x); }
  if (s3 == "cm") { return (SI) (x*cm); }
  if (s3 == "mm") { return (SI) (x*cm/10.0); }
  if (s3 == "in") { return (SI) (x*in); }
  if (s3 == "pt") { return (SI) (x*in/72.0); }
  if (s2 == "spc") { return (SI) (x*fn->spc->def); }
  if (s2 == "spc-") { return (SI) (x*fn->spc->min); }
  if (s2 == "spc+") { return (SI) (x*fn->spc->max); }
  if (s2 == "fn") { return (SI) (x*f); }
  if (s2 == "fn-") { return (SI) (0.5*x*f); }
  if (s2 == "fn+") { return (SI) (1.5*x*f); }
  if (s2 == "fn*") { return 0; }
  if (s2 == "fn*-") { return 0; }
  if (s2 == "fn*+") { return (SI) (x*f); }
  if (s2 == "ln") { return (SI) (x*((double) fn->wline)); }
  if (s2 == "sep") { return (SI) (x*((double) fn->sep)); }
  if (s3 == "px") { return (SI) (x*(get_int(SFACTOR)*PIXEL)); }
  if (s3 == "yfrac") { return (SI) (x*fn->yfrac); }
  if (s3 == "par") {
    SI width, d1, d2, d3, d4, d5, d6, d7;
    get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
    width -= (get_length (PAR_LEFT) + get_length (PAR_RIGHT));
    return (SI) (x*width);
  }
  if (s3 == "pag") {
    SI d1, height, d2, d3, d4, d5, d6, d7;
    get_page_pars (d1, height, d2, d3, d4, d5, d6, d7);
    return (SI) (x*height);
  }
  return 0;
}

space
edit_env_rep::decode_space (string l) {
  SI _min= decode_length (l * "-");
  SI _def= decode_length (l);
  SI _max= decode_length (l * "+");
  return space (_def + ((SI) (flexibility * (_min - _def))),
		_def,
		_def + ((SI) (flexibility * (_max - _def))));
}

void
edit_env_rep::get_length_unit(string s, SI& un, string& un_str) {
  int i;
  for (i=0; i<N(s); i++)
    if ((s[i]>='a') && (s[i]<='z')) break;
  un= decode_length ("1" * s (i, N(s)));
  un_str= s (i, N(s));
}

string
edit_env_rep::add_lengths (string s1, string s2) {
  SI l1= decode_length (s1);
  SI l2= decode_length (s2);
  SI un; string un_str;
  get_length_unit (s1, un, un_str);
  if (un==0) return "0cm";
  double x= ((double) (l1+l2)) / ((double) un);
  return as_string (x) * un_str;
}

string
edit_env_rep::multiply_length (double x, string s) {
  SI l= decode_length (s);
  SI un; string un_str;
  get_length_unit (s, un, un_str);
  if (un==0) return "0cm";
  double xl= (x*l) / ((double) un);
  return as_string (xl) * un_str;
}

double
edit_env_rep::divide_lengths (string s1, string s2) {
  SI l1= decode_length (s1);
  SI l2= decode_length (s2);
  return ((double) l1) / ((double) l2);
}


bool
edit_env_rep::is_length (string s) {
  int i;
  for (i=0; (i<N(s)) && ((s[i]<'a') || (s[i]>'z')); i++);
  if (!is_double (s (0, i))) return false;
  int j=N(s);
  while ((j>i) && ((s[j-1]=='+') || (s[j-1]=='-') || (s[j-1]=='*'))) j--;
  return is_alpha (s (i, j));
}
