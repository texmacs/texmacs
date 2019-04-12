
/******************************************************************************
* MODULE     : concat_macro.cpp
* DESCRIPTION: Typesetting markup around macro expansion mechanisms
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"

/******************************************************************************
* Typesetting environment changes
******************************************************************************/

void
concater_rep::typeset_assign (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  tree r= env->exec (t[0]);
  if (!is_atomic (r)) return;
  string var= r->label;
  env->assign (var, copy (t[1]));
  if (env->var_type [var] == Env_Paragraph)
    control (tuple ("env_par", var, env->read (var)), ip);
  else if (env->var_type [var] == Env_Page)
    control (tuple ("env_page", var, env->read (var)), ip);
  else control (t, ip);
}

void
concater_rep::typeset_provide (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  tree r= env->exec (t[0]);
  if (!is_atomic (r)) return;
  string var= r->label;
  if (!env->provides (var)) env->assign (var, copy (t[1]));
  if (env->var_type [var] == Env_Paragraph)
    control (tuple ("env_par", var, env->read (var)), ip);
  else if (env->var_type [var] == Env_Page)
    control (tuple ("env_page", var, env->read (var)), ip);
  else control (t, ip);
}

void
concater_rep::typeset_with (tree t, path ip) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) { typeset_error (t, ip); return; }

  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(oldv,tree,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= env->exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      oldv[i]= env->read (var);
      newv[i]= env->exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return;
    }
  }

  marker (descend (ip, 0));
  // for (i=0; i<k; i++) env->monitored_write_update (vars[i], newv[i]);
  for (i=0; i<k; i++) env->write_update (vars[i], newv[i]);
  typeset (t[n-1], descend (ip, n-1));
  for (i=k-1; i>=0; i--) env->write_update (vars[i], oldv[i]);
  marker (descend (ip, 1));
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
}

void
concater_rep::typeset_compound (tree t, path ip) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    if (N(t) == 0) { typeset_error (t, ip); return; }
    d= 1;
    f= t[0];
    if (is_compound (f)) f= env->exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!env->provides (var)) {
	typeset_error (t, ip);
	return;
      }
      f= env->read (var);
    }
  }
  else {
    string var= as_string (L(t));
    if (!env->provides (var)) {
      typeset_error (t, ip);
      return;
    }
    d= 0;
    f= env->read (var);
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0])) {
	string var= f[0]->label;
	env->macro_arg->item (var)= t;
	env->macro_src->item (var)= ip;
      }
    }
    else for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var= f[i]->label;
	env->macro_arg->item (var)=
	  i<m? t[i+d]: attach_dip (tree (UNINIT), decorate_right(ip));
	env->macro_src->item (var)= i<m? descend (ip,i+d): decorate_right(ip);
      }
    if (is_decoration (ip))
      typeset (attach_here (f[n], ip));
    else {
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 0));
      typeset (attach_right (f[n], ip));
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 1));
    }
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else {
    if (is_decoration (ip)) typeset (attach_here (f, ip));
    else {
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 0));
      typeset (attach_right (f, ip));
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 1));
    }
  }
}

void
concater_rep::typeset_auto (tree t, path ip, tree f) {
  env->macro_arg= list<hashmap<string,tree> > (
    hashmap<string,tree> (UNINIT), env->macro_arg);
  env->macro_src= list<hashmap<string,path> > (
    hashmap<string,path> (path (DECORATION)), env->macro_src);
  string var= f[0]->label;
  env->macro_arg->item (var)= t;
  env->macro_src->item (var)= ip;
  typeset (attach_right (f[1], ip));
  env->macro_arg= env->macro_arg->next;
  env->macro_src= env->macro_src->next;
}

void
concater_rep::typeset_include (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  url file_name= url_unix (env->exec_string (t[0]));
  url incl_file= relative (env->base_file_name, file_name);
  tree incl= load_inclusion (incl_file);
  url save_name= env->cur_file_name;
  env->cur_file_name= incl_file;
  env->secure= is_secure (env->cur_file_name);
  typeset_dynamic (incl, ip);
  env->cur_file_name= save_name;
  env->secure= is_secure (env->cur_file_name);
}

void
concater_rep::typeset_drd_props (tree t, path ip) {
  (void) env->exec (t);
  flag ("drd-properties", ip, brown);
  control (t, ip);
}

void
concater_rep::typeset_eval (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  tree r= env->exec (t[0]);
  typeset_dynamic (r, ip);
}

void
concater_rep::typeset_value (tree t, path ip) {
  // cout << "Value " << t << ", " << ip << "\n";
  if (N(t) != 1) { typeset_error (t, ip); return; }
  tree r= env->exec (t[0]);
  if (is_compound (r)) {
    typeset_error (t, ip);
    return;
  }
  string name= r->label;
  if (!env->provides (name)) {
    typeset_error (t, ip);
    return;
  }
  typeset_dynamic (env->read (name), ip);
}

void
concater_rep::typeset_or_value (tree t, path ip) {
  // cout << "Value " << t << ", " << ip << "\n";
  for (int i=0; i<N(t); i++) {
    tree r= env->exec (t[i]);
    if (is_compound (r)) {
      typeset_error (t, ip);
      return;
    }
    string name= r->label;
    if (env->provides (name) && !is_func (env->read (r->label), UNINIT)) {
      typeset_dynamic (env->read (name), ip);
      return;
    }
  }
}

void
concater_rep::typeset_argument (tree t, path ip) {
  // cout << "Argument " << t << ", " << ip << "\n";
  if (N(t) == 0) { typeset_error (t, ip); return; }
  tree r= t[0];
  if (is_compound (r) ||
      is_nil (env->macro_arg) ||
      (!env->macro_arg->item->contains (r->label)))
    {
      typeset_error (t, ip);
      return;
    }

  string name = r->label;
  tree   value= env->macro_arg->item [name];
  path   valip= decorate_right (ip);
  if (!is_func (value, BACKUP)) {
    path new_valip= env->macro_src->item [name];
    if (is_accessible (new_valip)) valip= new_valip;
  }
  // cout << "Src   " << name << "=\t " << valip << "\n";

  marker (descend (ip, 0));
  list<hashmap<string,tree> > old_var= env->macro_arg;
  list<hashmap<string,path> > old_src= env->macro_src;
  if (!is_nil (env->macro_arg)) env->macro_arg= env->macro_arg->next;
  if (!is_nil (env->macro_src)) env->macro_src= env->macro_src->next;

  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree r= env->exec (t[i]);
      if (!is_int (r)) {
        value= tree (ERROR, "arg " * name);
        valip= decorate_right (ip);
        break;
      }
      int nr= as_int (r);
      if ((!is_compound (value)) || (nr<0) || (nr>=N(value))) {
        value= tree (ERROR, "arg " * name);
        valip= decorate_right (ip);
        break;
      }
      value= value[nr];
      valip= descend (valip, nr);
    }
  }
  typeset (attach_here (value, valip));

  env->macro_arg= old_var;
  env->macro_src= old_src;
  marker (descend (ip, 1));
}

void
concater_rep::typeset_eval_args (tree t, path ip) { 
  if (N(t) != 1) { typeset_error (t, ip); return; }
  marker (descend (ip, 0));
  typeset_inactive (attach_right (env->exec (t), ip));
  marker (descend (ip, 1));
}

void
concater_rep::typeset_mark (tree t, path ip) {
  // cout << "Mark: " << t << ", " << ip << "\n\n";
  if (N(t) != 2) { typeset_error (t, ip); return; }
  if (is_func (t[0], ARG) &&
      is_atomic (t[0][0]) &&
      (!is_nil (env->macro_arg)) &&
      env->macro_arg->item->contains (t[0][0]->label))
    {
      string name = t[0][0]->label;
      tree   value= env->macro_arg->item [name];
      path   valip= decorate_right (ip);
      if (!is_func (value, BACKUP)) {
	path new_valip= env->macro_src->item [name];
	if (is_accessible (new_valip)) valip= new_valip;
      }
      // cout << "Src   " << name << "=\t " << valip << "\n";

      if (N(t[0]) > 1) {
	int i, n= N(t[0]);
	for (i=1; i<n; i++) {
	  tree r= env->exec (t[0][i]);
	  if (!is_int (r)) break;
	  int nr= as_int (r);
	  if ((!is_compound (value)) || (nr<0) || (nr>=N(value))) break;
	  value= value[nr];
	  valip= descend (valip, nr);
	}
      }

      marker (descend (valip, 0));
      typeset (t[1], descend (ip, 1));
      marker (descend (valip, right_index (value)));
    }
  else typeset (t[1], descend (ip, 1));
}

void
concater_rep::typeset_expand_as (tree t, path ip) {
  // cout << "Mark: " << t << ", " << ip << "\n\n";
  if (N(t) != 2) { typeset_error (t, ip); return; }
  marker (descend (ip, 0));
  typeset (t[1], descend (ip, 1));
  marker (descend (ip, 1));
}

void
concater_rep::typeset_executable (tree t, path ip) {
  tree r= env->exec (t);
  typeset_dynamic (r, ip);
}

void
concater_rep::typeset_rewrite (tree t, path ip) {
  tree r= env->rewrite (t);
  typeset_dynamic (r, ip);
}

void
concater_rep::typeset_dynamic (tree t, path ip) {
  // ATTENTION: in the future, the ip should still be passed to
  // this routine, since the typesetting depends on whether ip
  // is a decoration or not

  // cout << "Dynamic " << t << ", " << ip << "\n";
  if (is_decoration (ip)) {
    typeset (attach_here (t, ip));
    return;
  }

  marker (descend (ip, 0));
  // int start= N(a);
  typeset (attach_right (t, ip));
  // int end= N(a), i;

  /* Old style unaccessible text, using left and middle decorations
  SI total= 0, half= 0, mid;
  for (i=start; i<end; i++)
    total += a[i]->b->w();
  for (mid=start; mid<end; mid++) {
    half += a[mid]->b->w();
    if (half >= (total>>1)) break;
  }
  for (i=start; i<mid; i++)
    a[i]->b->relocate (decorate_left (ip));
  if (i<end) {
    if (a[i]->b->decoration ())
      if ((a[i]->type==STD_ITEM) ||
          (a[i]->type == MARKER_ITEM) ||
	  (a[i]->type==STRING_ITEM))
	a[i]->b= macro_box (decorate_right (ip), a[i]->b);
    a[i]->b->relocate (decorate_middle (ip));
    i++;
  }
  */

  marker (descend (ip, 1));

  /*
  for (i=start-1; i<end+1; i++)
    cout << i << ": " << a[i]->b
	 << ", " << a[i]->b->find_lip ()
	 << ", " << a[i]->b->find_rip () << "\n";
  */

  /*
  for (i=start; i<end; i++)
    cout << i << ": " << a[i]->b << ", " << a[i]->b->ip << "\n";
  */
}

void
concater_rep::typeset_range (tree t, path ip) {
  if (N(t) != 3) { typeset_error (t, ip); return; }
  tree t1= env->exec (t[0]);
  tree t2= env->exec (t[1]);
  tree t3= env->exec (t[2]);
  if (!(is_int (t2) && is_int (t3)))
    typeset_dynamic (tree (ERROR, "bad range"), ip);
  else if (is_compound (t1)) {
    if (is_tuple (t1)) {
      int i1= max (0, as_int (t2));
      int i2= min (N (t1), as_int (t3));
      i2 = max (i1, i2);
      typeset_dynamic (t1 (i1, i2), ip);
    }
    else typeset_dynamic (tree (ERROR, "bad range"), ip);
  }
  else {
    int i1= max (0, as_int (t2));
    int i2= min (N(t1->label), as_int (t3));
    i2 = max (i1, i2);
    path ip1= obtain_ip (t1);
    if (is_decoration (ip1))
      typeset_dynamic (t1->label (i1, i2), ip);
    else {
      marker (descend (ip, 0));
      if (env->mode == 1)
        typeset_text_string (t1->label, ip1, i1, i2);
      else if (env->mode == 2)
        typeset_math_string (t1->label, ip1, i1, i2);
      else if (env->mode == 3)
        typeset_prog_string (t1       , ip1, i1, i2);
      else
        typeset_text_string (t1->label, ip1, i1, i2);
      marker (descend (ip, 1));
    }
  }
}
