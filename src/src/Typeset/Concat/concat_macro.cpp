
/******************************************************************************
* MODULE     : concat_macro.cpp
* DESCRIPTION: Typesetting markup around macro expansion mechanisms
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"

/******************************************************************************
* Typesetting environment changes
******************************************************************************/

void
concater_rep::typeset_assign (tree t, path ip) {
  tree r= env->exec (t[0]);
  if ((N(t)==2) && is_atomic (r)) {
    string var= r->label;
    env->assign (var, t[1]);
    if (env->var_type [var] == Env_Paragraph)
      control (tuple ("env_par", var, env->read (var)), ip);
    else if (env->var_type [var] == Env_Page)
      control (tuple ("env_page", var, env->read (var)), ip);
    else control (t, ip);
  }
}

void
concater_rep::typeset_with (tree t, path ip) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) return;

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
	env->macro_arg->item (var)= i<m? t[i+d]: tree (UNINIT);
	env->macro_src->item (var)= i<m? descend (ip,i+d): decorate_right(ip);
      }
    if (is_decoration (ip)) typeset (f[n], ip);
    else {
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 0));
      typeset (f[n], decorate_right (ip));
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 1));
    }
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else {
    if (is_decoration (ip)) typeset (f, ip);
    else {
      /*IF_NON_CHILD_ENFORCING(t)*/ marker (descend (ip, 0));
      typeset (f, decorate_right (ip));
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
  typeset (f[1], decorate_right (ip));
  env->macro_arg= env->macro_arg->next;
  env->macro_src= env->macro_src->next;
}

void
concater_rep::typeset_include (tree t, path ip) {
  url file_name= as_string (t[0]);
  tree incl= load_inclusion (relative (env->base_file_name, file_name));
  typeset_dynamic (incl, ip);
}

void
concater_rep::typeset_drd_props (tree t, path ip) {
  (void) env->exec (t);
  flag ("drd-properties", ip, env->dis->brown);
  control (t, ip);
}

void
concater_rep::typeset_eval (tree t, path ip) {
  tree r= env->exec (t[0]);
  typeset_dynamic (r, ip);
}

void
concater_rep::typeset_value (tree t, path ip) {
  // cout << "Value " << t << ", " << ip << "\n";
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
concater_rep::typeset_argument (tree t, path ip) {
  // cout << "Argument " << t << ", " << ip << "\n";
  tree r= t[0];
  if (is_compound (r) ||
      nil (env->macro_arg) ||
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
  if (!nil (env->macro_arg)) env->macro_arg= env->macro_arg->next;
  if (!nil (env->macro_src)) env->macro_src= env->macro_src->next;

  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree r= env->exec (t[i]);
      if (!is_int (r)) break;
      int nr= as_int (r);
      if ((!is_compound (value)) || (nr<0) || (nr>=N(value))) break;
      value= value[nr];
      valip= descend (valip, nr);
    }
  }
  typeset (value, valip);

  env->macro_arg= old_var;
  env->macro_src= old_src;
  marker (descend (ip, 1));
}

void
concater_rep::typeset_eval_args (tree t, path ip) { 
  marker (descend (ip, 0));
  typeset_inactive (env->exec (t), decorate_right (ip));
  marker (descend (ip, 1));
}

void
concater_rep::typeset_mark (tree t, path ip) {
  // cout << "Argument " << t << ", " << ip << "\n";
  if (is_func (t[0], ARG) &&
      is_atomic (t[0][0]) &&
      (!nil (env->macro_arg)) &&
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
  // cout << "Dynamic " << t << ", " << ip << "\n";
  if (is_decoration (ip)) {
    typeset (t, ip);
    return;
  }

  marker (descend (ip, 0));
  // int start= N(a);
  typeset (t, decorate_right (ip));
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
      if ((a[i]->type==STD_ITEM) || (a[i]->type==STRING_ITEM))
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
