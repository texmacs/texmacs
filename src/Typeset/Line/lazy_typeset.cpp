
/******************************************************************************
* MODULE     : lazy_typeset.cpp
* DESCRIPTION: Lazy typesetting of various primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Line/lazy_typeset.hpp"
#include "Line/lazy_vstream.hpp"
#include "Format/format.hpp"
#include "Stack/stacker.hpp"

array<line_item> typeset_marker (edit_env env, path ip);
array<line_item> typeset_concat (edit_env, tree t, path ip);
array<line_item> join (array<line_item> a, array<line_item> b);
lazy make_lazy_paragraph (edit_env env, tree t, path ip);
lazy make_lazy_table (edit_env env, tree t, path ip);

/******************************************************************************
* Documents
******************************************************************************/

lazy_document_rep::lazy_document_rep (edit_env env, tree t, path ip):
  lazy_rep (LAZY_DOCUMENT, ip), par (N(t))
{
  int i, n= N(t);
  for (i=0; i<n; i++)
    par[i]= make_lazy (env, t[i], descend (ip, i));
}

format
lazy_document_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    query_vstream_width qvw= (query_vstream_width) fm;
    array<line_item> before= qvw->before;
    array<line_item> after = qvw->after;

    SI w= 1;
    int i, n= N(par);
    for (i=0; i<n; i++) {
      format tmp_fm= make_query_vstream_width (
        i==0  ? before: array<line_item> (),
	i==n-1? after : array<line_item> ());
      format ret_fm= par[i]->query (request, tmp_fm);
      format_width fmw= (format_width) ret_fm;
      w= max (w, fmw->width);
    }
    return make_format_width (w);
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_document_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM) {
    int i, n= N(par);
    SI width= 1;
    array<line_item> before;
    array<line_item> after;
    if (fm->type == FORMAT_VSTREAM) {
      format_vstream fs= (format_vstream) fm;
      width = fs->width ;
      before= fs->before;
      after = fs->after ;
    }
    array<page_item> l;
    stack_border     sb;
    for (i=0; i<n; i++) {
      format tmp_fm= make_format_vstream (width,
        i==0  ? before: array<line_item> (),
	i==n-1? after : array<line_item> ());
      lazy tmp= par[i]->produce (request, tmp_fm);
      lazy_vstream tmp_vs= (lazy_vstream) tmp;
      if (i == 0) {
	l = tmp_vs->l ;
	sb= tmp_vs->sb;
      }
      else merge_stack (l, sb, tmp_vs->l, tmp_vs->sb);
    }
    return lazy_vstream (ip, "", l, sb);
  }
  return lazy_rep::produce (request, fm);
}

/******************************************************************************
* Surround
******************************************************************************/

lazy_surround_rep::lazy_surround_rep (edit_env env, tree t, path ip):
  lazy_rep (LAZY_SURROUND, ip)
{
  a  = typeset_concat (env, t[0], descend (ip, 0));
  b  = typeset_concat (env, t[1], descend (ip, 1));
  par= make_lazy (env, t[2], descend (ip, 2));
}

lazy_surround_rep::lazy_surround_rep (
  array<line_item> a2, array<line_item> b2, lazy par2, path ip):
  lazy_rep (LAZY_SURROUND, ip), a (a2), b (b2), par (par2) {}

format
lazy_surround_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    query_vstream_width qvw= (query_vstream_width) fm;
    array<line_item> before= qvw->before;
    array<line_item> after = qvw->after;
    if (N(a) != 0) before= join (a, before);
    if (N(b) != 0) after = join (after, b);
    format tmp_fm= make_query_vstream_width (before, after);
    return par->query (request, tmp_fm);
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_surround_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM) {
    SI               width = 1;
    array<line_item> before= a;
    array<line_item> after = b;
    if (fm->type == FORMAT_VSTREAM) {
      format_vstream fs= (format_vstream) fm;
      width = fs->width ;
      before= join (before, fs->before);
      after = join (fs->after, after);
    }
    format ret_fm= make_format_vstream (width, before, after);
    return par->produce (request, ret_fm);
  }
  return lazy_rep::produce (request, fm);
}

/******************************************************************************
* Formatting
******************************************************************************/

lazy
make_lazy_formatting (edit_env env, tree t, path ip, string v) {
  int last= N(t)-1;
  tree new_format= join (env->read (v), t (0, last));
  tree old_format= env->local_begin (v, new_format);
  array<line_item> a;
  array<line_item> b;
  if (v != CELL_FORMAT) {
    a= typeset_marker (env, descend (ip, 0));
    b= typeset_marker (env, descend (ip, 1));
  }
  lazy par= make_lazy (env, t[last], descend (ip, last));
  env->local_end (v, old_format);
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* With
******************************************************************************/

lazy
make_lazy_with (edit_env env, tree t, path ip) {
  int last= N(t)-1;
  int i, k= last>>1; // is k=0 allowed ?
  // if ((last&1) != 0) return;
  
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
    /*
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return;
    }
    */
  }

  // for (i=0; i<k; i++) env->monitored_write_update (vars[i], newv[i]);
  for (i=0; i<k; i++) env->write_update (vars[i], newv[i]);
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  lazy par= make_lazy (env, t[last], descend (ip, last));
  for (i=k-1; i>=0; i--) env->write_update (vars[i], oldv[i]);
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Expand
******************************************************************************/

lazy
make_lazy_expand (edit_env env, tree t, path ip) {
  tree f= t[0];
  if (is_compound (f)) f= env->exec (f);
  if (is_atomic (f)) {
    string var= f->label;
    if (env->provides (var)) f= env->read (var);
    else f= tree (ERROR, "expand " * var);
  }

  array<line_item> a;
  array<line_item> b;
  if ((!is_func (t, VAR_EXPAND)) && (!is_decoration (ip))) {
    a= typeset_marker (env, descend (ip, 0));
    b= typeset_marker (env, descend (ip, 1));
  }
  lazy par;

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-1;
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var= f[i]->label;
	env->macro_arg->item (var)= i<m? t[i+1]: tree("");
	env->macro_src->item (var)= i<m? descend (ip,i+1): decorate_right(ip);
      }
    if (is_decoration (ip)) par= make_lazy (env, f[n], ip);
    else par= make_lazy (env, f[n], decorate_right (ip));
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else {
    if (is_decoration (ip)) par= make_lazy (env, f, ip);
    else par= make_lazy (env, f, decorate_right (ip));
  }
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Apply
******************************************************************************/

lazy
make_lazy_apply (edit_env env, tree t, path ip) {
  if (env->preamble) return make_lazy_paragraph (env, t, ip);

  tree f= t[0];
  if (is_compound (f)) f= env->exec (f);
  if (is_atomic (f)) {
    string var= f->label;
    if (env->provides (var)) f= env->read (var);
    else f= tree (ERROR, "apply " * var);
  }

  lazy par;
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  if (is_applicable (f)) {
    int i, k=N(f)-1, n=N(t)-1; // is k=0 allowed ?
    STACK_NEW_ARRAY(vars,string,k);
    STACK_NEW_ARRAY(oldv,tree,k);
    STACK_NEW_ARRAY(newv,tree,k);
    for (i=0; i<k; i++)
      if (is_atomic (f[i])) {
	vars[i]= f[i]->label;
	oldv[i]= env->read (vars[i]);
	newv[i]= (i<n? env->exec (t[i+1]): tree (""));
	if ((i==k-1) && (n>=k)) {
	  int nv= N(vars[i]);
	  if ((nv>0) && (vars[i][nv-1]=='*')) {
	    vars[i]= vars[i] (0, nv-1);
	    newv[i]= env->exec_extra_list (t, i+1);
	  }
	  else if (n>k) newv[i]= env->exec_extra_tuple (t, i+1);
	}
	env->monitored_write (vars[i], newv[i]);
      }
      /*
      else {
	STACK_DELETE_ARRAY(vars);
	STACK_DELETE_ARRAY(oldv);
	STACK_DELETE_ARRAY(newv);
	return;
      }
      */
    par= make_lazy (env, f[k], decorate_right (ip));
    for (i=k-1; i>=0; i--) env->write (vars[i], oldv[i]);
    STACK_DELETE_ARRAY(vars);
    STACK_DELETE_ARRAY(oldv);
    STACK_DELETE_ARRAY(newv);
  }
  else par= make_lazy (env, f, decorate_right (ip));
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Include
******************************************************************************/

lazy
make_lazy_include (edit_env env, tree t, path ip) {
  if (env->preamble) return make_lazy_paragraph (env, t, ip);

  url file_name= as_string (t[0]);
  tree incl= load_inclusion (relative (env->base_file_name, file_name));
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  lazy par= make_lazy (env, incl, decorate_right (ip));
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Argument
******************************************************************************/

lazy
make_lazy_argument (edit_env env, tree t, path ip) {
  string name;
  tree   value;
  path   valip= decorate_right (ip);

  tree r= t[0];
  if (is_compound (r)) value= tree (ERROR, "value");
  else {
    name = r->label;
    if ((!nil (env->macro_arg)) && env->macro_arg->item->contains (r->label)) {
      value= env->macro_arg->item [name];
      if (!is_func (value, BACKUP)) {
	path new_valip= env->macro_src->item [name];
	if (is_accessible (new_valip)) {
	  valip= new_valip;
	  env->macro_src->item (name)= decorate_right (valip);
	}
      }
    }
    else value= tree (ERROR, "value " * name);
  }

  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  list<hashmap<string,tree> > old_var= env->macro_arg;
  list<hashmap<string,path> > old_src= env->macro_src;
  if (!nil (env->macro_arg)) env->macro_arg= env->macro_arg->next;
  if (!nil (env->macro_src)) env->macro_src= env->macro_src->next;
  lazy par= make_lazy (env, value, valip);
  env->macro_arg= old_var;
  env->macro_src= old_src;
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Main routine
******************************************************************************/

lazy
make_lazy (edit_env env, tree t, path ip) {
  switch (L(t)) {
  case DOCUMENT:
    return lazy_document (env, t, ip);
  case SURROUND:
    return lazy_surround (env, t, ip);
  case DECORATE_ATOMS:
    return make_lazy_formatting (env, t, ip, ATOM_DECORATIONS);
  case DECORATE_LINES:
    return make_lazy_formatting (env, t, ip, LINE_DECORATIONS);
  case DECORATE_PAGES:
    return make_lazy_formatting (env, t, ip, PAGE_DECORATIONS);
  case TABLE_FORMAT:
    return make_lazy_formatting (env, t, ip, CELL_FORMAT);
  case TABLE:
    return make_lazy_table (env, t, ip);
  case WITH:
    return make_lazy_with (env, t, ip);
  case EXPAND:
  case VAR_EXPAND:
  case HIDE_EXPAND:
    return make_lazy_expand (env, t, ip);
  case APPLY:
    return make_lazy_apply (env, t, ip);
  case INCLUDE:
    return make_lazy_include (env, t, ip);
  case ARGUMENT:
    return make_lazy_argument (env, t, ip);
  default:
    return make_lazy_paragraph (env, t, ip);
  }
}
