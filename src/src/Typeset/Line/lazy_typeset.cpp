
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
    if (N(a) != 0) before= join (before, a);
    if (N(b) != 0) after = join (b, after);
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
      before= join (fs->before, before);
      after = join (after, fs->after);
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
  tree new_format= env->read (v) * t (0, last);
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
* Compound
******************************************************************************/

lazy
make_lazy_compound (edit_env env, tree t, path ip) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= env->exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (env->provides (var)) f= env->read (var);
      else f= tree (ERROR, "compound " * var);
    }
  }
  else {
    string var= as_string (L(t));
    if (env->provides (var)) f= env->read (var);
    else f= tree (ERROR, "compound " * var);
    d= 0;
  }

  array<line_item> a;
  array<line_item> b;
  if (/*NON_CHILD_ENFORCING(t)&&*/ (!is_decoration (ip))) {
    a= typeset_marker (env, descend (ip, 0));
    b= typeset_marker (env, descend (ip, 1));
  }
  lazy par;

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
* Rewrite
******************************************************************************/

lazy
make_lazy_rewrite (edit_env env, tree t, path ip) {
  tree r= env->rewrite (t);
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  lazy par= make_lazy (env, r, decorate_right (ip));
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Eval
******************************************************************************/

lazy
make_lazy_eval (edit_env env, tree t, path ip) {
  tree r= env->exec (is_func (t, EVAL, 1)? t[0]: tree (QUASIQUOTE, t[0]));
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));
  lazy par= make_lazy (env, r, decorate_right (ip));
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Auto
******************************************************************************/

lazy
make_lazy_auto (edit_env env, tree t, path ip, tree f) {
  array<line_item> a;
  array<line_item> b;
  if (!is_decoration (ip)) {
    a= typeset_marker (env, descend (ip, 0));
    b= typeset_marker (env, descend (ip, 1));
  }

  lazy par;
  env->macro_arg= list<hashmap<string,tree> > (
    hashmap<string,tree> (UNINIT), env->macro_arg);
  env->macro_src= list<hashmap<string,path> > (
    hashmap<string,path> (path (DECORATION)), env->macro_src);
  string var= f[0]->label;
  env->macro_arg->item (var)= t;
  env->macro_src->item (var)= ip;
  if (is_decoration (ip)) par= make_lazy (env, f[1], ip);
  else par= make_lazy (env, f[1], decorate_right (ip));
  env->macro_arg= env->macro_arg->next;
  env->macro_src= env->macro_src->next;

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
	if (is_accessible (new_valip)) valip= new_valip;
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
  lazy par= make_lazy (env, value, valip);

  env->macro_arg= old_var;
  env->macro_src= old_src;
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Mark
******************************************************************************/

lazy
make_lazy_mark (edit_env env, tree t, path ip) {
  // cout << "Lazy mark: " << t << ", " << ip << "\n";
  array<line_item> a= typeset_marker (env, descend (ip, 0));
  array<line_item> b= typeset_marker (env, descend (ip, 1));

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
      if (is_compound (value)) {
	a= typeset_marker (env, descend (valip, 0));
	b= typeset_marker (env, descend (valip, 1));
      }
    }

  lazy par= make_lazy (env, t[1], descend (ip, 1));
  return lazy_surround (a, b, par, ip);
}

/******************************************************************************
* Main routine
******************************************************************************/

static tree inactive_m
  (MACRO, "x",
   tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "once*"));
static tree var_inactive_m
  (MACRO, "x",
   tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "recurse*"));

lazy
make_lazy (edit_env env, tree t, path ip) {
  switch (L(t)) {
  case DOCUMENT:
    return lazy_document (env, t, ip);
  case SURROUND:
    return lazy_surround (env, t, ip);
  case DATOMS:
    return make_lazy_formatting (env, t, ip, ATOM_DECORATIONS);
  case DLINES:
    return make_lazy_formatting (env, t, ip, LINE_DECORATIONS);
  case DPAGES:
    return make_lazy_formatting (env, t, ip, PAGE_DECORATIONS);
  case TFORMAT:
    return make_lazy_formatting (env, t, ip, CELL_FORMAT);
  case TABLE:
    return make_lazy_table (env, t, ip);
  case WITH:
    return make_lazy_with (env, t, ip);
  case ARG:
    return make_lazy_argument (env, t, ip);
  case MARK:
    return make_lazy_mark (env, t, ip);
  case EVAL:
  case QUASI:
    return make_lazy_eval (env, t, ip);
  case COMPOUND:
    return make_lazy_compound (env, t, ip);
  case EXTERN:
    return make_lazy_rewrite (env, t, ip);
  case INCLUDE:
    return make_lazy_rewrite (env, t, ip);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return make_lazy_compound (env, t, ip);
  case INACTIVE:
    return make_lazy_auto (env, t, ip, inactive_m);
  case VAR_INACTIVE:
    return make_lazy_auto (env, t, ip, var_inactive_m);
  case REWRITE_INACTIVE:
    return make_lazy_rewrite (env, t, ip);
  default:
    if (L(t) < START_EXTENSIONS) return make_lazy_paragraph (env, t, ip);
    else return make_lazy_compound (env, t, ip);
  }
}
