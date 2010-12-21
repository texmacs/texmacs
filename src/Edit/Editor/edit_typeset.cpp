
/******************************************************************************
* MODULE     : typeset.cpp
* DESCRIPTION: typeset the tree being edited
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_typeset.hpp"
#include "tm_buffer.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "timer.hpp"
#include "Bridge/impl_typesetter.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Environment/std_environment.hpp"
#endif // EXPERIMENTAL

//box empty_box (path ip, int x1=0, int y1=0, int x2=0, int y2=0);
bool enable_fastenv= false;

/******************************************************************************
* Contructors, destructors and notification of modifications
******************************************************************************/

static bool
is_aux (url u) {
  if (!is_atomic (u->t)) return false;
  string s= u->t->label;
  return starts (s, "* ") && ends (s, " *");
}

edit_typeset_rep::edit_typeset_rep ():
  the_style (TUPLE),
  cur (hashmap<string,tree> (UNINIT)),
  pre (UNINIT), init (UNINIT), fin (UNINIT),
  env (drd, is_aux (buf->name)? buf->extra: buf->name,
       buf->ref, (buf->prj==NULL? buf->ref: buf->prj->ref),
       buf->aux, (buf->prj==NULL? buf->aux: buf->prj->aux)),
  ttt (new_typesetter (env, subtree (et, rp), reverse (rp))) {}
edit_typeset_rep::~edit_typeset_rep () { delete_typesetter (ttt); }

typesetter edit_typeset_rep::get_typesetter () { return ttt; }
tree edit_typeset_rep::get_style () { return the_style; }
void edit_typeset_rep::set_style (tree t) { the_style= copy (t); }
hashmap<string,tree> edit_typeset_rep::get_init () { return init; }
hashmap<string,tree> edit_typeset_rep::get_fin () { return fin; }
void edit_typeset_rep::set_fin (hashmap<string,tree> H) { fin= H; }

void
edit_typeset_rep::set_init (hashmap<string,tree> H) {
  init= hashmap<string,tree> (UNINIT);
  add_init (H);
}

void
edit_typeset_rep::add_init (hashmap<string,tree> H) {
  init->join (H);
  ::notify_assign (ttt, path(), subtree (et, rp));
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::set_base_name (url name) {
  env->base_file_name= name;
}

void
edit_typeset_rep::clear_local_info () {
  buf->ref= hashmap<string,tree> ();
  buf->aux= hashmap<string,tree> ();
}

/******************************************************************************
* Miscellaneous routines for lengths arithmetic (should be elsewhere)
******************************************************************************/

SI
edit_typeset_rep::as_length (string l) {
  return env->as_length (l); }

string
edit_typeset_rep::add_lengths (string l1, string l2) {
  return env->add_lengths (l1, l2); }

string
edit_typeset_rep::multiply_length (double x, string l) {
  return env->multiply_length (x, l); }

bool
edit_typeset_rep::is_length (string s) {
  return env->is_length (s); }

double
edit_typeset_rep::divide_lengths (string l1, string l2) {
  return env->divide_lengths (l1, l2); }

/******************************************************************************
* Processing preamble
******************************************************************************/

void
use_modules (tree t) {
  if (is_tuple (t))
    for (int i=0; i<N(t); i++) {
      string s= as_string (t[i]);
      if (starts (s, "(")) eval ("(use-modules " * s * ")");
      else if (s != "") eval ("(plugin-initialize '" * s * ")");
    }
}

void
edit_typeset_rep::typeset_style_use_cache (tree style) {
  bool ok;
  hashmap<string,tree> H;
  tree t;
  SERVER (style_get_cache (style, H, t, ok));
  if (ok) {
    env->patch_env (H);
    ok= drd->set_locals (t);
    drd->set_environment (H);
  }
  if (!ok) {
    if (!is_tuple (style)) FAILED ("tuple expected as style");
    tree t (USE_PACKAGE, A (style));
    env->exec (t);
    env->read_env (H);
    drd->heuristic_init (H);
    SERVER (style_set_cache (style, H, drd->get_locals ()));
  }
  use_modules (env->read (THE_MODULES));
}

void
edit_typeset_rep::typeset_preamble () {
  env->write_default_env ();
  typeset_style_use_cache (the_style);
  env->patch_env (init);
  env->update ();
  env->read_env (pre);
  drd->heuristic_init (pre);
}

void
edit_typeset_rep::typeset_prepare () {
  env->read_only= buf->read_only;
  env->write_default_env ();
  env->patch_env (pre);
  env->style_init_env ();
  env->update ();
}

void
edit_typeset_rep::drd_update () {
  typeset_exec_until (tp);
  drd->heuristic_init (cur[tp]);
}

#ifdef EXPERIMENTAL
void
edit_typeset_rep::environment_update () {
  hashmap<string,tree> h;
  typeset_prepare ();
  env->assign ("base-file-name", as_string (env->base_file_name));
  env->assign ("cur-file-name", as_string (env->cur_file_name));
  env->assign ("secure", bool_as_tree (env->secure));
  env->read_env (h);
  ::primitive (ste, h);
}
#endif

/******************************************************************************
* Routines for getting information
******************************************************************************/

void
edit_typeset_rep::typeset_invalidate_env () {
  cur= hashmap<path,hashmap<string,tree> > (hashmap<string,tree> (UNINIT));
}

void
edit_typeset_rep::typeset_exec_until (path p) {
  //time_t t1= texmacs_time ();
  if (has_changed (THE_TREE + THE_ENVIRONMENT))
    if (p != correct_cursor (et, rp * 0)) {
      if (DEBUG_STD)
	cout << "TeXmacs] Warning: resynchronizing for path " << p << "\n";
      // apply_changes ();
    }
  if (p == tp && inside_graphics (true) && p != closest_inside (et, p)) {
    //cout << "TeXmacs] Warning: corrected cursor\n";
    tp= closest_inside (et, tp);
    p = tp;
  }

  if (N(cur[p])!=0) return;
  if (N(cur)>=25) // avoids out of memory in weird cases
    typeset_invalidate_env ();
  typeset_prepare ();
  if (enable_fastenv) {
    tree t= subtree (et, rp);
    if (is_func (t, DOCUMENT) && N(t) > 0)
      if (is_compound (t[0], "hide-preamble", 1) ||
	  is_compound (t[0], "show-preamble", 1))
	env->exec (t[0][0]);
    path q= path_up (p / rp);
    while (!is_nil (q)) {
      int i= q->item;
      tree w= drd->get_env_child (t, i, tree (ATTR));
      if (w == "") break;
      //cout << "t= " << t << "\n";
      //cout << "i= " << i << "\n";
      //cout << "w= " << w << "\n";
      for (int j=0; j<N(w); j+=2) {
	//cout << w[j] << " := " << env->exec (w[j+1]) << "\n";
	env->write (w[j]->label, env->exec (w[j+1]));
      }
      t= t[i];
      q= q->next;
    }
    if (env->read (PREAMBLE) == "true")
      env->write (MODE, "src");
  }
  else exec_until (ttt, p / rp);
  env->read_env (cur (p));
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset_exec_until took " << t2-t1 << "ms\n";
}

bool
edit_typeset_rep::defined_at_cursor (string var) {
  typeset_exec_until (tp);
  return cur[tp]->contains (var);
}

tree
edit_typeset_rep::get_env_value (string var, path p) {
  typeset_exec_until (p);
  tree t= cur[p][var];
  return is_func (t, BACKUP, 2)? t[0]: t;
}

tree
edit_typeset_rep::get_env_value (string var) {
 /* FIXME: tp is wrong (and consequently, crashes TeXmacs)
  *   when we call this routine from inside the code which
  *   is triggered by a button, for example.
  *
  * Test: fire TeXmacs, then open a new Graphics, then click
  *   on the icon for going in spline mode. Then it crashes,
  *   because we call (get-env-tree) from inside the Scheme.
  *   If we call (get-env-tree-at ... (cDr (cursor-path))),
  *   then it works.
  */
  return get_env_value (var, tp);
}

bool
edit_typeset_rep::defined_at_init (string var) {
  if (init->contains (var)) return true;
  if (N(pre)==0) typeset_preamble ();
  return pre->contains (var);
}

bool
edit_typeset_rep::defined_in_init (string var) {
  return init->contains (var);
}

tree
edit_typeset_rep::get_init_value (string var) {
  if (init->contains (var)) {
    tree t= init [var];
    return is_func (t, BACKUP, 2)? t[0]: t;
  }
  if (N(pre)==0) typeset_preamble ();
  tree t= pre [var];
  return is_func (t, BACKUP, 2)? t[0]: t;
}

string
edit_typeset_rep::get_env_string (string var) {
  return as_string (get_env_value (var));
}

string
edit_typeset_rep::get_init_string (string var) {
  return as_string (get_init_value (var));
}

int
edit_typeset_rep::get_env_int (string var) {
  return as_int (get_env_value (var));
}

int
edit_typeset_rep::get_init_int (string var) {
  return as_int (get_init_value (var));
}

double
edit_typeset_rep::get_env_double (string var) {
  return as_double (get_env_value (var));
}

double
edit_typeset_rep::get_init_double (string var) {
  return as_double (get_init_value (var));
}

language
edit_typeset_rep::get_env_language () {
  string mode= get_env_string (MODE);
  if (mode == "text")
    return text_language (get_env_string (LANGUAGE));
  else if (mode == "math")
    return math_language (get_env_string (MATH_LANGUAGE));
  else return prog_language (get_env_string (PROG_LANGUAGE));
}

/******************************************************************************
* Execution without typesetting
******************************************************************************/

static tree
simplify_execed (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= simplify_execed (t[i]);
  if (is_func (r, QUOTE, 1) && is_atomic (r[0]))
    return r[0];
  else return r;
}

static tree
expand_references (tree t, hashmap<string,tree> h) {
  if (is_atomic (t)) return t;
  if (is_func (t, REFERENCE, 1) || is_func (t, PAGEREF)) {
    string ref= as_string (simplify_execed (t[0]));
    if (h->contains (ref)) {
      int which= is_func (t, REFERENCE, 1)? 0: 1;
      return tree (HLINK, copy (h[ref][which]), "#" * ref);
    }
    return tree (HLINK, "?", "#" * ref);
  }
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= expand_references (t[i], h);
  return r;  
}

tree
edit_typeset_rep::exec (tree t, hashmap<string,tree> H, bool expand_refs) {
  hashmap<string,tree> H2;
  env->read_env (H2);
  env->write_env (H);
  t= env->exec (t);
  if (expand_refs)
    t= expand_references (t, buf->ref);
  t= simplify_execed (t);
  t= simplify_correct (t);
  env->write_env (H2);
  return t;
}

tree
edit_typeset_rep::exec_texmacs (tree t, path p) {
  typeset_exec_until (p);
  return exec (t, cur[p]);
}

tree
edit_typeset_rep::exec_texmacs (tree t) {
  return exec_texmacs (t, rp * 0);
}

tree
edit_typeset_rep::exec_verbatim (tree t, path p) {
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  H ("TeXmacs")= tree (MACRO, "TeXmacs");
  H ("LaTeX")= tree (MACRO, "LaTeX");
  H ("TeX")= tree (MACRO, "TeX");
  return exec (t, H);
}

tree
edit_typeset_rep::exec_verbatim (tree t) {
  return exec_verbatim (t, rp * 0);
}

tree
edit_typeset_rep::exec_html (tree t, path p) {
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(stree->tree (tmhtml-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  return exec (t, H);
  //tree r= exec (t, H);
  //cout << "In: " << t << "\n";
  //cout << "Out: " << r << "\n";
  //return r;
}

tree
edit_typeset_rep::exec_html (tree t) {
  return exec_html (t, rp * 0);
}

static tree
value_to_compound (tree t, hashmap<string,tree> h) {
  if (is_atomic (t)) return t;
  else if (is_func (t, VALUE, 1) &&
	   is_atomic (t[0]) &&
	   h->contains (t[0]->label))
    return compound (t[0]->label);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= value_to_compound (t[i], h);
    return r;
  }
}

tree
edit_typeset_rep::exec_latex (tree t, path p) {
  string pref= "texmacs->latex:expand-macros";
  if (as_string (call ("get-preference", pref)) != "on") return t;
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (call ("stree->tree", call ("tmtex-env-patch", t)));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  if (is_document (t) && is_compound (t[0], "hide-preamble")) {
    tree r= copy (t);
    r[0]= "";
    r= exec (value_to_compound (r, P), H, false);
    r[0]= exec (t[0], H, false);
    return r;
  }
  else return exec (value_to_compound (t, P), H, false);
}

tree
edit_typeset_rep::exec_latex (tree t) {
  return exec_latex (t, rp * 0);
}

tree
edit_typeset_rep::texmacs_exec (tree t) {
  return ::texmacs_exec (env, t);
}

/******************************************************************************
* Initialization
******************************************************************************/

void
edit_typeset_rep::init_style () {
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_style (string name) {
  if ((name == "none") || (name == "") || (name == "style")) the_style= TUPLE;
  else if (arity (the_style) == 0) the_style= tree (TUPLE, name);
  else the_style= tree (TUPLE, name) * the_style (1, N(the_style));
  require_save ();
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_add_package (string name) {
  int i, n= N(the_style);
  for (i=0; i<n; i++)
    if (the_style[i] == name)
      return;

  the_style << tree (name);
  require_save ();
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_remove_package (string name) {
  int i, n= N(the_style);
  tree new_style= tree (TUPLE);
  for (i=0; i<n; i++)
    if (the_style[i] == name) {
      require_save ();
      notify_change (THE_ENVIRONMENT);
    }
    else new_style << the_style[i];
  the_style= new_style;
}

void
edit_typeset_rep::init_env (string var, tree by) {
  if (init (var) == by) return;
  init (var)= by;
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_default (string var) {
  if (!init->contains (var)) return;
  init->reset (var);
  notify_change (THE_ENVIRONMENT);
}

/******************************************************************************
* Actual typesetting
******************************************************************************/

void
edit_typeset_rep::typeset (SI& x1, SI& y1, SI& x2, SI& y2) {
  //time_t t1= texmacs_time ();
  typeset_prepare ();
  eb= empty_box (reverse (rp));
  // saves memory, also necessary for change_log update
  bench_start ("typeset");
  eb= ::typeset (ttt, x1, y1, x2, y2);
  bench_end ("typeset");
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset took " << t2-t1 << "ms\n";
}

void
edit_typeset_rep::typeset_invalidate (path p) {
  if (rp <= p) {
    //cout << "Invalidate " << p << "\n";
    notify_change (THE_TREE);
    ::notify_assign (ttt, p / rp, subtree (et, p));
  }
}

void
edit_typeset_rep::typeset_invalidate_all () {
  //cout << "Invalidate all\n";
  notify_change (THE_ENVIRONMENT);
  typeset_preamble ();
  ::notify_assign (ttt, path(), subtree (et, rp));
}
