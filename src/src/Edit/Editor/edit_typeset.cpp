
/******************************************************************************
* MODULE     : typeset.cpp
* DESCRIPTION: typeset the tree being edited
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_typeset.hpp"
#include "tm_buffer.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "analyze.hpp"

box empty_box (path ip, int x1=0, int y1=0, int x2=0, int y2=0);

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
  env (dis, drd, is_aux (buf->name)? buf->extra: buf->name,
       buf->ref, (buf->prj==NULL? buf->ref: buf->prj->ref),
       buf->aux, (buf->prj==NULL? buf->aux: buf->prj->aux)),
  ttt (new_typesetter (env, et, path())) {}
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
  ::notify_assign (ttt, path(), et);
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
edit_typeset_rep::decode_length (string l) {
  return env->decode_length (l); }

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
edit_typeset_rep::typeset_style (tree style) {
  //cout << "Process style " << style << "\n";
  if (L(style) != TUPLE)
    fatal_error ("tuple expected as style",
		 "edit_interface_rep::process_style");

  int i, n= N (style);
  for (i=0; i<n; i++) {
    url styp= "$TEXMACS_STYLE_PATH";
    url name= as_string (style[i]) * string (".ts");
    //cout << "Package " << name << "\n";
    if (is_rooted_web (env->base_file_name))
      styp= styp | head (env->base_file_name);
    else styp= head (env->base_file_name) | styp;
    string doc_s;
    if (!load_string (styp * name, doc_s, false)) {
      tree doc= texmacs_document_to_tree (doc_s);
      if (is_compound (doc)) {
	typeset_style (extract (doc, "style"));
	env->exec (extract (doc, "body"));
      }
    }
  }
}

void
edit_typeset_rep::typeset_style_use_cache (tree style) {
  bool ok;
  hashmap<string,tree> H;
  SERVER (style_get_cache (style, H, ok));
  if (ok) env->patch_env (H);
  else {
    tree style2= style;
    if (is_tuple (style2))
      style2= ::join (::join (tuple ("std--before"), style2),
		     tuple ("std--after"));
    typeset_style (style2);
    env->read_env (H);
    if ((!init->contains (PREAMBLE)) || (init[PREAMBLE] == "false"))
      SERVER (style_set_cache (style, H));
  }
}

void
edit_typeset_rep::typeset_preamble () {
  env->write_default_env ();
  typeset_style_use_cache (the_style);
  env->patch_env (init);
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

/******************************************************************************
* Routines for getting information
******************************************************************************/

void
edit_typeset_rep::typeset_invalidate_env () {
  cur= hashmap<path,hashmap<string,tree> > (hashmap<string,tree> (UNINIT));
}

void
edit_typeset_rep::typeset_exec_until (path p) {
  if (N(cur[p])!=0) return;
  if (N(cur)>=25) // avoids out of memory in weird cases
    typeset_invalidate_env ();
  typeset_prepare ();
  exec_until (ttt, p);
  env->read_env (cur (p));
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
    return text_language (get_env_string (TEXT_LANGUAGE));
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
      return tree (HYPERLINK, copy (h[ref][which]), "#" * ref);
    }
    return tree (HYPERLINK, "?", "#" * ref);
  }
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= expand_references (t[i], h);
  return r;  
}

tree
edit_typeset_rep::exec (tree t, hashmap<string,tree> H) {
  hashmap<string,tree> H2;
  env->read_env (H2);
  env->write_env (H);
  t= env->exec (t);
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
edit_typeset_rep::exec_html (tree t, path p) {
  if (p == 0) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(object->tree (tmhtml-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  return exec (t, H);
}

/******************************************************************************
* Initialization
******************************************************************************/

void
edit_typeset_rep::init_style () {
  bool old_need_save    = buf->need_save;
  bool old_need_autosave= buf->need_autosave;
  path old_tp= tp;
  ::notify_assign (ttt, path(), et);
  tp= old_tp;
  notify_change (THE_ENVIRONMENT);
  buf->need_save    = old_need_save;
  buf->need_autosave= old_need_autosave;
}

void
edit_typeset_rep::init_style (string name) {
  if ((name == "none") || (name == "") || (name == "style")) the_style= TUPLE;
  else the_style= tree (TUPLE, name);
  path old_tp= tp;
  ::notify_assign (ttt, path(), et);
  tp= old_tp;
  notify_change (THE_ENVIRONMENT);
  buf->need_save= buf->need_autosave= true;
}

void
edit_typeset_rep::init_extra_style (string name, bool check) {
  if (check) {
    int i, n= N(the_style);
    for (i=0; i<n; i++)
      if (the_style[i] == name)
	return;
  }

  the_style << tree (name);
  path old_tp= tp;
  ::notify_assign (ttt, path(), et);
  tp= old_tp;
  notify_change (THE_ENVIRONMENT);
  buf->need_save= buf->need_autosave= true;
}

void
edit_typeset_rep::init_env (string var, tree by) {
  if (init (var) == by) return;
  init (var)= by;
  path old_tp= tp;
  ::notify_assign (ttt, path(), et);
  tp= old_tp;
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_default (string var) {
  if (!init->contains (var)) return;
  init->reset (var);
  path old_tp= tp;
  ::notify_assign (ttt, path(), et);
  tp= old_tp;
  notify_change (THE_ENVIRONMENT);
}

/******************************************************************************
* Actual typesetting
******************************************************************************/

void
edit_typeset_rep::typeset (SI& x1, SI& y1, SI& x2, SI& y2) {
  typeset_prepare ();
  eb= empty_box (path ());
  // saves memory, also necessary for change_log update
  eb= ::typeset (ttt, x1, y1, x2, y2);
}

void
edit_typeset_rep::typeset_invalidate_all () {
  ::notify_assign (ttt, path(), et);
  notify_change (THE_ENVIRONMENT);
  typeset_preamble ();
}
