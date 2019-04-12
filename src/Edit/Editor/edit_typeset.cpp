
/******************************************************************************
* MODULE     : typeset.cpp
* DESCRIPTION: typeset the tree being edited
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <climits>
#include "edit_typeset.hpp"
#include "tm_buffer.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "tm_timer.hpp"
#include "Bridge/impl_typesetter.hpp"
#include "new_style.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Environment/std_environment.hpp"
#endif // EXPERIMENTAL

//box empty_box (path ip, int x1=0, int y1=0, int x2=0, int y2=0);
bool enable_fastenv= false;

/******************************************************************************
* Contructors, destructors and notification of modifications
******************************************************************************/

edit_typeset_rep::edit_typeset_rep ():
  editor_rep (), // NOTE: ignored by the compiler, but suppresses warning
  the_style (TUPLE),
  cur (hashmap<string,tree> (UNINIT)),
  stydef (UNINIT), pre (UNINIT), init (UNINIT), fin (UNINIT), grefs (UNINIT),
  env (drd, buf->buf->master,
       buf->data->ref, (buf->prj==NULL? grefs: buf->prj->data->ref),
       buf->data->aux, (buf->prj==NULL? buf->data->aux: buf->prj->data->aux),
       buf->data->att, (buf->prj==NULL? buf->data->att: buf->prj->data->att)),
  ttt (new_typesetter (env, subtree (et, rp), reverse (rp))) {
    init_update ();
}

edit_typeset_rep::~edit_typeset_rep () { delete_typesetter (ttt); }

void
edit_typeset_rep::set_data (new_data data) {
  set_style (data->style);
  set_init  (data->init);
  set_fin   (data->fin);
  set_att   (data->att);
  notify_page_change ();
  add_init (data->init);
  notify_change (THE_DECORATIONS);
  typeset_invalidate_env ();
  iterator<string> it = iterate (data->att);
  while (it->busy()) {
    string key= it->next ();
    (void) call (string ("notify-set-attachment"),
                 buf->buf->name, key, data->att [key]);
  }
}

void
edit_typeset_rep::get_data (new_data& data) {
  data->style= get_style ();
  data->init = get_init ();
  data->fin  = get_fin ();
  data->att  = get_att ();
}

typesetter edit_typeset_rep::get_typesetter () { return ttt; }
tree edit_typeset_rep::get_style () { return the_style; }
void edit_typeset_rep::set_style (tree t) { the_style= copy (t); }
hashmap<string,tree> edit_typeset_rep::get_init () { return init; }
hashmap<string,tree> edit_typeset_rep::get_fin () { return fin; }
hashmap<string,tree> edit_typeset_rep::get_att () { return buf->data->att; }
void edit_typeset_rep::set_fin (hashmap<string,tree> H) { fin= H; }
void edit_typeset_rep::set_att (hashmap<string,tree> H) { buf->data->att= H; }

tree
edit_typeset_rep::get_att (string key) {
  return buf->data->att[key];
}

void
edit_typeset_rep::set_att (string key, tree im) {
  buf->data->att (key)= im;
}

void
edit_typeset_rep::reset_att (string key) {
  buf->data->att->reset (key);
}

array<string>
edit_typeset_rep::list_atts () {
  tree a= (tree) buf->data->att;
  array<string> v;
  int i, n= N(a);
  for (i=0; i<n; i++)
    v << a[i][0]->label;
  merge_sort (v);
  return v;
}

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
edit_typeset_rep::clear_local_info () {
  buf->data->ref= hashmap<string,tree> ();
  buf->data->aux= hashmap<string,tree> ();
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
  style= preprocess_style (style, buf->buf->master);
  //cout << "Typesetting style using cache " << style << LF;
  bool ok;
  hashmap<string,tree> H;
  tree t;
  style_get_cache (style, H, t, ok);
  if (ok) {
    env->patch_env (H);
    ok= drd->set_locals (t);
    drd->set_environment (H);
  }
  if (!ok) {
    //cout << "Typeset without cache " << style << LF;
    if (!is_tuple (style)) FAILED ("tuple expected as style");
    H= get_style_env (style);
    drd= get_style_drd (style);
    style_set_cache (style, H, drd->get_locals ());
    env->patch_env (H);
    drd->set_environment (H);
  }
  use_modules (env->read (THE_MODULES));
}

void
edit_typeset_rep::typeset_preamble () {
  env->write_default_env ();
  typeset_style_use_cache (the_style);
  env->update ();
  env->read_env (stydef);
  env->patch_env (init);
  env->update ();
  env->read_env (pre);
  drd->heuristic_init (pre);
}

void
edit_typeset_rep::typeset_prepare () {
  env->base_file_name= buf->buf->master;
  env->read_only= buf->buf->read_only;
  env->write_default_env ();
  env->patch_env (pre);
  env->style_init_env ();
  env->update ();
}

void
edit_typeset_rep::init_update () {
  if (buf->prj != NULL) {
    string id= as_string (delta (buf->prj->buf->name, buf->buf->name));
    string lab= "part:" * id;
    hashmap<string,tree> aux= env->global_aux;
    hashmap<string,tree> ref= env->global_ref;
    if (aux->contains ("parts")) {
      tree parts= aux ["parts"];
      if (is_func (parts, DOCUMENT))
        for (int i=0; i<N(parts); i++)
          if (is_tuple (parts[i]) && N(parts[i]) >= 1)
            if (parts[i][0] == id)
              for (int j=1; j+1 < N(parts[i]); j+=2)
                if (is_atomic (parts[i][j])) {
                  buf->data->init (parts[i][j]->label)= copy (parts[i][j+1]);
                  init (parts[i][j]->label)= copy (parts[i][j+1]);
                }      
    }
    if (ref->contains (lab)) {
      tree val= ref [lab];
      if (is_tuple (val) && N(val) >= 2 && val[1] != tree (UNINIT)) {
        buf->data->init (PAGE_FIRST)= copy (val[1]);
        init (PAGE_FIRST)= copy (val[1]);
      }
    }
  }
  else if (buf->data->init ["part-flag"] == "true")
    grefs= copy (buf->data->ref);
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

static void
restricted_exec (edit_env env, tree t, int end) {
  if (is_func (t, ASSIGN, 2) && end == 2)
    env->exec (t);
  else if (is_document (t) || is_concat (t))
    for (int i=0; i < min (end, 10); i++)
      restricted_exec (env, t[i], arity (t[i]));
  else if (is_compound (t, "hide-preamble", 1) ||
           is_compound (t, "show-preamble", 1))
    env->exec (t[0]);
}

static tree
filter_format (tree fm, int i, int n) {
  array<tree> r;
  for (int k=0; k<N(fm); k++)
    if (is_func (fm[k], CWITH) && N(fm[k]) >= 4 &&
        is_int (fm[k][0]) && is_int (fm[k][1])) {
      int j1= as_int (fm[k][0]->label);
      int j2= as_int (fm[k][1]->label);
      if (j1 > 0) j1--; else j1 += n;
      if (j2 > 0) j2--; else j2 += n;
      if (i >= j1 && i <= j2) r << fm[k] (2, N(fm[k]));
    }
  return tree (TFORMAT, r);
}

static void
table_descend (tree& t, path& p, tree& fm) {
  while (!is_nil (p)) {
    if (L(t) == TFORMAT && p->item == N(t) - 1) {
      array<tree> r;
      for (int k=0; k<N(t)-1; k++)
        if (is_func (t[k], CWITH, 6) &&
            is_atomic (t[k][4]) &&
            !starts (t[k][4]->label, "cell-"))
          r << t[k];
      fm= tree (TFORMAT, r);
      t= t[N(t)-1];
      p= p->next;
    }
    else if ((L(t) == TABLE || L(t) == ROW) &&
             p->item >= 0 && p->item < N(t)) {
      fm= filter_format (fm, p->item, N(t));
      t= t[p->item];
      p= p->next;
    }
    else break;
  }
}

void
edit_typeset_rep::typeset_exec_until (path p) {
  //time_t t1= texmacs_time ();
  if (has_changed (THE_TREE + THE_ENVIRONMENT))
    if (p != correct_cursor (et, rp * 0)) {
      if (DEBUG_STD) std_warning << "resynchronizing for path " << p << "\n";
      // apply_changes ();
    }
  if (p == tp && inside_graphics (true) && p != closest_inside (et, p)) {
    //cout << "TeXmacs] Warning: corrected cursor\n";
    tp= closest_inside (et, tp);
    p = tp;
  }

  //cout << "Exec until " << p << LF;
  if (N(cur[p])!=0) return;
  if (N(cur)>=25) // avoids out of memory in weird cases
    typeset_invalidate_env ();
  typeset_prepare ();
  if (enable_fastenv) {
    if (!(rp < p)) {
      failed_error << "Erroneous path " << p << "\n";
      FAILED ("invalid typesetting path");
    }
    tree t= subtree (et, rp);
    path q= path_up (p / rp);
    while (!is_nil (q)) {
      int i= q->item;
      restricted_exec (env, t, i);
      if (L(t) == TFORMAT && i == N(t) - 1) {
        tree fm= tree (TFORMAT);
        table_descend (t, q, fm);
        if (!is_nil (q))
          for (int k=0; k<N(fm); k++)
            if (is_func (fm[k], CWITH, 2))
              env->write (fm[k][0]->label, fm[k][1]);
      }
      else {
        tree w= drd->get_env_child (t, i, tree (ATTR));
        if (w == "") break;
        //cout << "t= " << t << "\n";
        //cout << "i= " << i << "\n";
        //cout << "w= " << w << "\n";
        tree ww (w, N(w));
        for (int j=0; j<N(w); j+=2) {
          //cout << w[j] << " := " << env->exec (w[j+1]) << "\n";
          ww[j+1]= env->exec (w[j+1]);
        }
        for (int j=0; j<N(w); j+=2)
          env->write (w[j]->label, ww[j+1]);
        t= t[i];
        q= q->next;
      }
    }
    if (env->read (PREAMBLE) == "true")
      env->write (MODE, "src");
  }
  else exec_until (ttt, p / rp);
  env->read_env (cur (p));
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset_exec_until took " << t2-t1 << "ms\n";
}

tree
edit_typeset_rep::get_full_env () {
  typeset_exec_until (tp);
  return (tree) cur[tp];
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
    if (var == BG_COLOR && is_func (t, PATTERN)) t= env->exec (t);
    return is_func (t, BACKUP, 2)? t[0]: t;
  }
  if (N(pre)==0) typeset_preamble ();
  tree t= pre [var];
  if (var == BG_COLOR && is_func (t, PATTERN)) t= env->exec (t);
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

int
edit_typeset_rep::get_page_count () {
  return N (eb[0]);
}

SI
edit_typeset_rep::get_page_width (bool deco) {
  (void) get_env_string (PAGE_WIDTH);
  return (env->get_page_width (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_pages_width (bool deco) {
  (void) get_env_string (PAGE_WIDTH);
  return (env->get_pages_width (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_page_height (bool deco) {
  (void) get_env_string (PAGE_HEIGHT);
  return (env->get_page_height (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_total_width (bool deco) {
  SI w= eb->w();
  if (!deco) {
    SI w1= env->get_pages_width (false);
    SI w2= env->get_pages_width (true);
    w -= (w2 - w1);
  }
  return (w + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_total_height (bool deco) {
  SI h= eb->h();
  if (!deco) {
    SI h1= env->get_page_height (false);
    SI h2= env->get_page_height (true);
    int nr= get_page_count ();
    int nx= env->page_packet;
    int ny= ((nr + env->page_offset) + nx - 1) / nx;
    h -= ny * (h2 - h1);
  }
  return (h + std_shrinkf - 1) / std_shrinkf;
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
    t= expand_references (t, buf->data->ref);
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
  t= convert_OTS1_symbols_to_universal_encoding (t);
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
  t= convert_OTS1_symbols_to_universal_encoding (t);
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(stree->tree (tmhtml-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  tree w (WITH);
  if (H->contains ("html-title"))
    w << string ("html-title") << H["html-title"];
  if (H->contains ("html-css"))
    w << string ("html-css") << H["html-css"];
  if (H->contains ("html-head-javascript"))
    w << string ("html-head-javascript") << H["html-head-javascript"];
  if (H->contains ("html-head-javascript-src"))
    w << string ("html-head-javascript-src") << H["html-head-javascript-src"];
  if (N(w) == 0) return exec (t, H);
  else {
    w << t;
    return exec (w, H);
  }
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
  t= convert_OTS1_symbols_to_universal_encoding (t);
  bool expand_unknown_macros= "on" == as_string (
      call ("get-preference", "texmacs->latex:expand-macros"));
  bool expand_user_macro= "on" == as_string (
      call ("get-preference", "texmacs->latex:expand-user-macros"));
  if (!expand_unknown_macros && !expand_user_macro)
    return t;
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  object l= null_object ();
  iterator<string> it= iterate (H);
  while (it->busy ()) l= cons (object (it->next ()), l);
  tree patch= as_tree (call ("stree->tree", call ("tmtex-env-patch", t, l)));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);

  if (!expand_user_macro &&
      is_document (t) && is_compound (t[0], "hide-preamble")) {
    tree r= copy (t);
    r[0]= "";
    r= exec (value_to_compound (r, P), H, false);
    r[0]= exec (t[0], H, false);
    return r;
  }
  else {
    tree r= exec (value_to_compound (t, P), H, false);
    return r;
  }
}

tree
edit_typeset_rep::exec_latex (tree t) {
  return exec_latex (t, rp * 0);
}

tree
edit_typeset_rep::texmacs_exec (tree t) {
  return ::texmacs_exec (env, t);
}

tree
edit_typeset_rep::var_texmacs_exec (tree t) {
  typeset_exec_until (tp);
  env->write_env (cur[tp]);
  env->update_frame ();
  return texmacs_exec (t);
}

/******************************************************************************
* Wrappers for editing animations
******************************************************************************/

tree
edit_typeset_rep::checkout_animation (tree t) {
  path p= search_upwards (ANIM_STATIC);
  if (is_nil (p)) p= search_upwards (ANIM_DYNAMIC);
  if (is_nil (p)) p= search_upwards ("anim-edit");
  if (!is_nil (p)) {
    typeset_exec_until (p);
    env->write_env (cur[p]);
  }
  return env->checkout_animation (t);
}

tree
edit_typeset_rep::commit_animation (tree t) {
  path p= search_upwards ("anim-edit");
  if (is_nil (p)) p= search_upwards (ANIM_STATIC);
  if (is_nil (p)) p= search_upwards (ANIM_DYNAMIC);
  if (!is_nil (p)) {
    typeset_exec_until (p);
    env->write_env (cur[p]);
  }
  return env->commit_animation (t);
}

/******************************************************************************
* Initialization
******************************************************************************/

void
edit_typeset_rep::change_style (tree t) {
  bool changed= (the_style != t);
  the_style= copy (t);
  if (changed) {
    require_save ();
    notify_change (THE_ENVIRONMENT);
  }
}

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

tree
edit_typeset_rep::get_init_all () {
  return (tree) init;
}

void
edit_typeset_rep::init_env (string var, tree by) {
  if (init (var) == by) return;
  init (var)= by;
  if (var != PAGE_SCREEN_WIDTH &&
      var != PAGE_SCREEN_HEIGHT &&
      var != ZOOM_FACTOR)
    require_save ();
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_default (string var) {
  if (!init->contains (var)) return;
  init->reset (var);
  if (stydef->contains (var)) pre(var)= stydef[var];
  else pre->reset (var);
  notify_change (THE_ENVIRONMENT);
}

/******************************************************************************
* Actual typesetting
******************************************************************************/

void
edit_typeset_rep::typeset_sub (SI& x1, SI& y1, SI& x2, SI& y2) {
  //time_t t1= texmacs_time ();
  typeset_prepare ();
  eb= empty_box (reverse (rp));
  // saves memory, also necessary for change_log update
  bench_start ("typeset");
#ifdef USE_EXCEPTIONS
  try {
#endif
    eb= ::typeset (ttt, x1, y1, x2, y2);
#ifdef USE_EXCEPTIONS
  }
  catch (string msg) {
    the_exception= msg;
    std_error << "Typesetting failure, resetting to empty document\n";
    assign (rp, tree (DOCUMENT, ""));
    ::notify_assign (ttt, path(), subtree (et, rp));
    eb= ::typeset (ttt, x1, y1, x2, y2);    
  }
  handle_exceptions ();
#endif
  bench_end ("typeset");
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset took " << t2-t1 << "ms\n";
  picture_cache_clean ();
}

static void
report_missing (hashmap<string,tree> missing) {
  array<string> a;
  for (iterator<string> it= iterate (missing); it->busy(); a << it->next ()) {}
  merge_sort (a);
  for (int i=0; i<N(a); i++)
    if (!starts (a[i], "bib-"))
      typeset_warning << "Undefined reference " << a[i] << LF;
}

static void
report_redefined (array<tree> redefined) {
  for (int i=0; i<N(redefined); i++) {
    tree t= redefined[i];
    if (t[1]->label == "")
      typeset_warning << "Redefined " << t[0]->label << LF;
    else
      typeset_warning << "Redefined " << t[0]->label
                      << " as " << t[1]->label << LF;
  }
}

static void
clean_unused (hashmap<string,tree>& refs, hashmap<string,bool> used) {
  array<string> a;
  for (iterator<string> it= iterate (refs); it->busy(); ) {
    string key= it->next ();
    if (!used->contains (key)) a << key;
  }
  for (int i=0; i<N(a); i++)
    refs->reset (a[i]);
}

void
edit_typeset_rep::typeset (SI& x1, SI& y1, SI& x2, SI& y2) {
  int missing_nr= INT_MAX;
  int redefined_nr= INT_MAX;
  while (true) {
    typeset_sub (x1, y1, x2, y2);
    if (!env->complete) break;
    env->complete= false;
    clean_unused (env->local_ref, env->touched);
    if (N(env->missing) == 0 && N(env->redefined) == 0) break;
    if ((N(env->missing) == missing_nr && N(env->redefined) == redefined_nr) ||
        (N(env->missing) > missing_nr || N(env->redefined) > redefined_nr)) {
      report_missing (env->missing);
      report_redefined (env->redefined);
      break;
    }
    missing_nr= N(env->missing);
    redefined_nr= N(env->redefined);
    ::notify_assign (ttt, path(), ttt->br->st);
  }
}

void
edit_typeset_rep::typeset_forced () {
  //cout << "Typeset forced\n";
  SI x1, y1, x2, y2;
  typeset (x1, y1, x2, y2);
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

void
edit_typeset_rep::typeset_invalidate_players (path p, bool reattach) {
  if (rp <= p) {
    tree t= subtree (et, p);
    blackbox bb;
    bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
    if (ok) {
      if (reattach) tree_addendum_delete (t, ADDENDUM_PLAYER);
      typeset_invalidate (p);
    }
    if (is_compound (t)) {
      int i, n= N(t);
      for (i=0; i<n; i++)
        typeset_invalidate_players (p * i, reattach);
    }
  }
}
