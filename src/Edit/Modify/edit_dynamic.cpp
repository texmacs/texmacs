
/******************************************************************************
* MODULE     : edit_dynamic.cpp
* DESCRIPTION: editing dynamic content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_dynamic.hpp"
#include "tree_analyze.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_dynamic_rep::edit_dynamic_rep () {}
edit_dynamic_rep::~edit_dynamic_rep () {}

/******************************************************************************
* Subroutines for inactive content
******************************************************************************/

static bool env_locked   = false;
static bool env_in_source= false;

bool
edit_dynamic_rep::in_source () {
  // FIXME: we use a very dirty trick to "lock the environment",
  // so that no new look-ups are made during modifications of et or tp.
  // In fact, we would need to retypeset the document in order to
  // get a correct value.
  if (!env_locked)
    env_in_source= get_env_string (MODE) == "src";
  return env_in_source;
}

path
edit_dynamic_rep::find_dynamic (path p) {
  path parent= path_up (p);
  if (!(rp < parent)) return path ();
  if (drd->is_dynamic (subtree (et, parent))) return p;
  return find_dynamic (parent);
}

/******************************************************************************
* Making general compound objects
******************************************************************************/

bool
edit_dynamic_rep::is_multi_paragraph_macro (tree t, bool pure) {
  int n= arity (t);
  if (is_document (t) || is_func (t, PARA) || is_func (t, SURROUND))
    return true;
  if (is_func (t, MACRO) || is_func (t, WITH) ||
      is_func (t, LOCUS) ||
      is_func (t, CANVAS) || is_func (t, ORNAMENT) || is_func (t, ART_BOX))
    return is_multi_paragraph_macro (t [n-1], pure);
  if (is_extension (t) &&
      !is_compound (t, "footnote") &&
      !is_compound (t, "footnote-anchor") &&
      !is_compound (t, "note-footnote") &&
      !is_compound (t, "note-footnote*")) {
    int i;
    for (i=1; i<n; i++)
      if (is_multi_paragraph_macro (t[i], pure))
        return true;
    tree f= get_env_value (as_string (L(t)));
    return is_multi_paragraph_macro (f, pure);
  }
  if (!pure)
    if (is_func (t, ARG))
      return true;
  return false;
}

static bool
contains_table_format (tree t, tree var) {
  // FIXME: this should go into the DRD
  if (is_atomic (t)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (contains_table_format (t[i], var))
        return true;
    return is_func (t, TFORMAT) && (t[N(t)-1] == tree (ARG, var));
  }
}

void
edit_dynamic_rep::make_compound (tree_label l, int n= -1) {
  //cout << "Make compound " << as_string (l) << ", " << n << "\n";
  eval ("(use-modules (generic generic-edit))");
  if (n == -1) {
    for (n=0; true; n++) {
      if (drd->correct_arity (l, n) &&
          ((n>0) || (drd->get_arity_mode (l) == ARITY_NORMAL))) break;
      if (n == 100) return;
    }
  }

  tree t (l, n);
  path p (0, 0);
  int  acc=0;
  for (; acc<n; acc++)
    if (drd->is_accessible_child (t, acc))
      break;
  if (acc<n) p->item= acc;
  if (n == 0) insert_tree (t, 1);
  else if (is_with_like (t) && as_bool (call ("with-like-check-insert", t)));
  else {
    string s= as_string (l);
    tree f= get_env_value (s);
    bool block_macro= (N(f) == 2) && is_multi_paragraph_macro (f, true);
    bool large_macro= (N(f) == 2) && is_multi_paragraph_macro (f, false);
    bool table_macro= (N(f) == 2) && contains_table_format (f[1], f[0]);
    // FIXME: why do we take the precaution N(f) == 2 ?
    if (s == "explain") block_macro= true;

    tree sel= "";
    if (selection_active_small () ||
        (large_macro && selection_active_normal ()))
      sel= selection_get_cut ();
    else if (is_with_like (t) && selection_active_normal ()) {
      sel= selection_get_cut ();
      t[n-1]= sel;
      insert_tree (t, p);
      return;
    }
    if ((block_macro && (!table_macro)) ||
        (l == make_tree_label ("footnote")) ||
        (l == make_tree_label ("footnote-anchor")) ||
        (l == make_tree_label ("note-footnote")) ||
        (l == make_tree_label ("note-footnote*")))
      {
        t[0]= tree (DOCUMENT, "");
        p   = path (0, 0, 0);
      }
    if (!drd->all_accessible (l))
      if (get_init_string (MODE) != "src" && !inside ("show-preamble")) {
        t= tree (INACTIVE, t);
        p= path (0, p);
      }
    insert_tree (t, p);
    if (table_macro) make_table (1, 1);
    if (sel != "") insert_tree (sel, end (sel));
    
    tree mess= concat ();
    if (drd->get_arity_mode (l) != ARITY_NORMAL)
      mess= concat (kbd ("A-right"), ": insert argument");
    if (!drd->all_accessible (l)) {
      if (mess != "") mess << ", ";
      mess << kbd ("return") << ": activate";
    }
    if (mess == concat ()) mess= "Move to the right when finished";
    set_message (mess, drd->get_name (l));
  }
}

void
edit_dynamic_rep::activate () {
  path p= search_upwards (INACTIVE);
  if (is_nil (p)) return;
  tree st= subtree (et, p * 0);

  if (is_func (st, COMPOUND) && is_atomic (st[0])) {
    tree u (make_tree_label (st[0]->label));
    u << A (st (1, N(st)));
    assign (p, u);
    call ("notify-activated", object (subtree (et, p)));
    go_to (end (et, p));
    correct (path_up (p));
  }
  else {
    bool acc= (p < path_up (tp) && drd->is_accessible_child (st, tp[N(p)]));
    remove_node (p * 0);
    call ("notify-activated", object (subtree (et, p)));
    if (!acc) go_to (end (et, p));
    correct (path_up (p));
  }
}

/******************************************************************************
* Inserting and removing arguments
******************************************************************************/

void
edit_dynamic_rep::go_to_argument (path p, bool start_flag) {
  tree t= subtree (et, path_up (p));
  bool inactive= is_func (subtree (et, path_up (p, 2)), INACTIVE);
  int i= last_item (p), n= N(t);
  if (i < 0) go_to_start (path_up (p, inactive? 2: 1));
  else if (i >= n) go_to_end (path_up (p, inactive? 2: 1));
  else {
    if ((!drd->is_accessible_child (t, i)) &&
        (!inactive) && (!in_source ()))
      {
        insert_node (path_up (p) * 0, INACTIVE);
        call ("notify-disactivated", object (subtree (et, path_up (p) * 0)));
        p= path_up (p) * path (0, i);
      }
    if (start_flag) go_to_start (p);
    else go_to_end (p);
  }
}

void
edit_dynamic_rep::insert_argument (path p, bool forward) {
  tree t= subtree (et, path_up (p));
  int i= last_item (p), n= N(t), d= 1;
  if (is_func (t, WITH) ||
      is_func (t, STYLE_WITH) ||
      is_func (t, VAR_STYLE_WITH))
    if (i == n-1) i--;
  if ((!in_source ()) || drd->contains (as_string (L(t)))) {
    if (forward) do i++; while ((i<=n) && (!drd->insert_point (L(t), i, n)));
    else while ((i>=0) && (!drd->insert_point (L(t), i, n))) i--;
    if ((i<0) || (i>n)) return;
    while (!drd->correct_arity (L(t), n+d)) d++;
  }
  else if (forward) i++;
  path q= path_up (p) * i;
  tree ins (L(t), d);
  insert (q, ins);
  go_to_argument (q, forward);
}

void
edit_dynamic_rep::insert_argument (bool forward) {
  path p= find_dynamic (tp);
  if (is_nil (p)) return;
  if (p == tp) p= find_dynamic (path_up (tp));
  if (is_nil (p)) return;
  insert_argument (p, forward);
}

void
edit_dynamic_rep::remove_empty_argument (path p, bool forward) {
  tree t= subtree (et, path_up (p));
  int i= last_item (p), j, d, n= N(t);
  bool src_flag= in_source () && (!drd->contains (as_string (L(t))));

  for (d=1; d<=n-i; d++)
    if ((src_flag && (d==1)) ||
        ((!src_flag) &&
         drd->correct_arity (L(t), n-d) &&
         drd->insert_point (L(t), i, n-d)))
      {
        bool flag= true;
        for (j=0; j<d; j++)
          flag= flag && is_empty (t[i+j]);
        if (flag) {
          bool old_locked= env_locked; env_locked= true;
          remove (p, d);
          if ((d == n) && is_mod_active_once (subtree (et, path_up (p, 2)))) {
            remove_node (path_up (p, 2) * 0);
            go_to_border (path_up (p, 2), forward);
          }
          else if (forward) go_to_argument (path_up (p) * i, true);
          else go_to_argument (path_up (p) * (i-1), false);
          env_locked= old_locked;
          return;
        }
        else break;
      }

  bool flag= true;
  for (j=0; j<n; j++)
    flag= flag && is_empty (t[j]);
  if (flag) {
    assign (path_up (p), "");
    tree st= subtree (et, path_up (p, 2));
    if ((is_mod_active_once (st) || is_compound (st, "doc-inactive")) &&
        (st[0] == "")) {
      assign (path_up (p, 2), "");
      correct (path_up (p, 3));
    }
    // FIXME: temporary hack for doc-data and doc-author-data
    else if (is_compound (st, "doc-data")      ||
             is_compound (st, "abstract-data") ||
             is_compound (st, "doc-author")    ||
             is_compound (st, "author-data")) {
      if (N(st)==1) {
        assign (path_up (p, 2), "");
        correct (path_up (p, 3));
      }
      else {
        int i= last_item (path_up (p)) + (forward? 0: -1);
        remove (path_up (p), 1);
        if (i<0) go_to_start (path_up (p, 2));
        else go_to_border (path_up (p, 2) * i, forward);
      }
    }
    else correct (path_up (p, 2));
    return;
  }

  if (forward) go_to_argument (path_up (p) * (i+1), true);
  else go_to_argument (path_up (p) * (i-1), false);
}

void
edit_dynamic_rep::remove_argument (path p, bool forward) {
  tree t= subtree (et, path_up (p));
  int i= last_item (p), n= N(t), d= 1;
  if ((!in_source ()) || drd->contains (as_string (L(t)))) {
    if (forward) do i++; while (i<=n && !drd->insert_point (L(t), i, n));
    else while (i>=0 && !drd->insert_point (L(t), i, n)) i--;
    if ((i<0) || (i>n)) return;
    while (i>=d && !drd->correct_arity (L(t), n-d)) d++;
    if (i<d || n<=d || !drd->insert_point (L(t), i-d, n-d)) return;
  }
  else {
    if (forward) i++;
    if (i<d || n<=d) return;
  }
  path q= path_up (p) * (i-d);
  remove (q, d);
  go_to_argument (q, forward);
}

void
edit_dynamic_rep::remove_argument (bool forward) {
  path p= find_dynamic (tp);
  if (is_nil (p)) return;
  if (p == tp) p= find_dynamic (path_up (tp));
  if (is_nil (p)) return;
  remove_argument (p, forward);
}

/******************************************************************************
* Backspace and delete
******************************************************************************/

void
edit_dynamic_rep::back_monolithic (path p) {
  if (!is_concat (subtree (et, path_up (p)))) assign (p, "");
  else remove (p, 1);
  correct (path_up (p));
}

void
edit_dynamic_rep::back_general (path p, bool forward) {
  tree st= subtree (et, p);
  int n= N(st);
  if ((L(st) >= START_EXTENSIONS) && in_source () && (forward || (n == 0))) {
    tree u (COMPOUND, copy (as_string (L(st))));
    u << copy (A (st));
    assign (p, u);
    go_to_border (p * 0, forward);
  }
  else if (n==0) back_monolithic (p);
  else if ((n==1) && is_func (st[0], DOCUMENT, 1) &&
           (is_func (st[0][0], TFORMAT) || is_func (st[0][0], TABLE)))
    back_table (p * path (0, 0), forward);
  else if ((n==1) && (is_func (st[0], TFORMAT) || is_func (st[0], TABLE)))
    back_table (p * 0, forward);
  else go_to_argument (p * (forward? 0: n-1), forward);
}

void
edit_dynamic_rep::back_in_general (tree t, path p, bool forward) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_source ())
    if ((L(t) >= START_EXTENSIONS) && (last_item (p) == 0) && (!forward)) {
      bool src_flag= in_source () && (!drd->contains (as_string (L(t))));
      tree u (COMPOUND, copy (as_string (L(t))));
      if (is_empty (t[0]) && src_flag) u << A (copy (t (1, N(t))));
      else u << A (copy (t));
      assign (path_up (p), u);
      go_to_end (p);
      return;
    }
  remove_empty_argument (p, forward);
}

/******************************************************************************
* The WITH tag
******************************************************************************/

static tree
remove_changes_in (tree t, string var) {
  if (is_atomic (t)) return t;
  else if (is_func (t, WITH)) {
    int i, n=N(t), k=(n-1)>>1;
    if (k==1) {
      tree r= remove_changes_in (t[2], var);
      if (t[0] != var) r= tree (WITH, t[0], t[1], r);
      return simplify_correct (r);
    }
    tree r (WITH);
    for (i=0; i<k; i++)
      if (t[i<<1] != var) r << t[i<<1] << t[(i<<1)+1];
    r << remove_changes_in (t[i<<1], var);
    return simplify_correct (r);
  }
  else if (is_format (t) || is_func (t, SURROUND)) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= remove_changes_in (t[i], var);
    return simplify_correct (r);
  }
  else return t;
}

void
edit_dynamic_rep::make_with (string var, string val) {
  if (selection_active_normal ()) {
    tree t= remove_changes_in (selection_get (), var);
    selection_cut ();
    insert_tree (tree (WITH, var, val, t), path (2, end (t)));
  }
  else insert_tree (tree (WITH, var, val, ""), path (2, 0));
}

void
edit_dynamic_rep::insert_with (path p, string var, tree val) {
  tree st= subtree (et, p);
  if (is_func (st, WITH)) {
    int i, n= N(st)-1;
    for (i=0; i<n; i+=2)
      if (st[i] == var) {
        assign (p * (i+1), copy (val));
        return;
      }
    insert (p * n, copy (tree (WITH, var, val)));    
  }
  else if ((rp < p) && is_func (subtree (et, path_up (p)), WITH))
    insert_with (path_up (p), var, val);
  else insert_node (p * 2, copy (tree (WITH, var, val)));
}

void
edit_dynamic_rep::remove_with (path p, string var) {
  tree st= subtree (et, p);
  if (is_func (st, WITH)) {
    int i, n= N(st)-1;
    for (i=0; i<n; i+=2)
      if (st[i] == var) {
        remove (p * i, 2);
        if (n == 2) remove_node (p * 0);
        return;
      }
  }
  else if ((rp < p) && is_func (subtree (et, path_up (p)), WITH))
    remove_with (path_up (p), var);
}

void
edit_dynamic_rep::back_in_with (tree t, path p, bool forward) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) ||
      ((is_func (t, WITH) || is_func (t, LOCUS)) && in_source ()))
    back_in_general (t, p, forward);
  else if (t[N(t)-1] == "") {
    assign (path_up (p), "");
    correct (path_up (p, 2));
  }
  else go_to_border (path_up (p), !forward);
}

/******************************************************************************
* Style file editing
******************************************************************************/

void
edit_dynamic_rep::make_mod_active (tree_label l) {
  if (selection_active_normal ()) {
    tree t= selection_get ();
    selection_cut ();
    insert_tree (tree (l, t), path (0, end (t)));
  }
  else if ((l == VAR_STYLE_ONLY) || (l == VAR_ACTIVE) || (l == VAR_INACTIVE))
    insert_tree (tree (l, ""), path (0, 0));
  else {
    path p= path_up (tp);
    if (is_atomic (subtree (et, p))) p= path_up (p);
    if (rp < p) insert_node (p * 0, l);
  }
}

void
edit_dynamic_rep::insert_style_with (path p, string var, string val) {
  if (!(rp < p)) return;
  tree st= subtree (et, path_up (p));
  if (is_func (st, STYLE_WITH)) {
    int i, n= N(st);
    for (i=n-1; i>=0; i-=2)
      if (st[i] == var) {
        assign (path_up (p) * (i+1), copy (val));
        return;
      }
    insert (path_up (p) * (n-1), tree (STYLE_WITH, copy (var), copy (val)));
  }
  else insert_node (p * 2, copy (tree (STYLE_WITH, var, val)));
}

void
edit_dynamic_rep::make_style_with (string var, string val) {
  if (selection_active_normal ()) {
    tree t= selection_get ();
    selection_cut ();
    if (subtree (et, path_up (tp)) == "") {
      insert_style_with (path_up (tp), var, val);
      insert_tree (t);
    }
    else insert_tree (tree (STYLE_WITH, var, val, t), path (2, end (t)));
  }
  else insert_style_with (path_up (tp), var, val);
}

/******************************************************************************
* The HYBRID and LATEX tags
******************************************************************************/

void
edit_dynamic_rep::make_hybrid () {
  tree t (HYBRID, "");
  if (selection_active_small ())
    t[0]= selection_get_cut ();
  if (is_func (t, HYBRID, 1) && (t[0] != "") &&
      (!(is_atomic (t[0]) && drd->contains (t[0]->label))))
    t= tree (HYBRID, "", t[0]);
  path p= end (t, path (0));
  if (in_source ()) insert_tree (t, p);
  else insert_tree (tree (INACTIVE, t), path (0, p));
  set_message (concat (kbd ("return"), ": activate symbol or macro"),
               "hybrid");
}

bool
edit_dynamic_rep::activate_latex () {
  path p= search_upwards (LATEX);
  if (is_nil (p)) p= search_upwards (HYBRID);
  if (is_nil (p)) return false;
  tree st= subtree (et, p);
  if (is_atomic (st[0])) {
    if (is_func (subtree (et, path_up (p)), INACTIVE))
      p= path_up (p);
    string  s= st[0]->label, help;
    command cmd;
    if (kbd_get_command (s, help, cmd)) {
      cut (p * 0, p * 1);
      cmd ();
      if (N(st) == 2) insert_tree (copy (st[1]));
      return true;
    }
    set_message ("Error: not a command name",
                 "activate latex command");
  }
  return false;
}

void
edit_dynamic_rep::activate_hybrid (bool with_args_hint) {
  // WARNING: update edit_interface_rep::set_hybrid_footer when updating this
  if (activate_latex ()) return;
  set_message ("", "");
  path p= search_upwards (HYBRID);
  if (is_nil (p)) return;
  tree st= subtree (et, p);
  if (is_compound (st[0])) return;
  if (is_func (subtree (et, path_up (p)), INACTIVE))
    p= path_up (p);

  // activate macro argument
  string name= st[0]->label;
  path mp= search_upwards (MACRO);
  if (is_nil (mp)) mp= search_upwards ("edit-macro");
  if (!is_nil (mp)) {
    tree mt= subtree (et, mp);
    int i, n= N(mt)-1;
    for (i=0; i<n; i++)
      if (mt[i] == name) {
        assign (p, tree (ARG, copy (name)));
        go_to (end (et, p));
        correct (path_up (p));
        return;
      }
  }

  // built-in primitives, macro applications and values
  bool old_locked= env_locked; env_locked= true;
  tree f= get_env_value (name);
  if ((drd->contains (name) && (f == UNINIT)) ||
      is_func (f, MACRO) || is_func (f, XMACRO)) {
    assign (p, "");
    correct (path_up (p));
    make_compound (make_tree_label (name));
    if (N(st) == 2) insert_tree (st[1]);
  }
  else if (f != UNINIT) {
    assign (p, tree (VALUE, copy (name)));
    go_to (end (et, p));
    correct (path_up (p));
  }
  else if (in_source ()) {
    assign (p, "");
    correct (path_up (p));
    make_compound (make_tree_label (name), with_args_hint? 1: 0);
    if (N(st) == 2) insert_tree (st[1]);
  }
  else set_message ("Error: unknown command",
                    "activate hybrid command");
  env_locked= old_locked;
}

/******************************************************************************
* Other special tags (SYMBOL and COMPOUND)
******************************************************************************/

void
edit_dynamic_rep::activate_symbol () {
  path p= search_upwards (SYMBOL);
  if (is_nil (p)) return;
  tree st= subtree (et, p);
  if (is_func (subtree (et, path_up (p)), INACTIVE))
    p= path_up (p);
  string s= st[0]->label;
  if (is_int (s))
    assign (p, string (((char) as_int (s))));
  else {
    int i, n= N(s);
    for (i=0; i<n; i++)
      if ((s[i]=='<') || (s[i]=='>'))
        { s= ""; break; }
    assign (p, "<" * s * ">");
  }
  go_to (end (et, p));
  correct (path_up (p));
}

/******************************************************************************
* Temporary fixes for block structures
******************************************************************************/

bool
edit_dynamic_rep::make_return_before () {
  bool flag;
  path q= tp;
  while (!is_document (subtree (et, path_up (q)))) q= path_up (q);
  flag= (N (subtree (et, path_up (q))) == (q->item+1)) || (tp != end (et, q));
  if (flag) {
    flag= insert_return ();
    go_to (end (et, q));
  }
  return flag;
}

bool
edit_dynamic_rep::make_return_after () {
  path q= tp;
  while (!is_document (subtree (et, path_up (q)))) {
    q= path_up (q);
    if (!(rp < q)) return false;
  }
  if (tp == start (et, q)) return false;
  return insert_return ();
}

void
edit_dynamic_rep::temp_proof_fix () {
  /* this routine should be removed as soon as possible */
  path p = search_upwards ("proof");
  if (is_nil (p) || (N(tp) < N(p)+2)) return;
  path q = head (tp, N(p)+2);
  tree st= subtree (et, path_up (q));
  if ((!is_document (st)) || (last_item (q) != (N(st)-1))) return;
  insert (path_inc (q), tree (DOCUMENT, ""));
}
