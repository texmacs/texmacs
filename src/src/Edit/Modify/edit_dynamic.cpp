
/******************************************************************************
* MODULE     : edit_dynamic.cpp
* DESCRIPTION: editing dynamic content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_dynamic.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_dynamic_rep::edit_dynamic_rep () {}
edit_dynamic_rep::~edit_dynamic_rep () {}

/******************************************************************************
* Subroutines for inactive content
******************************************************************************/

bool
edit_dynamic_rep::in_preamble_mode () {
  return get_env_string (PREAMBLE) == "true";
}

bool
edit_dynamic_rep::is_deactivated () {
  return !nil (find_deactivated (tp));
}

path
edit_dynamic_rep::find_deactivated (path p) {
  path parent= path_up (p);
  if (nil (parent)) return parent;
  if (is_func (subtree (et, parent), INACTIVE)) return parent;
  return find_deactivated (parent);
}

path
edit_dynamic_rep::find_dynamic (path p) {
  path parent= path_up (p);
  if (nil (parent)) return parent;
  if (drd->is_dynamic (subtree (et, parent))) return p;
  return find_dynamic (parent);
}

/******************************************************************************
* Making general compound objects
******************************************************************************/

bool
edit_dynamic_rep::is_multi_paragraph_macro (tree t) {
  int n= arity (t);
  if (is_document (t) || is_func (t, PARAGRAPH) || is_func (t, SURROUND))
    return true;
  if (is_func (t, MACRO) || is_func (t, WITH))
    return is_multi_paragraph_macro (t [n-1]);
  if (is_extension (t) && (!is_compound (t, "footnote"))) {
    int i;
    for (i=1; i<n; i++)
      if (is_multi_paragraph_macro (t[i]))
	return true;
    tree f= get_env_value (t[0]->label);
    return is_multi_paragraph_macro (f);
  }
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
    return is_func (t, TABLE_FORMAT) && (t[N(t)-1] == tree (ARGUMENT, var));
  }
}

void
edit_dynamic_rep::make_compound (tree_label l, int n= -1) {
  // cout << "Make compound " << as_string (l) << "\n";
  if (n == -1) {
    for (n=0; true; n++) {
      if (drd->correct_arity (l, n) &&
	  ((n>0) || (drd->get_arity_mode (l) == ARITY_NORMAL))) break;
      if (n == 100) return;
    }
  }

  tree t (l, n);
  path p (0, 0);
  if (n == 0) insert_tree (t, 1);
  else {
    tree f= get_env_value (as_string (l));
    bool block_macro= (N(f) == 2) && is_multi_paragraph_macro (f);
    bool table_macro= (N(f) == 2) && contains_table_format (f[1], f[0]);

    tree sel= "";
    if (selection_active_small () ||
	(block_macro && selection_active_normal ()))
      sel= selection_get_cut ();
    if (block_macro && (!table_macro)) {
      t[0]= tree (DOCUMENT, "");
      p   = path (0, 0, 0);
    }
    if ((!drd->all_accessible (l)) && (!in_preamble_mode ())) {
      t= tree (INACTIVE, t);
      p= path (0, p);
    }
    insert_tree (t, p);
    if (table_macro) make_table (1, 1);
    if (sel != "") insert_tree (sel, end (sel));

    string mess;
    if (drd->get_arity_mode (l) != ARITY_NORMAL)
      mess= "A-right: insert argument";
    if (!drd->all_accessible (l)) {
      if (mess != "") mess << ", ";
      mess << "return: activate";
    }
    if (mess == "") mess= "Move to the right when finished";
    set_message (mess, drd->get_name (l));
  }
}

void
edit_dynamic_rep::activate () {
  path p= find_deactivated (tp);
  if (nil (p)) return;
  tree st= subtree (et, p * 0);

  if (is_func (st, COMPOUND) && is_atomic (st[0])) {
    tree u (make_tree_label (st[0]->label));
    u << A (st (1, N(st)));
    st= u;
  }

  assign (p, st);
  go_to (end (et, p));
  correct (path_up (p));
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
	(!inactive) && (!in_preamble_mode ()))
      {
	ins_unary (path_up (p), INACTIVE);
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
  if (forward) do i++; while ((i<=n) && (!drd->insert_point (L(t), i, n)));
  else while ((i>=0) && (!drd->insert_point (L(t), i, n))) i--;
  if ((i<0) || (i>n)) return;
  path q= path_up (p) * i;
  while (!drd->correct_arity (L(t), n+d)) d++;
  tree ins (L(t), d);
  insert (q, ins);
  go_to_argument (q, forward);
}

void
edit_dynamic_rep::insert_argument (bool forward) {
  path p= find_dynamic (tp);
  if (nil (p)) return;
  if (p == tp) p= find_dynamic (path_up (tp));
  if (nil (p)) return;
  insert_argument (p, forward);
}

void
edit_dynamic_rep::remove_argument (path p, bool forward) {
  tree t= subtree (et, path_up (p));
  int i= last_item (p), j, d, n= N(t);

  for (d=1; d<=n-i; d++)
    if (drd->correct_arity (L(t), n-d) &&
	drd->insert_point (L(t), i, n-d))
      {
	bool flag= true;
	for (j=0; j<d; j++)
	  flag= flag && is_empty (t[i+j]);
	if (flag) {
	  remove (p, d);
	  if (forward) go_to_argument (path_up (p) * i, true);
	  else go_to_argument (path_up (p) * (i-1), false);
	  return;
	}
	else break;
      }

  bool flag= true;
  for (j=0; j<n; j++)
    flag= flag && is_empty (t[j]);
  if (flag) {
    assign (path_up (p), "");
    if (subtree (et, path_up (p, 2)) == tree (INACTIVE, "")) {
      assign (path_up (p, 2), "");
      correct (path_up (p, 3));
    }
    else correct (path_up (p, 2));
    return;
  }

  if (forward) go_to_argument (path_up (p) * (i+1), true);
  else go_to_argument (path_up (p) * (i-1), false);
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
  if (n==0) back_monolithic (p);
  else if ((n==1) && is_func (st[0], DOCUMENT, 1) &&
	   (is_func (st[0][0], TABLE_FORMAT) || is_func (st[0][0], TABLE)))
    back_table (p * path (0, 0), forward);
  else if ((n==1) && (is_func (st[0], TABLE_FORMAT) || is_func (st[0], TABLE)))
    back_table (p * 0, forward);
  else go_to_argument (p * (forward? 0: n-1), forward);
}

void
edit_dynamic_rep::back_in_general (tree t, path p, bool forward) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_preamble_mode ())
    if ((L(t) >= START_EXTENSIONS) && (last_item (p) == 0)) {
      tree u (COMPOUND, copy (as_string (L(t))));
      u << A(copy(t));
      assign (path_up (p), u);
      go_to_end (p);
      return;
    }
  remove_argument (p, forward);
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
  else if ((!nil (p)) && is_func (subtree (et, path_up (p)), WITH))
    insert_with (path_up (p), var, val);
  else {
    ins_unary (p, WITH);
    insert (p * 0, copy (tree (WITH, var, val)));
  }
}

void
edit_dynamic_rep::remove_with (path p, string var) {
  tree st= subtree (et, p);
  if (is_func (st, WITH)) {
    int i, n= N(st)-1;
    for (i=0; i<n; i+=2)
      if (st[i] == var) {
	remove (p * i, 2);
	if (n == 2) rem_unary (p);
	return;
      }
  }
  else if ((!nil (p)) && is_func (subtree (et, path_up (p)), WITH))
    remove_with (path_up (p), var);
}

void
edit_dynamic_rep::back_in_with (tree t, path p, bool forward) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_preamble_mode ())
    back_in_general (t, p, forward);
  else if (t[N(t)-1] == "") {
    assign (path_up (p), "");
    correct (path_up (p, 2));
  }
  else go_to_border (path_up (p), !forward);
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
  insert_tree (tree (INACTIVE, t), path (0, p));
  set_message ("return: activate symbol or macro", "hybrid");
}

bool
edit_dynamic_rep::activate_latex () {
  path p= find_deactivated (tp);
  if (!nil (p)) {
    tree st= subtree (et, p * 0);
    if ((is_func (st, LATEX) || (is_func (st, HYBRID))) && is_atomic (st[0])) {
      string  s= st[0]->label, help;
      command cmd;
      if (kbd_get_command (s, help, cmd)) {
	cut (p * 0, p * 1);
	cmd ();
	if (N(st) == 2) insert_tree (copy (st[1]));
	return true;
      }
    }
    set_message ("Error: not a command name", "activate latex command");
  }
  return false;
}

void
edit_dynamic_rep::activate_hybrid () {
  if (activate_latex ()) return;
  path p= find_deactivated (tp);
  if (nil (p)) return;
  tree st= subtree (et, p * 0);

  if (is_func (st, HYBRID) && is_atomic (st[0])) {
    // activate macro argument
    string name= st[0]->label;
    path mp= search_upwards (MACRO);
    if (!nil (mp)) {
      tree mt= subtree (et, mp);
      int i, n= N(mt)-1;
      for (i=0; i<n; i++)
	if (mt[i] == name) {
	  assign (p, tree (ARGUMENT, copy (name)));
	  go_to (end (et, p));
	  correct (path_up (p));
	  return;
	}
    }

    // activate macro application
    tree f= get_env_value (name);
    if (is_func (f, MACRO) || is_func (f, XMACRO)) {
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
    else set_message ("Error: unknown command", "activate hybrid command");
  }
}

/******************************************************************************
* Other special tags (SYMBOL and COMPOUND)
******************************************************************************/

void
edit_dynamic_rep::activate_symbol () {
  path p= find_deactivated (tp);
  if (nil (p)) return;
  tree st= subtree (et, p * 0);

  if (is_func (st, SYMBOL, 1) && is_atomic (st[0])) {
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
}

void
edit_dynamic_rep::activate_compound () {
  path p= search_upwards (COMPOUND);
  if (!nil (p)) {
    tree st= subtree (et, p);
    tree u (make_tree_label (st[0]->label));
    u << A (st (1, N(st)));
    assign (p, u);
    go_to (end (et, p));
  }
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
  while (!is_document (subtree (et, path_up (q)))) q= path_up (q);
  if (tp == start (et, q)) return false;
  return insert_return ();
}

void
edit_dynamic_rep::temp_proof_fix () {
  /* this routine should be removed as soon as possible */
  path p = search_upwards_compound ("proof");
  if (nil (p) || (N(tp) < N(p)+2)) return;
  path q = head (tp, N(p)+2);
  tree st= subtree (et, path_up (q));
  if ((!is_document (st)) || (last_item (q) != (N(st)-1))) return;
  insert (path_inc (q), tree (DOCUMENT, ""));
}
