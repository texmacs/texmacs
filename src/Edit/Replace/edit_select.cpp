
/******************************************************************************
* MODULE     : edit_select.cpp
* DESCRIPTION: Selection handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Replace/edit_select.hpp"
#include "Interface/edit_interface.hpp"
#include "convert.hpp"
#include "packrat.hpp"
#include "tree_select.hpp"
#include "drd_mode.hpp"

/******************************************************************************
* Internationalization
******************************************************************************/

string
selection_encode (string lan, string s) {
  if ((lan == "croatian") || (lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return cork_to_il2 (s);
  else if (lan == "spanish")
    return spanish_to_ispanish (s);
  else if (lan == "german")
    return german_to_igerman (s);
  else return s;
}

string
selection_decode (string lan, string s) {
  if ((lan == "croatian") || (lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return il2_to_cork (s);
  else if (lan == "spanish")
    return ispanish_to_spanish (s);
  else if (lan == "german")
    return igerman_to_german (s);
  else return s;
}

/******************************************************************************
* Constructor and destructor
******************************************************************************/

edit_select_rep::edit_select_rep ():
  cur_sel (no_ranges ()),
  selecting (false), shift_selecting (false), mid_p (),
  selection_import ("default"), selection_export ("default"),
  focus_p (), focus_hold (false) {}
edit_select_rep::~edit_select_rep () {}

/******************************************************************************
* Semantic selections
******************************************************************************/

path
edit_select_rep::semantic_root (path p) {
  while (p != rp) {
    tree st= subtree (et, path_up (p));
    if (path_up (p) != rp && is_func (st, DOCUMENT, 1))
      st= subtree (et, path_up (p, 2));
    if (is_func (st, CELL)) break;
    if (is_compound (st) && N(st) == 1) {
      tree env= drd->get_env (L(st), 0);
      tree mt= drd_env_read (env, MODE);
      tree pt= drd_env_read (env, "prog-language");
      if (mt == "math") break;
      if (mt == "prog" && pt == "minimal") break;
      if (mt == "text") return rp;
    }
    p= path_up (p);
  }
  return p;
}

bool
edit_select_rep::semantic_active (path q) {
  if (as_string (eval ("(get-preference \"semantic editing\")")) == "on") {
    path p= semantic_root (q);
    //cout << subtree (et, p) << ", " << p << " -> " << end (et, p) << "\n";
    tree mt= get_env_value (MODE, end (et, p));
    tree lt= get_env_value (MODE_LANGUAGE (mt->label), end (et, p));
    string lan= (is_atomic (lt)? lt->label: string ("std-math"));
    if (mt == "math" || (mt == "prog" && lan == "minimal"))
      return packrat_available_path (lan, subtree (et, p), q / p);
  }
  return false;
}

bool
edit_select_rep::semantic_select (path p, path& q1, path& q2, int mode) {
  if (!semantic_active (p)) return false;
  if (mode < 2 && get_preference ("semantic selections") != "on") return false;
  p= semantic_root (p);
  while (p != rp && !(p <= q1 && p <= q2))
    p= semantic_root (path_up (p));
  tree mt= get_env_value (MODE, end (et, p));
  tree lt= get_env_value (MODE_LANGUAGE (mt->label), end (et, p));
  string lan= (is_atomic (lt)? lt->label: string ("std-math"));
  path p1= q1 / p, p2= q2 / p;
  path cp= (p <= tp? tp / p: path ());
  tree st= subtree (et, p);
  bool ret= packrat_select (lan, "Main", st, cp, p1, p2, mode);
  p1= correct_right_most_inside (p1, st);
  if (ret) {
    q1= p * p1;
    q2= p * p2;
  }
  return ret;
}

/******************************************************************************
* Selecting particular things
******************************************************************************/

void
edit_select_rep::select (path p1, path p2) {
  //cout << "Select " << p1 << " -- " << p2 << "\n";
  if (cur_sel == simple_range (p1, p2)) return;
  if (!(rp <= p1 && rp <= p2)) return;
  if (is_empty (cur_sel) && p1 == p2) {
    cur_sel= simple_range (p1, p2);
    return;
  }
  if (p1 != p2) {
    path cp= common (p1, p2);
    tree st= subtree (et, cp);
    if (!is_func (st, TABLE) && !is_func (st, ROW))
      (void) semantic_select (cp, p1, p2, 0);
  }
  if (path_less (p1, p2))
    cur_sel= simple_range (p1, p2);
  else
    cur_sel= simple_range (p2, p1);
  notify_change (THE_SELECTION);
}

void
edit_select_rep::select (path p) {
  select (start (et, p), end (et, p));
}

void
edit_select_rep::select_all () {
  select (rp);
}

void
edit_select_rep::select_line () {
  select (search_parent_upwards (DOCUMENT));
}

void edit_select_rep::get_selection (path& p1, path& p2) {
  p1= start (cur_sel); p2= end (cur_sel); }
void edit_select_rep::set_selection (path p1, path p2) {
  select (p1, p2); }

/******************************************************************************
* For interface with cursor movement
******************************************************************************/

void
edit_select_rep::select_from_cursor () {
  if (selecting) {
    select (mid_p, tp);
    if (shift_selecting) selecting = false;
  }
}

void
edit_select_rep::select_from_cursor_if_active () {
  if (selecting) select (mid_p, tp);
  else selection_cancel ();
}

void
edit_select_rep::select_from_keyboard (bool flag) {
  selecting= flag;
  shift_selecting= false;
  if (flag) {
    cur_sel= simple_range (tp, tp);
    mid_p  = copy (tp);
  }
  else mid_p= rp;
}

void
edit_select_rep::select_from_shift_keyboard () {
  if (!shift_selecting || is_empty (cur_sel)) mid_p= copy (tp);
  selecting= true;
  shift_selecting= true;
}

/******************************************************************************
* Enlarging an existing selection
******************************************************************************/

static int
breaking_force (char c) {
  if (c == ' ') return 3;
  if (is_punctuation (c)) return 2;
  if (is_iso_alpha (c) || is_digit (c)) return 0;
  return 1;
}

void
edit_select_rep::select_enlarge_text () {
  path p= common (cur_sel);
  if (is_empty (cur_sel) && !is_nil (p)) p= path_up (p);
  tree st= subtree (et, p);
  ASSERT (is_atomic (st), "non textual tree");
  string s= st->label;
  string mode= get_env_string (MODE);
  int i1= last_item (start (cur_sel)), j1= i1;
  int i2= last_item (end   (cur_sel)), j2= i2;
  path q= path_up (p);

  if (mode == "text" || mode == "src") {
    int i, f= 4;
    if (i1 > 0) {
      i= i1; tm_char_backwards (s, i);
      f= min (f, breaking_force (s[i]));
    }
    if (i2 < N(s))
      f= min (f, breaking_force (s[i2]));

    while (i1 > 0) {
      i= i1; tm_char_backwards (s, i);
      if (breaking_force (s[i]) > f) break;
      i1= i;
    }
    while (i2 < N(s)) {
      if (breaking_force (s[i2]) > f) break;
      tm_char_forwards (s, i2);
    }

    if (i1 < i2 && (i1 != j1 || i2 != j2)) {
      if (is_concat (subtree (et, q)) && i1 == 0 && i2 == N(s))
        select (q * 0, q * 1);
      else select (p * i1, p * i2);
      return;
    }
  }

  if (is_concat (subtree (et, q)) || (i1 == 0 && i2 == N(s)))
    select (q * 0, q * 1);
  else select (p * 0, p * N(s));
}

bool
incomplete_script_selection (tree t, path lp, path rp) {
  if (!is_func (t, CONCAT)) return false;
  if (N(lp) < 2 || N(rp) < 2) return false;
  int l= lp->item, r= rp->item;
  if (is_func (t[l], RSUB) || is_func (t[l], RSUP)) return true;
  if (is_func (t[r], LSUB) || is_func (t[r], LSUP)) return true;
  if (l  >0    && (is_func (t[l-1], LSUB) || is_func (t[l-1], LSUP))) return true;
  if (r+1<N(t) && (is_func (t[r+1], RSUB) || is_func (t[r+1], RSUP))) return true;
  return false;
}

void
edit_select_rep::select_enlarge () {
  path sp, sq;
  if (is_empty (cur_sel) && !is_nil (start (cur_sel))) {
    sp= path_up (start (cur_sel));
    sq= sp;
  }
  else {
    sp= common (cur_sel);
    if (!(rp < sp)) {
      selection_cancel ();
      set_message ("", "");
      return;
    }
    sq= path_up (sp);
  }
  path pp= sp, p1= start (cur_sel), p2= end (cur_sel);
  if (start (cur_sel) == pp * 0 &&
      end   (cur_sel) == pp * right_index (subtree (et, pp)))
    if (!is_nil (pp)) pp= path_up (pp);
  if (is_func (subtree (et, pp), TFORMAT)) pp= path_up (pp);
  if (semantic_select (pp, p1, p2, 1))
    select (p1, p2);
  else {
    if (is_atomic (subtree (et, sp))) select_enlarge_text ();
    else select (sq * 0, sq * 1);
  }

  path p = common (cur_sel);
  tree st= subtree (et, p);
  if (drd->var_without_border (L(st)) ||
      is_func (st, TFORMAT) ||
      is_func (st, DOCUMENT, 1) ||
      is_script (st) ||
      incomplete_script_selection (st, start (cur_sel) / p, end (cur_sel) / p))
    select_enlarge ();
  else {
    tree s;
    if (is_atomic (st)) s= "text";
    else if (is_func (st, COMPOUND)) s= as_string (st[0]);
    else if (is_func (st, WITH)) s= concat ("with ", as_string (st[0]));
    else s= as_string (L(st));
    set_message (concat ("selected ", s), "enlarge selection");
  }
  selecting= shift_selecting= false;
}

static bool
stop_enlarge_environmental (tree t) {
  if (is_func (t, WITH, 3) && (t[0] == MODE) && (t[1] == "math")) return true;
  if (!is_extension (t)) return false;
  if (is_multi_paragraph (t)) return true;
  string s= as_string (L(t));
  return
    (s == "part") ||
    (s == "chapter") ||
    (s == "section") ||
    (s == "subsection") ||
    (s == "subsubsection") ||
    (s == "paragraph") ||
    (s == "subparagraph");
}

void
edit_select_rep::select_enlarge_environmental () {
  select_enlarge ();
  if (is_empty (cur_sel)) return;
  path p= common (cur_sel);
  tree st= subtree (et, p);
  if (stop_enlarge_environmental (st)) return;
  select_enlarge_environmental ();
}

/******************************************************************************
* Test whether selection is active
******************************************************************************/

bool
edit_select_rep::selection_active_any () {
  return !is_empty (cur_sel);
}

bool
edit_select_rep::selection_active_normal () {
  return selection_active_any () && (!selection_active_table ());
}

bool
edit_select_rep::selection_active_table (bool strict) {
  if (!selection_active_any ()) return false;
  return is_table_selection (et, start (cur_sel), end (cur_sel), strict);
}

bool
edit_select_rep::selection_active_small () {
  if (!selection_active_normal ()) return false;
  path p1, p2;
  selection_get (p1, p2);
  if (p2 == p1) return false;
  if (is_multi_paragraph (subtree (et, common (p1, p2)))) return false;
  return true;
}

bool
edit_select_rep::selection_active_enlarging () {
  return (selecting || !is_empty (cur_sel)) && (mid_p == tp);
}

/******************************************************************************
* Get the selection
******************************************************************************/

void
edit_select_rep::selection_correct (path i1, path i2, path& o1, path& o2) {
  ASSERT (rp <= i1 && rp <= i2, "paths not inside document");
  int old_mode= get_access_mode ();
  if (in_source ()) set_access_mode (DRD_ACCESS_SOURCE);
  ::selection_correct (subtree (et, rp), i1 / rp, i2 / rp, o1, o2);
  set_access_mode (old_mode);
  o1= rp * o1; o2= rp * o2;
}

path
edit_select_rep::selection_get_subtable (
  int& row1, int& col1, int& row2, int& col2)
{
  path fp= find_subtable_selection (et, start (cur_sel), end (cur_sel),
                                    row1, col1, row2, col2);
  table_bound (fp, row1, col1, row2, col2);
  return fp;
}

selection
edit_select_rep::compute_selection (path p1, path p2) {
  if (is_table_selection (et, p1, p2, true)) {
    int row1, col1, row2, col2;
    path fp= find_subtable_selection (et, p1, p2, row1, col1, row2, col2);
    tree st= subtree (et, fp);
    table_bound (fp, row1, col1, row2, col2);

    int i, j;
    rectangle r (0, 0, 0, 0);
    for (i=row1; i<=row2; i++)
      for (j=col1; j<=col2; j++) {
        path cp= fp * ::table_search_cell (st, i, j);
        selection sel= eb->find_check_selection (cp * 0, cp * 1);
        if (sel->valid) {
          rectangles rs= sel->rs;
          if (r != rectangle (0, 0, 0, 0)) rs= rectangles (r, rs);
          r= least_upper_bound (rs);
        }
      }
    return selection (rectangles (r), fp * 0, fp * 1);
  }
  else {
    path p_start, p_end;
    //cout << "Find " << p1 << " -- " << p2 << "\n";
    selection_correct (p1, p2, p_start, p_end);
    //cout << "Find " << p_start << " -- " << p_end << "\n";
    selection sel= eb->find_check_selection (p_start, p_end);
    //cout << "sel= " << sel << "\n";
    return sel;
  }
}

selection
edit_select_rep::compute_selection (range_set sel) {
  if (is_empty (sel)) return selection ();
  int old_mode= set_access_mode (DRD_ACCESS_SOURCE);
  // FIXME: instead of changing the access mode,
  // we should already start with setting the correct DRD.
  // For instance, when searching text using a popup window,
  // the DRD is incorrect.  Consequently, matching text inside
  // certain macros can become inaccessible, after which
  // the entire macro is erroneously highlighted.
  // Similar remark for the main routine for computing selections
  rectangles rs;
  for (int i=0; i+1<N(sel); i+=2) {
    selection ssel= compute_selection (sel[i], sel[i+1]);
    if (ssel->valid) rs << ssel->rs;
  }
  set_access_mode (old_mode);
  return selection (rs, start (sel), end (sel));
}

void
edit_select_rep::selection_get (selection& sel) {
  sel= compute_selection (cur_sel);
}

void
edit_select_rep::selection_get (path& p1, path& p2) {
  if (selection_active_table ()) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    p1= fp * 0;
    p2= fp * 1;
  }
  else selection_correct (start (cur_sel), end (cur_sel), p1, p2);
  /*
  selection sel; selection_get (sel);
  p1= sel->start;
  p2= sel->end;
  */
}

path
edit_select_rep::selection_get_start () {
  return start (cur_sel);
}

path
edit_select_rep::selection_get_end () {
  return end (cur_sel);
}

path
edit_select_rep::selection_var_get_start () {
  path p1, p2;
  selection_get (p1, p2);
  return p1;
}

path
edit_select_rep::selection_var_get_end () {
  path p1, p2;
  selection_get (p1, p2);
  return p2;
}

tree
edit_select_rep::selection_get () {
  if (!selection_active_any ()) return "";
  if (selection_active_table ()) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    return table_get_subtable (fp, row1, col1, row2, col2);
  }
  else {
    path p1, p2;
    // cout << "Selecting...\n";
    selection_get (p1, p2);
    // cout << "Between paths: " << p1 << " and " << p2 << "\n";
    tree t= selection_compute (et, p1, p2);
    // cout << "Selection : " << t << "\n";
    return simplify_correct (t);
  }
}

path
edit_select_rep::selection_get_path () {
  path p1, p2;
  selection_get (p1, p2);
  if (p2 == p1 && !is_empty (cur_sel))
    return path_up (p1);
  return common (p1, p2);
}

path
edit_select_rep::selection_get_cursor_path () {
  if (!selection_active_any ()) return tp;
  return start (et, selection_get_path ());
}

tree
edit_select_rep::selection_get_env_value (string var) {
  if (!selection_active_any ()) return get_env_value (var);
  return get_env_value (var, selection_get_cursor_path ());
}

/******************************************************************************
* Copy and paste
******************************************************************************/

void
edit_select_rep::selection_raw_set (string key, tree t) {
  (void) ::set_selection (key, t, "", "", "", "texmacs");
}

tree
edit_select_rep::selection_raw_get (string key) {
  tree t; string s;
  (void) ::get_selection (key, t, s, "texmacs");
  return t;
}

void
edit_select_rep::selection_set_start (path p) {
  if (!selection_active_any ()) {
    if (rp < start (cur_sel)) select (start (cur_sel), start (cur_sel));
    else select (tp, tp);
  }
  if (is_nil (p)) selection_set_start (tp);
  else if (path_less_eq (end (cur_sel), p)) select (p, p);
  else if (rp < p) select (p, end (cur_sel));
}

void
edit_select_rep::selection_set_end (path p) {
  if (is_nil (p)) selection_set_end (tp);
  else if (path_less_eq (p, start (cur_sel))) select (p, p);
  else if (rp < p) select (start (cur_sel), p);
}

void
edit_select_rep::selection_set_paths (path p1, path p2) {
  if (is_nil (p1) || is_nil (p2)) selection_set_paths (tp, tp);
  else if (path_less_eq (p2, p1)) select (p1, p1);
  else if (rp < p1 && rp < p2) select (p1, p2);
}

void
edit_select_rep::selection_set_range_set (range_set sel) {
  selection_set_paths (start (sel), end (sel));
}

void
edit_select_rep::selection_set (string key, tree t, bool persistant) {
  selecting= shift_selecting= false;
  string mode= as_string (selection_get_env_value (MODE));
  string lan = as_string (selection_get_env_value (MODE_LANGUAGE (mode)));
  tree sel= tuple ("texmacs", t, mode, lan);
  /* TODO: add mode="graphics" somewhere in the context of the <graphics>
     tag. To be done when implementing the different embeddings for
     nicely copying graphics into text, text into graphics, etc. */
  string s, sh, sv;
  if (key == "primary" || key == "mouse") {
    if (selection_export == "verbatim") t= exec_verbatim (t, tp);
    if (selection_export == "html") t= exec_html (t, tp);
    if (selection_export == "latex") t= exec_latex (t, tp);
    if ((selection_export == "latex") && (mode == "math"))
      t= compound ("math", t);
    if (selection_export != "default")
      s= tree_to_generic (t, selection_export * "-snippet");
    else {
      s= tree_to_generic (t, "texmacs-snippet");
#ifdef QTTEXMACS
      tree tmp;
      tmp= exec_verbatim (t, tp);
      sv= tree_to_generic (tmp, "verbatim-snippet");
      //tmp= exec_html (t, tp);
      //sh= tree_to_generic (tmp, "html-snippet");
#endif
    }
    s= selection_encode (lan, s);
  }
  if (::set_selection (key, sel, s, sv, sh, selection_export) && !persistant)
    selection_cancel ();
}

void
edit_select_rep::selection_set (tree t) {
  selection_set ("primary", t);
}

void
edit_select_rep::selection_copy (string key) {
  bool emacs= (get_preference ("look and feel", "default") == "emacs");
  if (inside_active_graphics ()) {
    tree t= as_tree (eval ("(graphics-copy)"));
    selection_set (key, t, !emacs);
    return;
  }
  if (selection_active_any ()) {
    path old_tp= tp;
    selection sel; selection_get (sel);
    go_to (sel->end);
    tree t= selection_get ();
    go_to (sel->start);
    selection_set (key, t, !emacs);
    go_to (old_tp);
  }
}

tree
edit_select_rep::selection_get (string key) {
  tree t; string s;
  (void) ::get_selection (key, t, s, selection_import);
  return t;
}

void
edit_select_rep::selection_paste (string key) {
  tree t; string s;
  (void) ::get_selection (key, t, s, selection_import);
  if (inside_active_graphics ()) {
    if (is_tuple (t, "texmacs", 3))
      call ("graphics-paste", t[1]);
    return;
  }
  if (is_tuple (t, "extern", 1)) {
    string mode= get_env_string (MODE);
    string lan = get_env_string (MODE_LANGUAGE (mode));
    string s   = selection_decode (lan, as_string (t[1]));
    if (mode == "prog")
      if (selection_import == "latex" || selection_import == "html")
        selection_import= "verbatim";
    if (mode == "math")
      if (selection_import == "latex") {
        while (starts (s, " ")) s= s(1, N(s));
        while (ends (s, " ")) s= s(0, N(s)-1);
        if (!starts (s, "$") && !ends (s, "$"))
          s= "$" * s * "$";
      }
    string fm;
    if (selection_import == "verbatim" && mode == "prog") fm= "code-snippet";
    else if (selection_import == "default") fm= "texmacs-snippet";
    else fm= selection_import * "-snippet";
    tree doc= generic_to_tree (s, fm);
    if (is_func (doc, DOCUMENT, 1)) doc= doc[0]; // temporary fix
    if (mode == "math" && is_compound (doc, "math", 1)) doc= doc[0];
    insert_tree (doc);
  }
  if (is_tuple (t, "texmacs", 3)) {
    string mode= get_env_string (MODE);
    string lan = get_env_string (MODE_LANGUAGE (mode));
    if (is_compound (t[1], "text", 1) && mode == "text")
      t= tuple ("texmacs", t[1][0], "text", lan);
    if (is_compound (t[1], "math", 1) && mode == "math")
      t= tuple ("texmacs", t[1][0], "math", lan);
    if (mode == "math" && t[2] == "text")
      set_message ("Error: invalid paste of text into a formula", "paste");
    else if (mode == "prog" && t[2] == "math") {
      tree in= tuple (lan, t[1]);
      tree r= stree_to_tree (call ("plugin-math-input", tree_to_stree (in)));
      insert_tree (r);
    }
    else {
      if ((t[2] != mode) && (t[2] != "src") && (mode != "src") &&
  ((t[2] == "math") || (mode == "math"))) {
        if (t[2] == "math")
          insert_tree (compound ("math", ""), path (0, 0));
        else if (t[2] == "text")
          insert_tree (compound ("text", ""), path (0, 0));
        else
          insert_tree (tree (WITH, copy (MODE), copy (t[2]), ""), path (2, 0));
      }
      if (is_func (t[1], TFORMAT) || is_func (t[1], TABLE)) {
        int row, col;
        path fp= search_format (row, col);
        if (is_nil (fp)) insert_tree (compound (copy (TABULAR), t[1]));
        else table_write_subtable (fp, row, col, t[1]);
      }
      else insert_tree (t[1]);
    }
  }
}

void
edit_select_rep::selection_clear (string key) {
  ::clear_selection (key);
}

void
edit_select_rep::selection_cancel () {
  selecting= shift_selecting= false;
  if (is_empty (cur_sel)) return;
  select (start (cur_sel), start (cur_sel));
}

void
edit_select_rep::selection_set_import (string fm) {
  selection_import= fm;
}

void
edit_select_rep::selection_set_export (string fm) {
  selection_export= fm;
}

string
edit_select_rep::selection_get_import () {
  return selection_import;
}

string
edit_select_rep::selection_get_export () {
  return selection_export;
}

/******************************************************************************
* Cutting the selection
******************************************************************************/

void
edit_select_rep::cut (path p) {
  cut (start (et, p), end (et, p));
}

void
edit_select_rep::cut (path p1, path p2) {
  path p = common (p1, p2);
  tree st= subtree (et, p);
  raw_cut (p1, p2);
  if (!is_func (st, TFORMAT) &&
      !is_func (st, TABLE) &&
      !is_func (st, ROW) &&
      !is_document (subtree (et, p)) &&
      is_concat (subtree (et, path_up (p))))
    correct_concat (path_up (p));
}

void
edit_select_rep::raw_cut (path p1, path p2) {
  if (p2 == p1) return;
  path p = common (p1, p2);
  tree t = subtree (et, p);
  int  n = N(p);
  int  i1= p1[n];
  int  i2= p2[n];

  if (is_document (t) || is_concat (t)) {
    path q1= copy (p); q1 << path (i1, end (t[i1]));
    path q2= copy (p); q2 << path (i2, start (t[i2]));
    raw_cut (q2, p2);
    if (i2>i1+1) remove (p * (i1+1), i2-i1-1);
    raw_cut (p1, q1);
    if (is_concat (t)) correct_concat (p);
    else remove_return (p * i1);
    return;
  }

  if (is_func (t, TFORMAT) || is_func (t, TABLE) || is_func (t, ROW)) {
    path fp= ::table_search_format (et, p);
    tree st= subtree (et, fp);
    int row1, col1, row2, col2, nr_rows, nr_cols;
    table_search_coordinates (st, tail (p1, N(fp)), row1, col1);
    table_search_coordinates (st, tail (p2, N(fp)), row2, col2);
    if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
    if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }
    table_get_extents (fp, nr_rows, nr_cols);

    int i, j;
    for (i=row1; i<=row2; i++)
      for (j=col1; j<=col2; j++) {
        path cp= fp * ::table_search_cell (st, i, j);
        if (is_func (subtree (et, cp), CELL, 1)) cp= cp * 0;
        assign (cp, "");
      }
    path cp= fp * ::table_search_cell (st, row1, col1);
    go_to (cp * path (0, 0));

    if (is_func (st, TFORMAT))
      table_del_format (fp, row1+1, col1+1, row2+1, col2+1, "");

    if (fp == search_format ()) {
      if (col1 == 0 && col2 == nr_cols-1 && row2 > row1)
        table_remove (fp, row1 + 1, col1, row2 - row1, 0);      
      else if (row1 == 0 && row2 == nr_rows-1 && col2 > col1)
        table_remove (fp, row1, col1 + 1, 0, col2 - col1);      
      table_correct_block_content ();
      table_resize_notify ();
    }
    else {
      observer obs= position_new (tp);
      go_to (start (et, fp * ::table_search_cell (st, row1, col1)));
      table_correct_block_content ();
      table_resize_notify ();
      go_to (position_get (obs));
      position_delete (obs);
    }
    return;
  }

  if (is_compound (t) && (!is_format (t))) {
    assign (p, "");
    return;
  }

  if ((N(p1) != (N(p)+1)) || (N(p2) != (N(p)+1))) {
    failed_error << "t = " << t << "\n";
    failed_error << "p = " << p << "\n";
    failed_error << "p1= " << p1 << "\n";
    failed_error << "p2= " << p2 << "\n";
    FAILED ("invalid cut");
  }

  if (is_atomic (t)) {
    int pos= last_item (p1);
    int nr = last_item (p2)-pos;
    if (nr>0) remove (p1, nr);
  }
  else {
    if ((last_item (p1) != 0) || (last_item (p2) != 1)) {
      failed_error << "t = " << t << "\n";
      failed_error << "p = " << p << "\n";
      failed_error << "p1= " << p1 << "\n";
      failed_error << "p2= " << p2 << "\n";
      FAILED ("invalid object cut");
    }
    assign (p, "");
  }
}

void
edit_select_rep::selection_cut (string key) {
  if (inside_active_graphics ()) {
    if (key != "none") {
      tree t= as_tree (eval ("(graphics-cut)"));
      selection_set (key, t);
    }
  }
  else if (selection_active_any ()) {
    path p1, p2;
    if (selection_active_table ()) {
      p1= start (cur_sel); p2= end (cur_sel);
      if(key != "none") {
        tree sel= selection_get ();
        selection_set (key, sel);
      }
    }
    else {
      selection_get (p1, p2);
      go_to (p2);
      if (p2 == p1) return;
      if (key != "none") {
        tree sel= selection_compute (et, p1, p2);
        selection_set (key, simplify_correct (sel));
      }
    }
    cut (p1, p2);
  }
}

tree
edit_select_rep::selection_get_cut () {
  tree t= selection_get ();
  selection_cut ("none");
  return t;
}

void
edit_select_rep::selection_move () {
  observer pos= position_new (tp);
  tree t= selection_get_cut ();
  go_to (position_get (pos));
  insert_tree (t);
  position_delete (pos);
}

/******************************************************************************
* Focus related routines
******************************************************************************/

path
edit_select_rep::manual_focus_get () {
  return focus_p;
}

void
edit_select_rep::manual_focus_set (path p, bool force) {
  //cout << "Set focus " << p << ", " << force << ", " << focus_hold << "\n";
  if (is_nil (p) && focus_hold && !force) return;
  focus_p= p;
  focus_hold= !is_nil (p);
}

void
edit_select_rep::manual_focus_release () {
  focus_hold= false;
}

path
edit_select_rep::focus_search (path p, bool skip_flag, bool up_flag) {
  //cout << "Search focus " << p << ", " << skip_flag << ", " << up_flag << "\n";
  if (!(rp < p)) return rp;
  tree st= subtree (et, p);
  if (!skip_flag) return p;
  if (none_accessible (st) && p == path_up (tp) && last_item (tp) != 0)
    return p;
  if (is_atomic (st) ||
      is_func (st, DOCUMENT) ||
      is_func (st, CONCAT) ||
      is_func (st, TFORMAT) ||
      is_func (st, TABLE) ||
      is_func (st, ROW) ||
      is_func (st, CELL) ||
      is_compound (st, "shown") ||
      is_func (st, HIDDEN) ||
      is_compound (st, "shared") ||
      is_compound (st, "slide") ||
      is_compound (st, "with-screen-color") ||
      is_compound (st, "mc-field") ||
      is_compound (st, "live-io*") ||
      up_flag)
    return focus_search (path_up (p), skip_flag, false);
  return p;
}

path
edit_select_rep::focus_get (bool skip_flag) {
  //cout << "Search focus " << focus_p << ", " << skip_flag << "\n";
  if (!is_nil (focus_p))
    return focus_search (focus_p, skip_flag, false);
  if (selection_active_any ())
    return focus_search (selection_get_path (), skip_flag, false);
  else {
    tree st= subtree (et, path_up (tp));
    if (is_compound (st, "draw-over")) skip_flag= false;
    if (is_compound (st, "draw-under")) skip_flag= false;
    if (is_compound (st, "float")) skip_flag= false;
    if (is_compound (st, "wide-float")) skip_flag= false;
    if (is_compound (st, "footnote")) skip_flag= false;
    if (is_compound (st, "footnote-anchor")) skip_flag= false;
    if (is_compound (st, "wide-footnote")) skip_flag= false;
    if (is_compound (st, "note-footnote")) skip_flag= false;
    if (is_compound (st, "note-footnote*")) skip_flag= false;
    if (is_compound (st, "cite-detail")) skip_flag= false;
    return focus_search (path_up (tp), skip_flag, true);
  }
}

/******************************************************************************
* Alternative selections
******************************************************************************/

void
edit_select_rep::set_alt_selection (string name, range_set sel) {
  if (alt_sels[name] != sel) {
    alt_sels (name)= sel;
    notify_change (THE_SELECTION);
  }
}

range_set
edit_select_rep::get_alt_selection (string name) {
  return alt_sels[name];
}

range_set
current_alt_selection (string name) {
  return get_current_editor()->get_alt_selection (name);
}

void
edit_select_rep::cancel_alt_selection (string name) {
  if (alt_sels->contains (name)) {
    alt_sels->reset (name);
    notify_change (THE_SELECTION);
  }
}

void
edit_select_rep::cancel_alt_selections () {
  if (N(alt_sels) > 0) {
    alt_sels= hashmap<string,range_set> ();
    notify_change (THE_SELECTION);
  }
}
