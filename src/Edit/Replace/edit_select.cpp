
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
#include "tree_traverse.hpp"

/******************************************************************************
* Internationalization
******************************************************************************/

string
selection_encode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return cork_to_il2 (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return koi8_to_iso (s);
  else if (lan == "ukrainian")
    return koi8uk_to_iso (s);
  else if (lan == "spanish")
    return spanish_to_ispanish (s);
  else if (lan == "german")
    return german_to_igerman (s);
  else return s;
}

string
selection_decode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return il2_to_cork (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return iso_to_koi8 (s);
  else if (lan == "ukrainian")
    return iso_to_koi8uk (s);
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
  selecting (false), shift_selecting (false), mid_p (),
  selection_import ("texmacs"), selection_export ("texmacs"),
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
      if (drd_env_read (env, "mode") == "math") break;
      if (drd_env_read (env, "mode") == "prog")
        if (drd_env_read (env, "prog-language") == "minimal") break;
    }
    p= path_up (p);
  }
  return p;
}

bool
edit_select_rep::semantic_active (path p) {
  if (as_string (eval ("(get-preference \"semantic editing\")")) == "on") {
    p= semantic_root (p);
    //cout << subtree (et, p) << ", " << p << " -> " << end (et, p) << "\n";
    tree mode= get_env_value (MODE, end (et, p));
    tree plan= get_env_value (PROG_LANGUAGE, end (et, p));
    return mode == "math" || (mode == "prog" && plan == "minimal");
  }
  else return false;
}

bool
edit_select_rep::semantic_select (path p, path& q1, path& q2, int mode) {
  if (!semantic_active (p)) return false;
  p= semantic_root (p);
  tree mt= get_env_value (MODE, end (et, p));
  tree lt= get_env_value (MODE_LANGUAGE (mt->label), end (et, p));
  string lan= (is_atomic (lt)? lt->label: string ("std-math"));
  path p1= q1 / p, p2= q2 / p;
  path cp= (p <= tp? tp / p: path ());
  tree st= subtree (et, p);
  bool ret= packrat_select (lan, "Main", st, cp, p1, p2, mode);
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
  if (start_p == p1 && end_p == p2) return;
  if (!(rp <= p1 && rp <= p2)) return;
  if (p1 != p2) {
    path cp= common (p1, p2);
    tree st= subtree (et, cp);
    if (!is_func (st, TABLE) && !is_func (st, ROW))
      (void) semantic_select (cp, p1, p2, 0);
  }
  if (path_less (p1, p2)) {
    start_p= copy (p1);
    end_p  = copy (p2);
  }
  else {
    start_p= copy (p2);
    end_p  = copy (p1);
  }
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

void edit_select_rep::get_selection (path& start, path& end) {
  start= copy (start_p); end= copy (end_p); }
void edit_select_rep::set_selection (path start, path end) {
  select (start, end); }

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
    start_p= copy (tp);
    mid_p  = copy (tp);
    end_p  = copy (tp);
  }
  else mid_p= rp;
}

void
edit_select_rep::select_from_shift_keyboard () {
  if (!shift_selecting || end_p == start_p) mid_p= copy (tp);
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
  path p= common (start_p, end_p);
  if (start_p == end_p) p= path_up (p);
  tree st= subtree (et, p);
  ASSERT (is_atomic (st), "non textual tree");
  string s= st->label;
  string mode= get_env_string (MODE);
  int i1= last_item (start_p), j1= i1;
  int i2= last_item (end_p), j2= i2;
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
  if (start_p == end_p) {
    sp= path_up (start_p);
    sq= sp;
  }
  else {
    sp= common (start_p, end_p);
    if (!(rp < sp)) {
      selection_cancel ();
      set_message ("", "");
      return;
    }
    sq= path_up (sp);
  }
  path pp= sp, p1= start_p, p2= end_p;
  if (start_p == pp * 0 && end_p == pp * right_index (subtree (et, pp)))
    if (!is_nil (pp)) pp= path_up (pp);
  if (semantic_select (pp, p1, p2, 1))
    select (p1, p2);
  else {
    if (is_atomic (subtree (et, sp))) select_enlarge_text ();
    else select (sq * 0, sq * 1);
  }

  path p = common (start_p, end_p);
  tree st= subtree (et, p);
  if (drd->var_without_border (L(st)) ||
      is_func (st, TFORMAT) ||
      is_func (st, DOCUMENT, 1) ||
      is_script (st) ||
      incomplete_script_selection (st, start_p / p, end_p / p))
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
  if (end_p == start_p) return;
  path p= common (start_p, end_p);
  tree st= subtree (et, p);
  if (stop_enlarge_environmental (st)) return;
  select_enlarge_environmental ();
}

/******************************************************************************
* Test whether selection is active
******************************************************************************/

bool
edit_select_rep::selection_active_any () {
  return end_p != start_p;
  // return made_selection;
}

bool
edit_select_rep::selection_active_normal () {
  return selection_active_any () && (!selection_active_table ());
}

bool
edit_select_rep::selection_active_table () {
  if (!selection_active_any ()) return false;
  path p= common (start_p, end_p);
  if ((p == start_p) || (p == end_p)) p= path_up (p);
  tree t= subtree (et, p);
  return
    is_func (t, TFORMAT) || is_func (t, TABLE) ||
    is_func (t, ROW) || is_func (t, CELL);
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
  return (selecting || (end_p != start_p)) && (mid_p == tp);
}

/******************************************************************************
* Subroutines for table selections
******************************************************************************/

static path
table_search_format (tree t, path p) {
  tree st= subtree (t, p);
  if (is_func (st, TFORMAT) && is_func (st[N(st)-1], TABLE)) return p;
  while ((!is_nil (p)) && (!is_func (subtree (t, p), TABLE))) p= path_up (p);
  if ((!is_nil (p)) && (is_func (subtree (t, path_up (p)), TFORMAT)))
    p= path_up (p);
  return p;
}

static void
table_search_coordinates (tree t, path p, int& row, int& col) {
  row= col= 0;
  while (true) {
    if (is_nil (p)) p= path (1);
    if (p == path (0)) p= path (0, 0);
    if (p == path (1)) p= path (N(t)-1, 1);
    if (is_func (t, TFORMAT));
    else if (is_func (t, TABLE)) row= p->item;
    else if (is_func (t, ROW)) col= p->item;
    else return;
    t= t [p->item];
    p= p->next;
  }
}

static path
table_search_cell (tree t, int row, int col) {
  path p;
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * row;
  t= t [row];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * col;
  t= t [col];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  return p;
}

/******************************************************************************
* Get the selection
******************************************************************************/

void
selection_correct (tree t, path i1, path i2, path& o1, path& o2) {
  if (i1 == i2) {
    o1= i1;
    o2= i2;
  }
  else if (is_atom (i1) || is_atom (i2)) {
    if (is_atomic (t)) {
      o1= i1;
      o2= i2;
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
  else if (i1->item == i2->item) {
    selection_correct (t[i1->item], i1->next, i2->next, o1, o2);
    o1= path (i1->item, o1);
    o2= path (i2->item, o2);
  }
  else {
    tree_label l= L(t);
    if ((l==DOCUMENT) || (l==PARA) || (l==CONCAT)) {
      if (is_compound (t[i1->item])) {
	path mid;
	selection_correct (t[i1->item], i1->next, end (t[i1->item]), o1, mid);
	o1= path (i1->item, o1);
      }
      else o1= i1;
      if (is_compound (t[i2->item])) {
	path mid;
	selection_correct (t[i2->item], start(t[i2->item]), i2->next, mid, o2);
	o2= path (i2->item, o2);
      }
      else o2= i2;
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
}

static void
selection_bcorrect (drd_info drd, tree t, path i1, path i2, path& o1, path& o2)
{
  o1= i1; o2= i2;
  if (is_compound (t) && !is_atom (i1) && !is_atom (i2) &&
      i1->item == i2->item) {
    path O1, O2;
    selection_bcorrect (drd, t[i1->item], i1->next, i2->next, O1, O2);
    if (drd->var_without_border (L(t[i1->item])) && (O1->item != O2->item)) {
      o1= path (0);
      o2= path (1);
    }
    else {
      o1= path (i1->item, O1);
      o2= path (i1->item, O2);
    }
  }
}

tree
compute_selection (tree t, path start, path end) {
  int  i1= start->item;
  int  i2= end->item;
  path p1= start->next;
  path p2= end->next;

  if (is_nil (p1) || is_nil (p2)) {
    if (start == path (right_index (t))) return "";
    if (end == path (0)) return "";
    if (start == end) return "";
    if (is_nil (p1) && is_nil (p2)) {
      if (is_compound (t)) return copy (t);
      if (i1>=i2) return "";
      return t->label (i1, i2);
    }
    if (is_compound (t) && (!is_format (t))) return copy (t);
    if (is_nil (p1)) {
      i1= 0;
      p1= (start->item==0? 0: right_index (t[i1]));
    }
    if (is_nil (p2)) {
      i2= N(t)-1;
      p2= (end->item==0? 0: right_index (t[i2]));
    }
  }

  if (i1==i2) return compute_selection (t[i1], p1, p2);
  if (is_compound (t) && (!is_format (t))) return copy (t);

  int i;
  tree r (t, i2-i1+1);
  r[0]     = compute_selection (t[i1], p1, path (right_index (t[i1])));
  r[N(r)-1]= compute_selection (t[i2], path (0), p2);
  for (i=1; i<N(r)-1; i++) r[i]= copy (t[i+i1]);
  return r;
}

path
edit_select_rep::selection_get_subtable (
  int& row1, int& col1, int& row2, int& col2)
{
  path fp= ::table_search_format (et, common (start_p, end_p));
  tree st= subtree (et, fp);
  table_search_coordinates (st, tail (start_p, N(fp)), row1, col1);
  table_search_coordinates (st, tail (end_p, N(fp)), row2, col2);
  if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
  if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }
  table_bound (fp, row1, col1, row2, col2);
  return fp;
}

void
edit_select_rep::selection_get (selection& sel) {
  if (selection_active_table ()) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    tree st= subtree (et, fp);

    int i, j;
    rectangle r (0, 0, 0, 0);
    for (i=row1; i<=row2; i++)
      for (j=col1; j<=col2; j++) {
	path cp= fp * ::table_search_cell (st, i, j);
	sel= eb->find_check_selection (cp * 0, cp * 1);
	if (sel->valid) {
	  rectangles rs= sel->rs;
	  if (r != rectangle (0, 0, 0, 0)) rs= rectangles (r, rs);
	  r= least_upper_bound (rs);
	}
      }
    sel= selection (rectangles (r), fp * 0, fp * 1);
  }
  else {
    path aux_start, aux_end, p_start, p_end;
    selection_bcorrect (drd, et, start_p, end_p, aux_start, aux_end);
    selection_correct (et, aux_start, aux_end, p_start, p_end);
    sel= eb->find_check_selection (p_start, p_end);
  }
}

void
edit_select_rep::selection_get (path& start, path& end) {
  selection sel; selection_get (sel);
  start= sel->start;
  end  = sel->end;
}

path
edit_select_rep::selection_get_start () {
  return start_p;
}

path
edit_select_rep::selection_get_end () {
  return end_p;
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
    path start, end;
    // cout << "Selecting...\n";
    selection_get (start, end);
    // cout << "Between paths: " << start << " and " << end << "\n";
    tree t= ::compute_selection (et, start, end);
    // cout << "Selection : " << t << "\n";
    return simplify_correct (t);
  }
}

path
edit_select_rep::selection_get_path () {
  path start, end;
  selection_get (start, end);
  return common (start, end);
}

/******************************************************************************
* Copy and paste
******************************************************************************/

void
edit_select_rep::selection_raw_set (string key, tree t) {
  (void) ::set_selection (key, t, "", "texmacs");
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
    if (rp < start_p) select (start_p, start_p);
    else select (tp, tp);
  }
  if (is_nil (p)) selection_set_start (tp);
  else if (path_less_eq (end_p, p)) select (p, p);
  else if (rp < p) select (p, end_p);
}

void
edit_select_rep::selection_set_end (path p) {
  if (is_nil (p)) selection_set_end (tp);
  else if (path_less_eq (p, start_p)) select (p, p);
  else if (rp < p) select (start_p, p);
}

void
edit_select_rep::selection_set_paths (path start, path end) {
  if (is_nil (start) || is_nil (end)) selection_set_paths (tp, tp);
  else if (path_less_eq (end, start)) select (start, start);
  else if (rp < start && rp < end) select (start, end);
}

void
edit_select_rep::selection_set (string key, tree t, bool persistant) {
  selecting= shift_selecting= false;
  string mode= get_env_string (MODE);
  string lan = get_env_string (MODE_LANGUAGE (mode));
  tree sel= tuple ("texmacs", t, mode, lan);
  /* TODO: add mode="graphics" somewhere in the context of the <graphics>
     tag. To be done when implementing the different embeddings for
     nicely copying graphics into text, text into graphics, etc. */
  string s;
  if (key == "primary" || key == "mouse") {
    if (selection_export == "verbatim") {
      t= tree (WITH, "TeXmacs", tree (MACRO, "TeXmacs"), t);
      t= tree (WITH, "LaTeX", tree (MACRO, "LaTeX"), t);
      t= tree (WITH, "TeX", tree (MACRO, "TeX"), t);
      t= exec_texmacs (t, tp);
    }
    if (selection_export == "html") t= exec_html (t, tp);
    if (selection_export == "latex") t= exec_latex (t, tp);
    if ((selection_export == "latex") && (mode == "math"))
      t= tree (WITH, "mode", "math", t);
    s= tree_to_generic (t, selection_export * "-snippet");
    s= selection_encode (lan, s);
  }
  if (::set_selection (key, sel, s, selection_export) && !persistant)
    selection_cancel ();
}

void
edit_select_rep::selection_set (tree t) {
  selection_set ("primary", t);
}

void
edit_select_rep::selection_copy (string key) {
  if (inside_active_graphics ()) {
    tree t= as_tree (eval ("(graphics-copy)"));
    selection_set (key, t);
    return;
  }
  if (selection_active_any ()) {
    path old_tp= tp;
    selection sel; selection_get (sel);
    go_to (sel->end);
    tree t= selection_get ();
    go_to (sel->start);
    selection_set (key, t);
    go_to (old_tp);
  }
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
    if ((selection_import == "latex") && (mode == "prog")) mode= "verbatim";
    if ((selection_import == "latex") && (mode == "math")) mode= "latex-math";
    if ((selection_import == "html") && (mode == "prog")) mode= "verbatim";
    string fm= selection_import * "-snippet";
    tree doc= generic_to_tree (selection_decode(lan, as_string(t[1])), fm);
    if (is_func (doc, DOCUMENT, 1)) doc= doc[0]; // temporary fix
    insert_tree (doc);
  }
  if (is_tuple (t, "texmacs", 3)) {
    string mode= get_env_string (MODE);
    string lan = get_env_string (MODE_LANGUAGE (mode));
    if ((mode == "prog") && (t[2] == "math")) {
      tree in= tuple (lan, t[1]);
      tree r= stree_to_tree (call ("plugin-math-input", tree_to_stree (in)));
      insert_tree (r);
    }
    else {
      if ((t[2] != mode) && (t[2] != "src") && (mode != "src") &&
	  ((t[2] == "math") || (mode == "math")))
	insert_tree (tree (WITH, copy (MODE), copy (t[2]), ""), path (2, 0));
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
  if (end_p == start_p) return;
  select (start_p, start_p);
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
    int row1, col1, row2, col2;
    table_search_coordinates (st, tail (p1, N(fp)), row1, col1);
    table_search_coordinates (st, tail (p2, N(fp)), row2, col2);
    if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
    if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }

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
    return;
  }

  if (is_compound (t) && (!is_format (t))) {
    assign (p, "");
    return;
  }

  if ((N(p1) != (N(p)+1)) || (N(p2) != (N(p)+1))) {
    cerr << "t = " << t << "\n";
    cerr << "p = " << p << "\n";
    cerr << "p1= " << p1 << "\n";
    cerr << "p2= " << p2 << "\n";
    FAILED ("invalid cut");
  }

  if (is_atomic (t)) {
    int pos= last_item (p1);
    int nr = last_item (p2)-pos;
    if (nr>0) remove (p1, nr);
  }
  else {
    if ((last_item (p1) != 0) || (last_item (p2) != 1)) {
      cerr << "t = " << t << "\n";
      cerr << "p = " << p << "\n";
      cerr << "p1= " << p1 << "\n";
      cerr << "p2= " << p2 << "\n";
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
      p1= start_p; p2= end_p;
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
        tree sel= compute_selection (et, p1, p2);
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
      up_flag)
    return focus_search (path_up (p), skip_flag, false);
  return p;
}

path
edit_select_rep::focus_get (bool skip_flag) {
  if (!is_nil (focus_p))
    return focus_search (focus_p, skip_flag, false);
  if (selection_active_any ())
    return focus_search (selection_get_path (), skip_flag, false);
  else
    return focus_search (path_up (tp), skip_flag, true);
}
