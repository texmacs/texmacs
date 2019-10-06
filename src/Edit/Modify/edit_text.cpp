
/******************************************************************************
* MODULE     : edit_text.cpp
* DESCRIPTION: modification of text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_text.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_text_rep::edit_text_rep () {}
edit_text_rep::~edit_text_rep () {}

/******************************************************************************
* Correction routines for the edit tree
******************************************************************************/

void
edit_text_rep::correct_concat (path p, int done) {
  tree t (subtree (et, p));
  if (L(t) != CONCAT) {
    failed_error << "The tree was '" << t << "'\n";
    FAILED ("concat expected");
  }

  int i, n= N(t);
  if (n == 0) { assign (p, string ("")); return; }
  if (n == 1) { remove_node (p * 0); return; }
  for (i=done; i<n; i++) {
    if (t[i] == "") {
      remove (p * i, 1);
      correct_concat (p, i);
      return;
    }
    if ((i<n-1) && is_atomic (t[i]) && is_atomic (t[i+1])) {
      join (p * i);
      correct_concat (p, i);
      return;
    }
    if (is_concat (t[i])) {
      insert_node (p * 0, CONCAT);
      split (p * path (0, i));
      split (p * path (1, 1));
      remove_node (p * path (1, 0));
      if (subtree (et, p * 0) == tree (CONCAT)) remove (p * 0, 1);
      else join (p * 0);
      if (subtree (et, p * 1) == tree (CONCAT)) remove (p * 1, 1);
      else join (p * 0);
      remove_node (p * 0);
      correct_concat (p, max (i-1, 0));
      return;
    }
    else if (is_multi_paragraph (t[i]) &&
	     is_document (subtree (et, path_up (p))))
      {
	if ((i+1)<n) {
	  split (p * (i+1));
	  correct_concat (path_inc (p));
	}
	if (i==0) remove_node (p * 0);
	else {
	  split (p * i);
	  remove_node (path_inc (p) * 0);
	  correct_concat (p);
	}
	return;
      }
  }
}

void
edit_text_rep::correct (path p) {
  tree t (subtree (et, p));
  if (L(t) == CONCAT) correct_concat (p);
  if (t == "") correct (path_up (p));
}

/******************************************************************************
* Inserting line breaks (text-mode only)
******************************************************************************/

bool
edit_text_rep::pure_line (path p) {
  p= path_up (p);
  tree st= subtree (et, path_up (p));
  return
    is_document (st) ||
    ((is_func (st, WITH) || is_func (st, LOCUS) ||
      is_func (st, CANVAS) ||
      is_func (st, ORNAMENT) || is_func (st, ART_BOX) ||
      is_func (st, WITH_PACKAGE)) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    (is_extension (st) && (last_item (p) >= 0) && pure_line (p));
}

bool
edit_text_rep::accepts_return (path p) {
  tree st= subtree (et, path_up (p));
  return
    is_document (st) ||
    (is_func (st, SURROUND, 3) && (last_item (p) == 2)) ||
    (is_func (st, _FLOAT) && (last_item (p) == (N(st)-1))) ||
    (is_func (st, DATOMS) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    (is_func (st, MACRO) && (last_item (p) == (N(st)-1))) ||
    (is_func (st, XMACRO, 2) && (last_item (p) == 1)) ||
    ((is_func (st, WITH) || is_mod_active (st) ||
      is_func (st, STYLE_WITH) || is_func (st, VAR_STYLE_WITH) ||
      is_func (st, LOCUS) || is_func (st, WITH_PACKAGE) ||
      is_func (st, CANVAS)) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    ((is_func (st, ORNAMENT) || is_func (st, ART_BOX)) &&
     (last_item (p) == 0) && pure_line (p)) ||
    (is_extension (st) && (last_item (p) >= 0) && pure_line (p));
}

bool
edit_text_rep::insert_return () {
  if (accepts_return (path_up (tp)))
    insert_node (path_up (tp) * 0, CONCAT);
  path p= path_up (tp, 2);
  if (!is_concat (subtree (et, p))) return true;
  if (!accepts_return (p)) return true;
  if (!is_document (subtree (et, path_up (p)))) {
    insert_node (p * 0, DOCUMENT);
    p= path_up (tp, 2);
  }

  if (is_atomic (subtree (et, path_up (tp)))) split (tp);
  if (last_item (tp)==right_index (subtree (et, path_up (tp))))
    split (path_inc (path_up (tp)));
  else split (path_up (tp));
  go_to (correct_cursor (et, path_inc (p) * 0));
  correct_concat (p);
  correct_concat (path_inc (p));
  return false;
}

void
edit_text_rep::remove_return (path p) {
  if (!is_document (subtree (et, path_up (p))))
    FAILED ("parent is not a document");

  if (!is_concat (subtree (et, p)))
    insert_node (p * 0, CONCAT);
  if (!is_concat (subtree (et, path_inc (p))))
    insert_node (path_inc (p) * 0, CONCAT);
  join (p);
  correct_concat (p);
}

/******************************************************************************
* Inserting a simple formula or tree
******************************************************************************/

path
edit_text_rep::prepare_for_insert () {
  path p = path_up (tp);
  int  l = last_item (tp);
  tree st= subtree (et, p);

  if ((!is_document (st)) && is_multi_paragraph (st)) {
    if (!is_document (subtree (et, path_up (p))))
      insert_node (p * 0, DOCUMENT);
    else {
      if (l == 0) {
	insert (p, tree (DOCUMENT, ""));
	go_to (p * 0);
      }
      else {
	insert (path_inc (p), tree (DOCUMENT, ""));
	go_to (path_inc (p) * 0);
      }
    }
    return prepare_for_insert ();
  }

  if ((rp < p) && is_concat (subtree (et, path_up (p)))) {
    if (l==0) return p;
    if (is_compound (st) || (l==N(st->label))) return path_inc (p);
    split (tp);
    return path_inc (p);
  }
  
  insert_node (p * 0, CONCAT);
  if (is_atomic (st) && (l!=0) && (l!=N(st->label))) {
    split (p * path (0, l));
    return p * 1;
  }
  return p * ((l==0)? 0: 1);
}

void
edit_text_rep::insert_tree (tree t, path p_in_t) {
  if (!as_bool (call ("like-emacs?"))) selection_cut ("none");
  if (is_atomic (t) && (p_in_t == end (t)) &&
      is_atomic (subtree (et, path_up (tp))))
    insert (tp, t);
  else if (is_document (t)) {
    if (subtree (et, path_up (tp)) == "" &&
        accepts_return (path_up (tp)) &&
        !is_func (subtree (et, path_up (tp, 2)), DOCUMENT))
      insert_node (path_up (tp) * 0, DOCUMENT);
    if (insert_return ()) return;
    path p= search_parent_upwards (DOCUMENT);
    bool empty= (subtree (et, p) == "");
    if (empty) remove (p, 1);
    insert (p, t);
    path q= path_add (p, p_in_t->item) * p_in_t->next;
    go_to (correct_cursor (et, q));
    remove_return (path_dec (p));
    if (!empty) remove_return (path_add (p, N(t)-2));
  }
  else if (is_multi_paragraph (t)) {
    if (subtree (et, path_up (tp)) == "" &&
        accepts_return (path_up (tp)) &&
        !is_func (subtree (et, path_up (tp, 2)), DOCUMENT))
      insert_node (path_up (tp) * 0, DOCUMENT);
    if (make_return_after ()) return;
    path p= search_parent_upwards (DOCUMENT);
    if (subtree (et, p) == "") remove (p, 1);
    insert (p, tree (DOCUMENT, t));
    go_to (correct_cursor (et, p * p_in_t));
  }
  else {
    path p= prepare_for_insert ();
    if (!is_concat (t)) { t= tree (CONCAT, t); p_in_t= path (0, p_in_t); }
    insert (p, t);
    path q= path_add (p, p_in_t->item) * p_in_t->next;
    go_to (correct_cursor (et, q));
    correct_concat (path_up (p));
  }
}

void
edit_text_rep::insert_tree (tree t) {
  t= copy (simplify_correct (t));
  insert_tree (t, end (t));
}

void
edit_text_rep::var_insert_tree (tree t, path p_in_t) {
  if (selection_active_any ()) {
    selection_cut ("primary");
    insert_tree (t, p_in_t);
    selection_paste ("primary");
  }
  else insert_tree (t, p_in_t);
}

/******************************************************************************
* Spaces
******************************************************************************/

static string
get_unit (tree t) {
  int i;
  string s= as_string (t);
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  return s (i, N(s));
}

static string
get_quantity (tree t) {
  int i;
  string s= as_string (t);
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  return s (0, i);
}

void
edit_text_rep::make_space (tree u) {
  if (is_atomic (u)) return;
  tree t= subtree (et, path_up (tp));

  int i, n= N(u);
  bool flag= is_func (t, L (u), n);
  for (i=0; i<n; i++) {
    if (!flag) break;
    string u1= get_unit (t[i]);
    string u2= get_unit (t[0]);
    double x1= as_double (get_quantity (t[i]));
    double x2= as_double (get_quantity (u[i]));
    flag= flag && (u1==u2) && ((x1*x2)>0);
  }
  
  if (flag) {
    for (i=0; i<n; i++) {
      double w1= as_double (get_quantity (t[i]));
      double w2= as_double (get_quantity (u[i]));
      string sum= as_string (w1+w2) * get_unit (t[i]);
      assign (path_up (tp) * i, sum);
    }
  }
  else insert_tree (u);
}

void
edit_text_rep::make_space (string w) {
  make_space (tree (SPACE, w));
}

void
edit_text_rep::make_space (string w, string y1, string y2) {
  insert_tree (tree (SPACE, w, y1, y2));
}

void
edit_text_rep::make_hspace (string s) {
  make_space (tree (HSPACE, s));
}

void
edit_text_rep::make_hspace (string smin, string sdef, string smax) {
  make_space (tree (HSPACE, smin, sdef, smax));
}

void
edit_text_rep::make_vspace_before (string s) {
  make_space (tree (VAR_VSPACE, s));
}

void
edit_text_rep::make_vspace_before (string smin, string sdef, string smax) {
  make_space (tree (VAR_VSPACE, smin, sdef, smax));
}

void
edit_text_rep::make_vspace_after (string s) {
  make_space (tree (VSPACE, s));
}

void
edit_text_rep::make_vspace_after (string smin, string sdef, string smax) {
  make_space (tree (VSPACE, smin, sdef, smax));
}

/******************************************************************************
* Insert formatting objects and miscellaneous
******************************************************************************/

void
edit_text_rep::make_htab (string spc) {
  insert_tree (tree (HTAB, spc));
}

void
edit_text_rep::make_image (
  string file_name, bool link, string w, string h, string x, string y)
{
  url image= url_unix (file_name);
  string type= "";
  tree t (IMAGE);
  if (link) {
    if (is_rooted (image, "default")) image= reroot (image, "file");
    // here we convert the filename to cork-universal because it is exposed in the document, in the <image> tag
    t << utf8_to_cork (as_string (image, URL_STANDARD));
  }
  else {
    string s;
    tree vim= verbatim (utf8_to_cork (as_string (image)));
    load_string (relative (get_name (), image), s, false);
    if (s == "") {
      set_message (concat ("File '", vim, "' not found"), "make image");
      return;
    }
    type= suffix (image);
    if (type == "") {
      set_message (concat ("File '", vim, "' is not an image"), "make image");
      return;
    }
    t << tuple (tree (RAW_DATA, s), type);
  }
  t << tree (w) << tree (h) << tree (x) << tree (y);
  insert_tree (t);
}
