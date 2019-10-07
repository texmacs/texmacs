
/******************************************************************************
* MODULE     : edit_delete.cpp
* DESCRIPTION: treat deletions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_text.hpp"
#include "tree_traverse.hpp"
#include "analyze.hpp"

bool is_empty_cell (tree t);

/******************************************************************************
* Getting the point where to delete
******************************************************************************/

void
edit_text_rep::get_deletion_point (
  path& p, int& last, int& rix, tree& t, tree& u, bool forward)
{
  // make right-glued positions left-glued
  p= tp;
  if (forward) {
    //cout << HRULE;
    if ((rp < p) && (N (p / rp) >= 2) &&
        is_concat (subtree (et, path_up (p, 2))) &&
        (last_item (p) == right_index (subtree (et, path_up (p)))) &&
        (last_item (path_up (p)) < (N (subtree (et, path_up (p, 2))) - 1)))
      {
        p= path_up (p);
        p= path_inc (p) * start (subtree (et, path_inc (p)), path ());
      }
    //cout << "p= " << p << "\n";
  }

  // get the position where to delete
  last= last_item (p);
  p   = path_up (p);
  t   = subtree (et, p);
  rix = right_index (t);
  //cout << "  t   = " << t << "\n";
  //cout << "  last= " << last << "\n";
  //cout << "  rix = " << rix << "\n";
  while (((forward && (last >= rix)) || ((!forward) && (last == 0))) &&
         (rp < p) && is_format (subtree (et, path_up (p))))
    {
      last= last_item (p);
      p   = path_up (p);
      t   = subtree (et, p);
      rix = N(t) - 1;
      //cout << "  t   = " << t << "\n";
      //cout << "  last= " << last << "\n";
      //cout << "  rix = " << rix << "\n";
    }
  if (rp < p) u= subtree (et, path_up (p));
}

/******************************************************************************
* Normal deletions
******************************************************************************/

static bool
is_multi_paragraph_or_sectional (tree t) {
  if (is_atomic (t)) return false;
  if (is_multi_paragraph (t)) return true;
  eval ("(use-modules (utils library tree) (text text-drd))");
  return as_bool (call ("tree-in?", t, call ("section-tag-list")));
}

void
edit_text_rep::remove_text_sub (bool forward) {
  path p;
  int  last, rix;
  tree t, u;
  get_deletion_point (p, last, rix, t, u, forward);

  // multiparagraph delete
  if (is_document (t)) {
    if ((forward && (last >= rix)) || ((!forward) && (last == 0))) {
      if (rp < p) {
        tree u= subtree (et, path_up (p));
        if (is_func (u, _FLOAT) || is_func (u, WITH) ||
            is_func (u, STYLE_WITH) || is_func (u, VAR_STYLE_WITH) ||
            is_func (u, LOCUS) || is_func (u, INCLUDE) ||
            is_extension (u))
          {
            if (is_extension (u) && (N(u) > 1)) {
              int i, n= N(u);
              bool empty= true;
              for (i=0; i<n; i++)
                empty= empty && ((u[i]=="") || (u[i]==tree (DOCUMENT, "")));
              if (!empty) {
                if (forward) go_to (next_valid (et, tp));
                else go_to (previous_valid (et, tp));
                return;
              }
            }
            if (t == tree (DOCUMENT, "")) {
              if (is_func (u, _FLOAT) ||
                  is_compound (u, "footnote", 1) ||
                  is_compound (u, "footnote-anchor", 2)) {
                assign (path_up (p), "");
                correct (path_up (p, 2));
              }
              else if (is_document (subtree (et, path_up (p, 2))))
                assign (path_up (p), "");
              else assign (path_up (p), tree (DOCUMENT, ""));
              if (is_func (subtree (et, path_up (p, 2)), INACTIVE))
                remove_structure (forward);
            }
            else go_to_border (path_up (p), !forward);
          }
        else if (is_func (u, TABLE) || is_func (u, SUBTABLE) ||
                 is_func (u, CELL) || is_func (u, ROW) ||
                 is_func (u, TFORMAT)) {
          if (t == tree (DOCUMENT, ""))
            back_in_table (u, p, forward);
        }
        else if (is_func (u, DOCUMENT_AT))
          back_in_text_at (u, p, forward);
      }
      return;
    }
    else {
      int l1= forward? last: last-1;
      int l2= forward? last+1: last;
      if (is_multi_paragraph_or_sectional (subtree (et, p * l1)) ||
          is_multi_paragraph_or_sectional (subtree (et, p * l2)))
        {
          if (subtree (et, p * l1) == "") remove (p * l1, 1);
          else {
            if (subtree (et, p * l2) == "") remove (p * l2, 1);
            if (!forward) go_to_end (p * l1);
            else if (last < N (subtree (et, p)) - 1) go_to_start (p * l2);
          }
        }
      else remove_return (p * l1);
    }
    return;
  }

  // deleting text
  if (forward && is_atomic (t) && (last != rix)) {
    language lan= get_env_language ();
    int end= last;
    tm_char_forwards (t->label, end);
    remove (p * last, end-last);
    correct (path_up (p));
    return;
  }

  if ((!forward) && is_atomic (t) && (last != 0)) {
    language lan= get_env_language ();
    int start= last;
    tm_char_backwards (t->label, start);
    remove (p * start, last-start);
    correct (path_up (p));
    return;
  }

  // deletion governed by parent t
  if (last == (forward? 0: 1))
    switch (L(t)) {
    case RAW_DATA:
    case HSPACE:
    case VAR_VSPACE:
    case VSPACE:
    case SPACE:
    case HTAB:
      back_monolithic (p);
      return;
    case AROUND:
    case VAR_AROUND:
    case BIG_AROUND:
      back_around (t, p, forward);
      return;
    case LEFT:
    case MID:
    case RIGHT:
    case BIG:
      back_monolithic (p);
      return;
    case LPRIME:
    case RPRIME:
      back_prime (t, p, forward);
      return;
    case WIDE:
    case VAR_WIDE:
      go_to_border (p * 0, forward);
      return;
    case TFORMAT:
    case TABLE:
    case ROW:
    case CELL:
    case SUBTABLE:
      back_table (p, forward);
      return;
    case WITH:
    case STYLE_WITH:
    case VAR_STYLE_WITH:
    case LOCUS:
      go_to_border (p * (N(t) - 1), forward);
      return;
    case VALUE:
    case QUOTE_VALUE:
    case ARG:
    case QUOTE_ARG:
      if (N(t) == 1) back_monolithic (p);
      else back_general (p, forward);
      return;
    default:
      if (is_compound (t, "separating-space", 1)) back_monolithic (p);
      else if (is_compound (t, "application-space", 1)) back_monolithic (p);
      else back_general (p, forward);
      break;
    }

  // deletion depends on children u
  if (last == (forward? rix: 0)) {
    switch (L (u)) {
    case AROUND:
    case VAR_AROUND:
    case BIG_AROUND:
      back_in_around (u, p, forward);
      return;
    case LONG_ARROW:
      back_in_long_arrow (u, p, forward);
      return;
    case WIDE:
    case VAR_WIDE:
      back_in_wide (u, p, forward);
      return;
    case TREE:
      back_in_tree (u, p, forward);
      return;
    case TFORMAT:
    case TABLE:
    case ROW:
    case CELL:
    case SUBTABLE:
      back_in_table (u, p, forward);
      return;
    case WITH:
    case STYLE_WITH:
    case VAR_STYLE_WITH:
    case LOCUS:
      back_in_with (u, p, forward);
      return;
    default:
      if (is_graphical_text (u))
        back_in_text_at (u, p, forward);
      else if (is_compound (u, "cell-inert") ||
               is_compound (u, "cell-input") ||
               is_compound (u, "cell-output")) {
        tree st= subtree (et, path_up (p, 2));
        back_in_table (u, p, forward);
      }
      else
        back_in_general (u, p, forward);
      break;
    }
  }
}

void
edit_text_rep::empty_document_fix () {
  // FIXME: we might want to call this after arbitrary editing operations
  tree rt= subtree (et, rp);
  if (exists_accessible_inside (rt)) return;
  if (!is_func (rt, DOCUMENT)) {
    insert_node (rt, 0, DOCUMENT);
    rt= subtree (et, rp);
  }
  int n= N(rt);
  insert (rt, n, tree (DOCUMENT, ""));
  go_to (rp * path (n, 0));
}

void
edit_text_rep::remove_text (bool forward) {
  remove_text_sub (forward);
  empty_document_fix ();
}

/******************************************************************************
* Structured deletions
******************************************************************************/

void
edit_text_rep::remove_structure (bool forward) {
  path p;
  int  last, rix;
  tree t, u;
  get_deletion_point (p, last, rix, t, u, forward);

  // multiparagraph delete
  if (!(rp < p)) {
    if (forward) {
      if (last >= rix) return;
      remove_return (path (last));
    }
    else {
      if (last == 0) return;
      remove_return (path (last-1));
    }
    return;
  }

  // deleting text
  if (is_atomic (t) && (last != (forward? rix: 0))) {
    language lan= get_env_language ();
    int start= last, end= last, pos;
    string s= t->label;
    while (true) {
      if (forward) {
        pos= start;
        (void) lan->advance (t, pos);
        if (pos <= last) break;
      }
      else {
        int pos= max (start-1, 0);
        (void) lan->advance (t, pos);
        if (pos < last) break;
      }
      end= pos;
      if (start == 0) break;
      start--;
    }
    if (forward) {
      start= min (start+1, last);
      while ((end < N(s)) && (s[end] == ' ')) end++;
    }
    else while ((start>0) && (s[start-1] == ' ')) start--;
    if (end>start) {
      remove (p * start, end-start);
      correct (path_up (p));
    }
    return;
  }

  // deleting structure
  if (forward) {
    if (is_concat (t) && (last < rix)) {
      remove (p * (last+1), 1);
      correct (path_up (p));
    }
    else if (is_compound (t) && (last == 0)) {
      assign (p, "");
      correct (path_up (p));
    }
    else remove_structure_upwards ();
  }
  else {
    if (last==1) {
      if (!is_concat (u)) assign (p, "");
      else remove (p, 1);
      correct (path_up (p));
    }
    else remove_structure_upwards ();
  }
}

/******************************************************************************
* Deletion of an object
******************************************************************************/

void
edit_text_rep::remove_structure_upwards () {
  path p= path_up (tp);
  while ((rp < p) && is_format (subtree (et, path_up (p)))) p= path_up (p);
  if (!(rp < p)) return;
  int last= last_item (p);
  p= path_up (p);
  tree st= subtree (et, p);
  if (is_func (st, AROUND, 3) ||
      is_func (st, VAR_AROUND, 3) ||
      is_func (st, BIG_AROUND, 2))
    pre_remove_around (p);
  bool recurse=
    is_func (st, TFORMAT) || is_func (st, TABLE) ||
    is_func (st, ROW) || is_func (st, CELL) ||
    is_compound (st, "shown") ||
    drd->var_without_border (L(st));
  remove (p * (last+1), N(st)-(last+1));
  remove (p * 0, last);

  do {
    remove_node (p * 0);
    last= last_item (p);
    p= path_up (p);
    st= subtree (et, p);
  } while (is_mod_active_once (st));

  if (is_document (st) && is_document (st[last])) {
    int very_last= 0;
    if ((N(tp) >= N(p)+2) && (tp[N(p)] == last)) very_last= tp[N(p)+1];
    tree left = st[last] (0, very_last);
    tree right= st[last] (very_last+1, N(st[last]));
    remove (p * path (last, very_last+1), N(st[last])- (very_last+1));
    remove (p * path (last, 0), very_last);
    remove_node (p * path (last, 0));
    insert (p * (last+1), right);
    insert (p * last, left);
  }
  else correct (p);

  if (recurse) remove_structure_upwards ();
}
