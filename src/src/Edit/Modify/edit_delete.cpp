
/******************************************************************************
* MODULE     : edit_delete.cpp
* DESCRIPTION: treat deletions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_text.hpp"

/******************************************************************************
* Delete backwards
******************************************************************************/

void
edit_text_rep::remove_backwards () {
  path p= tp;
  int  last;
  tree t, u;

  /********************* get the position where to delete ********************/
  
  do {
    last= last_item (p);
    p   = path_up (p);
    t   = subtree (et, p);
  } while ((last==0) && (!nil (p)) && is_format (subtree (et, path_up (p))));
  if (is_document (t)) {
    if (last==0) {
      if (!nil(p)) {
	tree u= subtree (et, path_up (p));
	if (is_func (u, _FLOAT) || is_func (u, WITH) || is_extension (u)) {
	  if (is_extension (u) && (N(u) > (1+d_exp))) {
	    int i, n= N(u);
	    bool empty= true;
	    for (i=d_exp; i<n; i++)
	      empty= empty && ((u[i]=="") || (u[i]==tree (DOCUMENT, "")));
	    if (!empty) {
	      if (last_item (p) == d_exp) go_to (start (et, path_up (p)));
	      else go_to (end (et, path_dec (p)));
	      return;
	    }
	  }
	  if (t == tree (DOCUMENT, "")) {
	    if (is_func (u, _FLOAT) || is_compound (u, "footnote", 1)) {
	      assign (path_up (p), "");
	      correct (path_up (p, 2));
	    }
	    else if (is_document (subtree (et, path_up (p, 2))))
	      assign (path_up (p), "");
	    else assign (path_up (p), tree (DOCUMENT, ""));
	  }
	  else go_to (start (et, path_up (p)));
	}
      }
    }
    else {
      if (is_multi_paragraph (subtree (et, p * (last-1))) ||
	  is_multi_paragraph (subtree (et, p * last)))
	{
	  if (subtree (et, p * (last-1)) == "") remove (p * (last-1), 1);
	  else {
	    if (subtree (et, p * last) == "") remove (p * last, 1);
	    go_to (end (et, p * (last-1)));
	  }
	}
      else remove_return (p * (last-1));
    }
    return;
  }
  u= subtree (et, path_up (p));

  // cout << "Backspace\n-----------------------";
  // cout << "t= " << t << "\n";
  // cout << "u= " << u << "\n";
  // cout << "p= " << p << "\n";

  /**************************** deleting text ********************************/

  if (is_atomic (t) && (last != 0)) {
    language lan= get_env_language ();
    int start= last;
    if (lan->enc->token_backward (t->label, start))
      fatal_error ("bad cursor position in string",
		   "edit_text_rep::backspace");
    remove (p * start, last-start);
    correct (path_up (p));
    return;
  }

  /************************ deletion governed by t ***************************/

  if (last==1)
    switch (L(t)) {
    case SURROUND:
      go_to (end (et, p * 2));
      return;
    case FORMAT:
    case HSPACE:
    case VSPACE_BEFORE:
    case VSPACE_AFTER:
    case SPACE:
    case HTAB:
      if (!is_concat (subtree (et, path_up (p)))) assign (p, "");
      else remove (p, 1);
      correct (path_up (p));
      return;
    case SPLIT:
      back_dynamic (p);
      return;
    case MOVE:
    case RESIZE:
      back_dynamic (p);
      return;
    case _FLOAT:
      go_to (end (et, p * 2));
      return;
    case REPEAT:
    case DECORATE_ATOMS:
    case DECORATE_LINES:
    case DECORATE_PAGES:
      back_dynamic (p);
      return;

    case WITH_LIMITS:
    case LINE_BREAK:
    case NEW_LINE:
    case LINE_SEP:
    case NEXT_LINE:
    case NO_BREAK:
    case NO_FIRST_INDENT:
    case YES_FIRST_INDENT:
    case NO_FIRST_INDENT_AFTER:
    case YES_FIRST_INDENT_AFTER:
    case PAGE_BREAK_BEFORE:
    case PAGE_BREAK:
    case NO_PAGE_BREAK_BEFORE:
    case NO_PAGE_BREAK_AFTER:
    case NEW_PAGE_BEFORE:
    case NEW_PAGE:
    case NEW_DOUBLE_PAGE_BEFORE:
    case NEW_DOUBLE_PAGE:
      if (!is_concat (subtree (et, path_up (p)))) assign (p, "");
      else remove (p, 1);
      correct (path_up (p));
      return;

    case GROUP:
      go_to (end (et, p * 0));
      return;
    case LEFT:
    case MIDDLE:
    case RIGHT:
    case BIG:
      if (!is_concat (u)) assign (p, "");
      else remove (p, 1);
      correct (path_up (p));
      return;
    case LEFT_PRIME:
    case RIGHT_PRIME:
      back_prime (t, p);
      return;
    case BELOW:
    case ABOVE:
      go_to (end (et, p * 1));
      return;
    case LEFT_SUB:
    case LEFT_SUP:
    case RIGHT_SUB:
    case RIGHT_SUP:
      go_to (end (et, p * 0));
      return;
    case FRAC:
      go_to (end (et, p * 1));
      return;
    case SQRT:
    case WIDE:
    case WIDE_UNDER:
    case NEG:
      go_to (end (et, p * 0));
      return;
    case TREE:
      go_to (end (et, p * (N(t)-1)));
      return;
    case OLD_MATRIX:
    case OLD_TABLE:
    case OLD_MOSAIC:
      back_dynamic (p);
      return;

    case TABLE_FORMAT:
      back_table (p);
      return;
    case TABLE_WITH:
    case CELL_WITH:
    case TABLE_MARKER:
      back_dynamic (p);
      return;
    case TABLE:
    case ROW:
    case CELL:
    case SUB_TABLE:
      back_table (p);
      return;

    case ASSIGN:
      back_dynamic (p);
      return;
    case WITH:
      go_to (end (et, p * (N(t)-1)));
      return;
    case EXPAND:
    case VAR_EXPAND:
      back_expand (p);
      return;
    case HIDE_EXPAND:
      back_hide_expand (p);
      return;
    case APPLY:
    case INCLUDE:
      back_dynamic (p);
      return;
    case MACRO:
    case FUNCTION:
    case EVAL:
      back_dynamic (p);
      return;
    case PROVIDES:
    case VALUE:
    case ARGUMENT:
      if (!is_concat (u)) assign (p, "");
      else remove (p, 1);
      correct (path_up (p));
      return;
    case QUOTE:
    case DELAY:
    case HOLD:
    case RELEASE:
      back_dynamic (p);
      return;

    case OR:
    case XOR:
    case AND:
    case NOT:
    case PLUS:
    case MINUS:
    case TIMES:
    case OVER:
    case DIVIDE:
    case MODULO:
    case MERGE:
    case LENGTH:
    case RANGE:
    case NUMBER:
    case _DATE:
    case TRANSLATE:
    case FIND_FILE:
    case IS_TUPLE:
    case LOOK_UP:
    case EQUAL:
    case UNEQUAL:
    case LESS:
    case LESSEQ:
    case GREATER:
    case GREATEREQ:
    case IF:
    case VAR_IF:
    case CASE:
    case WHILE:
    case EXTERN:
    case AUTHORIZE:
      back_dynamic (p);
      return;

    case INACTIVE:
    case ACTIVE:
    case VAR_INACTIVE:
    case VAR_ACTIVE:
      go_to (end (et, p * (N(t)-1)));
      return;
    case SYMBOL:
    case LATEX:
    case HYBRID:
      back_dynamic (p);
      return;
    case TUPLE:
    case ATTR:
      go_to (end (et, p * (N(t)-1)));
      return;
    case COLLECTION:
    case ASSOCIATE:
    case LABEL:
    case REFERENCE:
    case PAGEREF:
    case WRITE:
    case SPECIFIC:
    case HYPERLINK:
    case ACTION:
    case TAG:
    case MEANING:
      back_dynamic (p);
      return;

    case GRAPHICS:
    case SUPERPOSE:
    case TEXT_AT:
    case _POINT:
    case LINE:
    case CLINE:
    case SPLINE:
    case VAR_SPLINE:
    case CSPLINE:
    case FILL:
      back_dynamic (p);
      return;
    case POSTSCRIPT:
      back_dynamic (p);
      return;
    default:
      if (L(t) >= START_EXTENSIONS) back_extension (p);
      break;
    }

  /************************* deletion depends on u ***************************/

  if (last==0) {
    switch (L (u)) {
    case SURROUND:
      back_in_dynamic (u, p, 3);
      return;
    case SPLIT:
      back_in_dynamic (u, p);
      return;
    case MOVE:
      back_in_dynamic (u, p, 3);
      return;
    case RESIZE:
      back_in_dynamic (u, p, 5);
      return;
    case _FLOAT:
      back_in_dynamic (u, p, 3);
      return;
    case REPEAT:
    case DECORATE_ATOMS:
    case DECORATE_LINES:
    case DECORATE_PAGES:
      back_in_dynamic (u, p, 2);
      return;

    case GROUP:
      back_in_math (u, p);
      return;
    case LEFT:
    case MIDDLE:
    case RIGHT:
    case BIG:
      fatal_error ("cursor should not be inside big symbol",
		   "edit_text_rep::backspace");
      return;
    case BELOW:
    case ABOVE:
    case LEFT_SUB:
    case LEFT_SUP:
    case RIGHT_SUB:
    case RIGHT_SUP:
    case FRAC:
      back_in_math (u, p);
      return;
    case SQRT:
      back_in_dynamic (u, p);
      return;
    case WIDE:
    case WIDE_UNDER:
      back_in_math_accent (u, p);
      return;
    case NEG:
      back_in_math (u, p);
      return;
    case TREE:
      back_in_tree (u, p);
      return;
    case OLD_MATRIX:
    case OLD_TABLE:
    case OLD_MOSAIC:
    case OLD_MOSAIC_ITEM:
      back_in_dynamic (u, p);
      return;

    case TABLE_FORMAT:
      back_in_table (u, p);
      return;
    case TABLE_WITH:
      back_in_dynamic (u, p, 2);
      return;
    case CELL_WITH:
      back_in_dynamic (u, p, 6);
      return;
    case TABLE_MARKER:
      back_in_dynamic (u, p);
      return;
    case TABLE:
    case ROW:
    case CELL:
    case SUB_TABLE:
      back_in_table (u, p);
      return;

    case ASSIGN:
      back_in_dynamic (u, p);
      return;
    case WITH:
      back_in_with (u, p);
      return;
    case EXPAND:
    case VAR_EXPAND:
    case HIDE_EXPAND:
      back_in_expand (u, p);
      return;
    case APPLY:
    case INCLUDE:
      back_in_dynamic (u, p);
      return;
    case MACRO:
    case FUNCTION:
      back_in_dynamic (u, p);
      return;
    case EVAL:
    case PROVIDES:
    case VALUE:
    case ARGUMENT:
    case QUOTE:
    case DELAY:
    case HOLD:
    case RELEASE:
      back_in_dynamic (u, p);
      return;

    case OR:
    case XOR:
    case AND:
      back_in_dynamic (u, p, 2);
      return;
    case NOT:
      back_in_dynamic (u, p);
      return;
    case PLUS:
    case MINUS:
    case TIMES:
    case OVER:
    case DIVIDE:
    case MODULO:
    case MERGE:
      back_in_dynamic (u, p, 2);
      return;
    case LENGTH:
      back_in_dynamic (u, p);
      return;
    case RANGE:
      back_in_dynamic (u, p, 3);
      return;
    case NUMBER:
    case _DATE:
      back_in_dynamic (u, p);
      return;
    case TRANSLATE:
      back_in_dynamic (u, p, 3);
      return;
    case FIND_FILE:
      back_in_dynamic (u, p, 2);
      return;
    case IS_TUPLE:
      back_in_dynamic (u, p);
      return;
    case LOOK_UP:
    case EQUAL:
    case UNEQUAL:
    case LESS:
    case LESSEQ:
    case GREATER:
    case GREATEREQ:
    case IF:
    case VAR_IF:
    case CASE:
    case WHILE:
      back_in_dynamic (u, p, 2);
      return;
    case EXTERN:
      back_in_dynamic (u, p, 1);
      return;
    case AUTHORIZE:
      back_in_dynamic (u, p, 2);
      return;

    case INACTIVE:
    case ACTIVE:
    case VAR_INACTIVE:
    case VAR_ACTIVE:
    case SYMBOL:
    case LATEX:
    case HYBRID:
      back_in_dynamic (u, p);
      return;
    case TUPLE:
      back_in_dynamic (u, p);
      return;
    case ATTR:
      back_in_dynamic (u, p, 1, 2);
      return;
    case COLLECTION:
      back_in_dynamic (u, p);
      return;
    case ASSOCIATE:
      back_in_dynamic (u, p, 2);
      return;
    case LABEL:
    case REFERENCE:
    case PAGEREF:
    case WRITE:
      back_in_dynamic (u, p);
      return;
    case SPECIFIC:
    case HYPERLINK:
    case ACTION:
    case TAG:
    case MEANING:
      back_in_dynamic (u, p, 2);
      return;

    case GRAPHICS:
    case SUPERPOSE:
    case TEXT_AT:
    case _POINT:
    case LINE:
    case CLINE:
    case SPLINE:
    case VAR_SPLINE:
    case CSPLINE:
    case FILL:
      back_in_dynamic (u, p);
      return;
    case POSTSCRIPT:
      back_in_dynamic (u, p, 7);
      return;
    default:
      if (L(u) >= START_EXTENSIONS) back_in_extension (u, p);
      break;
    }
  }
}

/******************************************************************************
* Delete forwards
******************************************************************************/

void
edit_text_rep::remove_forwards () {
  path p= tp;
  int  last, rix;
  tree t, u;

  /********************* get the position where to delete ********************/
  
  if ((last_item (p) == 1) && (!atom (p)) &&
      is_compound (subtree (et, path_up (p))) &&
      is_format (subtree (et, path_up (p, 2))))
    p= path_up (p);
  do {
    last= last_item (p);
    p   = path_up (p);
    t   = subtree (et, p);
    rix = is_atomic (t)? N(t->label): (N(t)-1);
  } while ((last >= rix) &&
	   (!nil (p)) && is_format (subtree (et, path_up (p))));
  if (is_document (t)) {
    if (last >= rix) {
      /* Not yet implemented */
    }
    else {
      if (is_multi_paragraph (subtree (et, p * last)) ||
	  is_multi_paragraph (subtree (et, p * (last+1))))
	{
	  if (subtree (et, p * last) == "") remove (p * last, 1);
	  else {
	    if (subtree (et, p * (last+1)) == "") remove (p * (last+1), 1);
	    go_to (start (et, p * (last+1)));
	  }
	}
      else remove_return (p * last);
    }
    return;
  }
  u= subtree (et, path_up (p));

  // cout << "Delete\n-----------------------";
  // cout << "t= " << t << "\n";
  // cout << "u= " << u << "\n";
  // cout << "p= " << p << "\n";

  /**************************** deleting text ********************************/

  if (is_atomic (t) && (last != rix)) {
    language lan= get_env_language ();
    int end= last;
    if (lan->enc->token_forward (t->label, end))
      fatal_error ("bad cursor position in string",
		   "edit_text_rep::backspace");
    remove (p * last, end-last);
    correct (path_up (p));
    return;
  }

  /* not yet implemented */
}

/******************************************************************************
* Backward deletion of an object
******************************************************************************/

path left_match (tree t, path p, tree which, int level= 0);
path right_match (tree t, path p, tree which, int level= 0);

void
edit_text_rep::remove_structure_backwards () {
  path p= tp;
  int  last;
  tree t, u;

  /********************* get the position where to delete ********************/
  
  do {
    last= last_item (p);
    p   = path_up (p);
    t   = subtree (et, p);
  } while ((last==0) && (!nil (p)) && is_format (subtree (et, path_up (p))));
  if (nil (p)) {
    if (last==0) return;
    remove_return (path (last-1));
    return;
  }
  u= subtree (et, path_up (p));

  /**************************** deleting text ********************************/

  if (is_atomic (t) && (last!=0)) {
    language lan= get_env_language ();
    int start= last, end= last;
    string s= t->label;
    while (true) {
      int pos= max (start-1, 0);
      (void) lan->advance (s, pos);
      if (pos < last) break;
      end= pos;
      if (start == 0) break;
      start--;
    }
    while ((start>0) && (s[start-1] == ' ')) start--;
    if (end>start) {
      remove (p * start, end-start);
      correct (path_up (p));
    }
    return;
  }

  /************************** deleting structure *****************************/

  if (last==1) {
    if (!is_concat (u)) assign (p, "");
    else remove (p, 1);
    correct (path_up (p));
  }
  else remove_structure_upwards ();
}

/******************************************************************************
* Forward deletion of an object
******************************************************************************/

void
edit_text_rep::remove_structure_forwards () {
  path p= tp;
  int  last, rix;
  tree t, u;

  /********************* get the position where to delete ********************/

  if ((last_item (p) == 1) && (!atom (p)) &&
      is_compound (subtree (et, path_up (p))) &&
      is_format (subtree (et, path_up (p, 2))))
    p= path_up (p);
  do {
    last= last_item (p);
    p   = path_up (p);
    t   = subtree (et, p);
    rix = is_atomic (t)? N(t->label): (N(t)-1);
  } while ((last >= rix) &&
	   (!nil (p)) && is_format (subtree (et, path_up (p))));
  if (nil (p)) {
    if (last >= rix) return;
    remove_return (path (last));
    return;
  }
  u= subtree (et, path_up (p));

  /**************************** deleting text ********************************/

  if (is_atomic (t) && (last != rix)) {
    language lan= get_env_language ();
    int start= last, end= last;
    string s= t->label;
    while (true) {
      int pos= start;
      (void) lan->advance (s, pos);
      if (pos <= last) break;
      end= pos;
      if (start == 0) break;
      start--;
    }
    start= min (start+1, last);
    while ((end < N(s)) && (s[end] == ' ')) end++;
    if (end>start) {
      remove (p * start, end-start);
      correct (path_up (p));
    }
    return;
  }

  /************************** deleting structure *****************************/
  
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

/******************************************************************************
* Deletion of an object
******************************************************************************/

void
edit_text_rep::remove_structure_upwards () {
  path p= path_up (tp);
  while ((!nil (p)) && is_format (subtree (et, path_up (p)))) p= path_up (p);
  if (nil (p)) return;
  int last= last_item (p);
  p= path_up (p);
  tree st= subtree (et, p);
  bool recurse=
    is_func (st, TABLE_FORMAT) || is_func (st, TABLE) ||
    is_func (st, ROW) || is_func (st, CELL);
  remove (p * (last+1), N(st)-(last+1));
  remove (p * 0, last);

  do {
    rem_unary (p);
    last= last_item (p);
    p= path_up (p);
    st= subtree (et, p);
  } while (is_func (st, INACTIVE, 1));

  if (is_document (st) && is_document (st[last])) {
    int very_last= 0;
    if ((N(tp) >= N(p)+2) && (tp[N(p)] == last)) very_last= tp[N(p)+1];
    tree left = st[last] (0, very_last);
    tree right= st[last] (very_last+1, N(st[last]));
    remove (p * path (last, very_last+1), N(st[last])- (very_last+1));
    remove (p * path (last, 0), very_last);
    rem_unary (p * last);
    insert (p * (last+1), right);
    insert (p * last, left);
  }
  else correct (p);

  if (recurse) remove_structure_upwards ();
}
