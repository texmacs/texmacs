
/******************************************************************************
* MODULE     : env_inactive.cpp
* DESCRIPTION: rewrite inactive markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "env.hpp"

static tree
subvar (tree var, int i) {
  tree svar= copy (var);
  return svar << as_string (i);
}

static bool
is_long (tree t) {
  switch (L(t)) {
  case DOCUMENT:
  case INCLUDE:
  case TFORMAT:
  case TABLE:
    return true;
  case CONCAT:
  case ROW:
    return false;
  case SURROUND:
  case ASSIGN:
  case DATOMS:
  case DLINES:
  case DPAGES:
  case WITH:
  case MARK:
  case MACRO:
  case XMACRO:
  case CELL:
    return is_long (t[N(t)-1]);
  default:
    if (L(t) < START_EXTENSIONS) return false;
    else {
      int i, n= N(t);
      for (i=0; i<n; i++)
	if (is_long (t[i]))
	  return true;
      return false;
    }
  }
}

static bool
is_long_arg (tree t) {
  switch (L(t)) {
  case TWITH:
  case CWITH:
  case ROW:
    return true;
  default:
    return is_long (t);
  }
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var, bool block, bool flush) {
  if (is_atomic (t)) {
    if (src_style == STYLE_SCHEME)
      return tree (CONCAT,
		   tree (WITH, COLOR, "blue", "``"),
		   var,
		   tree (WITH, COLOR, "blue", "''"));
    return var;
  }
  else if (is_concat (t) &&
	   (src_special > SPECIAL_RAW) &&
	   (src_compact != COMPACT_NONE))
    {
      int i, n= N(t);
      tree r (CONCAT, n);
      for (i=0; i<n; i++)
	r[i]= rewrite_inactive (t[i], subvar (var, i), false, false);
      return r;
    }
  else if (is_document (t) &&
	   (block || (src_compact == COMPACT_NONE)) &&
	   (src_special > SPECIAL_RAW) &&
	   (src_compact != COMPACT_ALL))
    {
      int i, n= N(t);
      tree r (DOCUMENT, n);
      for (i=0; i<n; i++)
	r[i]= rewrite_inactive (t[i], subvar (var, i), true, flush || (i<n-1));
      return r;
    }
  else if ((src_compact == COMPACT_ALL) ||
	   ((!block) && (src_compact != COMPACT_NONE)) ||
	   (!is_long (t)) && (src_compact != COMPACT_NONE))
    {
      int i, n= N(t);
      tree r (INLINE_TAG, n+1);
      r[0]= as_string (L(t));
      for (i=0; i<n; i++)
	r[i+1]= rewrite_inactive (t[i], subvar (var, i), false, false);
      return tree (MARK, var, r);
    }
  else {
    string op= as_string (L(t));
    tree doc (DOCUMENT);
    int i=0, n= N(t);
    bool compact= (src_compact < COMPACT_INLINE);
 
    for (i=0; i<n; i++) {
      tree next;
      if ((!compact) || is_long_arg (t[i])) {
	if (i==0) doc << tree (OPEN_TAG, op);
	bool big= (src_close >= CLOSE_LONG);
	next= rewrite_inactive (t[i], subvar (var, i), true, big);
	next= compound ("indent", next);
	i++;
      }

      int start= i;
      for (; i<n; i++)
	if ((!compact) || is_long_arg (t[i])) break;
      int end= i;
      tree_label l= MIDDLE_TAG;
      if (start == 0) l= OPEN_TAG;
      if (end == n) l= CLOSE_TAG;
      tree u (l, end - start + 1);
      u[0]= op;
      for (i=0; i<end-start; i++)
	u[i+1]= rewrite_inactive (t[start+i], subvar (var, start+i),
				  false, false);
      i= end-1;
      compact= (src_compact < COMPACT_INLINE_START);

      if (start==0) doc << u;
      else {
	if (src_close < CLOSE_LONG)
	  doc << tree (SURROUND, "", u, next);
	else doc << next << u;
      }
    }

    if (flush) doc= tree (SURROUND, "", compound ("rightflush"), doc);
    return tree (MARK, var, doc);
  }
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var) {
  tree r= rewrite_inactive (t, var, true, true);
  if (is_multi_paragraph (r)) {
    r= tree (WITH, PAR_PAR_SEP, "0fn", r);
    r= tree (SURROUND,
	     tree (VAR_VSPACE, "0.5fn"),
	     tree (VSPACE, "0.5fn"),
	     r);
  }
  return r;
}
