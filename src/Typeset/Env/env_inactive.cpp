
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

/******************************************************************************
* Test whether a tree (argument) should be rendered in compact format or not.
******************************************************************************/

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
is_long_arg (tree t, int i) {
  if (is_document (t)) return true;
  tree u= t[i];
  switch (L(u)) {
  case TWITH:
  case CWITH:
  case ROW:
    return true;
  default:
    return is_long (u);
  }
}

/******************************************************************************
* For syntactic coloring
******************************************************************************/

static string
arg_type (tree t, int i) {
  int n= N(t);
  switch (L(t)) {
  case ASSIGN:
  case DRD_PROPS:
  case VALUE:
    if (i == 0) return "id";
    else return "";
  case WITH:
    if ((i<n-1) && ((i&1)==0)) return "id";
    else return "";
  case TWITH:
  case CWITH:
    if (i<n-2) return "integer";
    else if (i==n-2) return "id";
    else return "";
  case MACRO:
    if (i<n-1) return "arg";
    else return "";
  case XMACRO:
    if (i==0) return "arg";
    else return "";
  case ARG:
    if (i==0) return "arg";
    else return "integer";
    break;
  case MAP_ARGS:
    if (i<2) return "id";
    else if (i==2) return "arg";
    else return "";
  case SPECIFIC:
    if ((i==1) && (t[0] != "texmacs") &&
	(t[0] != "screen") && (t[0] != "printer"))
      return "tt";
    else return "";
  case ACTION:
    if (i==n-1) return "tt";
    else return "";
  default:
    return "";
  }
}

static tree
highlight (tree t, string kind) {
  if (kind == "") return t;
  else if (kind == "id")
    return tree (WITH, COLOR, "dark green", t);
  else if (kind == "arg")
    return tree (WITH, COLOR, "brown", t);
  else if (kind == "tt")
    return tree (WITH, MODE, "text", FONT_FAMILY, "tt", t);
  else if (kind == "integer")
    return tree (WITH, COLOR, "dark grey", t);
  return t;
}

/******************************************************************************
* Compute rendering of inactive markup
******************************************************************************/

tree
edit_env_rep::rewrite_inactive_arg (
  tree t, tree var, int i, bool block, bool flush)
{
  tree r= rewrite_inactive (t[i], subvar (var, i), block, flush);
  return highlight (r, arg_type (t, i));
}

tree
edit_env_rep::rewrite_inactive_raw_data (
  tree t, tree var, bool block, bool flush)
{
  return rewrite_inactive_default (tree (RAW_DATA), var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_document (
  tree t, tree var, bool block, bool flush)
{
  if ((block || (src_compact == COMPACT_NONE)) &&
      (src_special > SPECIAL_RAW) &&
      (src_compact != COMPACT_ALL))
    {
      int i, n= N(t);
      tree r (DOCUMENT, n);
      for (i=0; i<n; i++)
	r[i]= rewrite_inactive_arg (t, var, i, true, flush || (i<n-1));
      return r;
    }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_concat (
  tree t, tree var, bool block, bool flush)
{
  if ((src_special > SPECIAL_RAW) && (src_compact != COMPACT_NONE)) {
    int i, n= N(t);
    tree r (CONCAT, n);
    for (i=0; i<n; i++)
      r[i]= rewrite_inactive_arg (t, var, i, false, false);
    return r;
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_value (
  tree t, tree var, bool block, bool flush)
{
  if ((N(t) == 1) && is_atomic (t[0]) && src_special >= SPECIAL_NORMAL) {
    tree name= rewrite_inactive_arg (t, var, 0, false, false);
    tree r= highlight (name, "id");
    return tree (MARK, var, r);
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_arg (
  tree t, tree var, bool block, bool flush)
{
  if ((N(t) == 1) && is_atomic (t[0]) && src_special >= SPECIAL_NORMAL) {
    tree name= rewrite_inactive_arg (t, var, 0, false, false);
    tree r= highlight (name, "arg");
    return tree (MARK, var, r);
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_symbol (
  tree t, tree var, bool block, bool flush)
{
  if ((N(t) == 1) && is_atomic (t[0]) && (src_special >= SPECIAL_NORMAL)) {
    tree r (INLINE_TAG, subvar (var, 0));
    return tree (MARK, var, r);
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_hybrid (
  tree t, tree var, bool block, bool flush)
{
  if (is_atomic (t[0]) && (src_special >= SPECIAL_NORMAL)) {
    int i, n= N(t);
    tree r (INLINE_TAG, n);
    r[0]= tree (CONCAT, "\\", highlight (subvar (var, 0), "id"));
    for (i=1; i<n; i++)
      r[i]= rewrite_inactive_arg (t, var, i, false, false);
    return tree (MARK, var, r);
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_default (
  tree t, tree var, bool block, bool flush)
{
  int i, d= 0, n= N(t);
  tree op= as_string (L(t));
  if ((L(t) == COMPOUND) &&
      is_atomic (t[0]) &&
      (src_special >= SPECIAL_NORMAL))
    {
      d = 1;
      op= highlight (subvar (var, 0), "id");
    }
  if ((src_compact == COMPACT_ALL) ||
      ((!block) && (src_compact != COMPACT_NONE)) ||
      (!is_long (t)) && (src_compact != COMPACT_NONE))
    {
      tree r (INLINE_TAG, n+1-d);
      r[0]= op;
      for (i=d; i<n; i++)
	r[i+1-d]= rewrite_inactive_arg (t, var, i, false, false);
      return tree (MARK, var, r);
    }
  else {
    tree doc (DOCUMENT);
    bool compact= (src_compact < COMPACT_INLINE);
 
    for (i=d; i<n; i++) {
      tree next;
      if ((!compact) || is_long_arg (t, i)) {
	if (i==d) doc << tree (OPEN_TAG, op);
	next= rewrite_inactive_arg (t, var, i, true, src_close >= CLOSE_LONG);
	next= compound ("indent", next);
	i++;
      }

      int start= i;
      for (; i<n; i++)
	if ((!compact) || is_long_arg (t, i)) break;
      int end= i;
      tree_label l= MIDDLE_TAG;
      if (end == n) l= CLOSE_TAG;
      if (start == d) l= OPEN_TAG;
      tree u (l, end - start + 1);
      u[0]= op;
      for (i=0; i<end-start; i++)
	u[i+1]= rewrite_inactive_arg (t, var, start+i, false, false);
      i= end-1;
      compact= (src_compact < COMPACT_INLINE_START);

      if (start==d) doc << u;
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
edit_env_rep::rewrite_inactive (tree t, tree var, bool block, bool flush) {
  if (is_atomic (t)) {
    if (src_style == STYLE_SCHEME)
      return tree (CONCAT,
		   tree (WITH, COLOR, "blue", "``"),
		   var,
		   tree (WITH, COLOR, "blue", "''"));
    return var;
  }
  switch (L(t)) {
  case RAW_DATA:
    return rewrite_inactive_raw_data (t, var, block, flush);
  case DOCUMENT:
    return rewrite_inactive_document (t, var, block, flush);
  case CONCAT:
    return rewrite_inactive_concat (t, var, block, flush);
  case VALUE:
    return rewrite_inactive_value (t, var, block, flush);
  case ARG:
    return rewrite_inactive_arg (t, var, block, flush);
  case SYMBOL:
    return rewrite_inactive_symbol (t, var, block, flush);
  case HYBRID:
    return rewrite_inactive_hybrid (t, var, block, flush);
  default:
    return rewrite_inactive_default (t, var, block, flush);
  }
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var) {
  tree r= rewrite_inactive (t, var, true, true);
  if (is_multi_paragraph (r)) {
    r= tree (WITH, PAR_PAR_SEP, "0fn", r);
    r= tree (SURROUND, "", tree (VSPACE, "0.5fn"), r);
  }
  return r;
}
