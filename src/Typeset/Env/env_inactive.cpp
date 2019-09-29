
/******************************************************************************
* MODULE     : env_inactive.cpp
* DESCRIPTION: rewrite inactive markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

bool is_long (tree t);

bool
is_long_arg (tree t, int i) {
  // FIXME: should go into the DRD
  int n= N(t);
  switch (L(t)) {
  case DOCUMENT:
  case VAR_INCLUDE:
  case TFORMAT:
  case TABLE:
    return true;
  case CONCAT:
    return false;
  case SURROUND:
    if (i == 2) return true;
    break;
  case ROW:
    return is_long (t[i]);
  case ASSIGN:
  case DATOMS:
  case DLINES:
  case DPAGES:
  case WITH:
  case MARK:
  case MACRO:
  case XMACRO:
  case CELL:
  case EVAL:
  case QUOTE:
  case QUASI:
  case QUASIQUOTE:
  case UNQUOTE:
  case VAR_UNQUOTE:
    if (i == n-1) return is_long (t[i]);
    break;
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    if (i == n-1) {
      for (i=0; i<n-1; i+=2)
	if (t[i] == SRC_COMPACT) {
	  if (t[i+1] == "none") return true;
	  if (t[i+1] == "all")  return false;
	}
      return is_long (t[i]);
    }
    break;
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return is_multi_paragraph (t[i]);
  case LOCUS:
  case CANVAS:
    if (i == n-1) return is_long (t[i]);
    break;
  case ORNAMENT:
  case ART_BOX:
    if (i == 0) return is_long (t[i]);
    break;
  default:
    break;
  }

  tree u= t[i];
  switch (L(u)) {
  case TWITH:
  case CWITH:
  case ROW:
    return true;
  default:
    if (L(t) < START_EXTENSIONS) return false;
    return is_long (u);
  }
}

bool
is_long (tree t) {
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_long_arg (t, i))
	return true;
  }
  return false;
}

/******************************************************************************
* For syntactic coloring
******************************************************************************/

static tree
highlight (tree t, tree orig, int kind) {
  (void) orig;
  switch (kind) {
  case TYPE_INVALID:
    return compound ("src-unknown", t);
  case TYPE_REGULAR:
    return compound ("src-regular", t);
  case TYPE_ADHOC:
    return compound ("src-textual", t);
  case TYPE_RAW:
    return compound ("src-error", t);
  case TYPE_VARIABLE:
    return compound ("src-var", t);
  case TYPE_ARGUMENT:
    return compound ("src-arg", t);
  case TYPE_BINDING:
    return compound ("src-regular", t);
  case TYPE_BOOLEAN:
    return compound ("src-numeric", t);
  case TYPE_INTEGER:
    return compound ("src-numeric", t);
  case TYPE_STRING:
    return compound ("src-textual", t);
  case TYPE_LENGTH:
    return compound ("src-length", t);
  case TYPE_NUMERIC:
    return compound ("src-numeric", t);
  case TYPE_CODE:
    return compound ("src-tt", t);
  case TYPE_IDENTIFIER:
    return compound ("src-tt", t);
  case TYPE_URL:
    return compound ("src-tt", t);
  case TYPE_COLOR:
    return compound ("src-textual", t);
  case TYPE_GRAPHICAL:
    return compound ("src-regular", t);
  case TYPE_POINT:
    return compound ("src-regular", t);
  case TYPE_CONSTRAINT:
    return compound ("src-regular", t);
  case TYPE_GRAPHICAL_ID:
    return compound ("src-var", t);
  case TYPE_ANIMATION:
    return compound ("src-regular", t);
  case TYPE_DURATION:
    return compound ("src-length", t);
  case TYPE_FONT_SIZE:
    return compound ("src-numeric", t);
  case TYPE_OBSOLETE:
    return compound ("src-unknown", t);
  case TYPE_UNKNOWN:
    return compound ("src-unknown", t);
  case TYPE_ERROR:
    return compound ("src-error", t);
  default:
    return compound ("src-error", t);
  }
}

/******************************************************************************
* Compute rendering of inactive markup
******************************************************************************/

tree
edit_env_rep::rewrite_inactive_arg (
  tree t, tree var, int i, bool block, bool flush)
{
  tree r= subvar (var, i);
  if ((inactive_mode == INACTIVE_INLINE_RECURSE) ||
      (inactive_mode == INACTIVE_BLOCK_RECURSE))
    {
      if (N (recover_env) > 0) {
	int j;
	tree recover= copy (recover_env), old_recover= recover_env;
	for (j=0; j<N(recover); j+=2) {
	  string var= recover[j]->label;
	  recover[j+1]= read (var);
	  write_update (var, recover_env[j+1]);
	}
	recover_env= tuple ();
	r= rewrite_inactive (t[i], r, block, flush);
	recover_env= old_recover;
	for (j=0; j<N(recover); j+=2)
	  write_update (recover[j]->label, recover[j+1]);
      }
      else r= rewrite_inactive (t[i], r, block, flush);
    }
  return highlight (r, t[i], drd->get_type_child (t, i));
}

tree
edit_env_rep::rewrite_inactive_raw_data (
  tree t, tree var, bool block, bool flush)
{
  (void) t; (void) block;
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
  if ((N(t) == 1) && is_atomic (t[0]) &&
      src_style != STYLE_SCHEME && src_special >= SPECIAL_NORMAL)
    {
      tree r= highlight (subvar (var, 0), t[0],
        inactive_mode == INACTIVE_INLINE_ERROR ||
	inactive_mode == INACTIVE_BLOCK_ERROR ?
	TYPE_INVALID: TYPE_VARIABLE);
      return tree (MARK, var, r);
  }
  return rewrite_inactive_default (t, var, block, flush);
}

tree
edit_env_rep::rewrite_inactive_arg (
  tree t, tree var, bool block, bool flush)
{
  if ((N(t) == 1) && is_atomic (t[0]) &&
      src_style != STYLE_SCHEME && src_special >= SPECIAL_NORMAL)
    {
      tree r= highlight (subvar (var, 0), t[0],
        inactive_mode == INACTIVE_INLINE_ERROR ||
	inactive_mode == INACTIVE_BLOCK_ERROR ?
	TYPE_INVALID: TYPE_ARGUMENT);
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
edit_env_rep::rewrite_inactive_style_with (
  tree t, tree var, bool block, bool flush, bool once)
{
  int i, n= N(t);
  tree recover= tuple ();
  for (i=0; i<n-1; i+=2)
    if (is_atomic (t[i])) {
      recover << t[i] << read (t[i]->label);
      write_update (t[i]->label, t[i+1]);
    }
  if (once) recover_env= recover;
  tree r= rewrite_inactive (t[n-1], subvar (var, n-1), block, flush);
  for (i=0; i<N(recover); i+=2)
    write_update (recover[i]->label, recover[i+1]);
  if (once) recover_env= tuple ();
  return tree (MARK, var, r);
}

tree
edit_env_rep::rewrite_inactive_active (
  tree t, tree var, bool block, bool flush)
{
  tree st= t[0];
  tree svar= subvar (var, 0);
  int i, n= N(st);
  tree r (st, n);
  bool mp= is_multi_paragraph (st);
  for (i=0; i<n; i++) {
    bool smp= mp && is_long_arg (st, i);
    if (is_func (st, WITH) && (i<n-1)) r[i]= subvar (svar, i);
    else r[i]= rewrite_inactive_arg (st, svar, i, block && smp, flush && smp);
  }
  return tree (MARK, var, r);
}

tree
edit_env_rep::rewrite_inactive_var_active (
  tree t, tree var, bool block, bool flush)
{
  (void) block;
  tree r= tree (WITH, MODE, copy (env [MODE]), subvar (var, 0));
  if (flush &&
      (src_compact != COMPACT_ALL) &&
      (is_multi_paragraph (t[0]) || (src_compact == COMPACT_NONE)))
    r= tree (SURROUND, "", compound ("right-flush"), r);
  return tree (MARK, var, r);
}

tree
edit_env_rep::rewrite_inactive_hybrid (
  tree t, tree var, bool block, bool flush)
{
  if (is_atomic (t[0]) && (src_special >= SPECIAL_NORMAL)) {
    int i, n= N(t);
    tree r (INLINE_TAG, n);
    r[0]= tree (CONCAT, "\\",
		highlight (subvar (var, 0), t[0], TYPE_VARIABLE));
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
      op= highlight (subvar (var, 0), t[0], TYPE_VARIABLE);
    }
  if (inactive_mode == INACTIVE_INLINE_ERROR ||
      inactive_mode == INACTIVE_BLOCK_ERROR)
    op= highlight (op, "", TYPE_INVALID);

  if ((N(t) == d) ||
      (src_compact == COMPACT_ALL) ||
      ((!block) && (src_compact != COMPACT_NONE)) ||
      ((!is_long (t)) && (src_compact != COMPACT_NONE)))
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
	next= compound ("indent*", next);
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

    if (flush) doc= tree (SURROUND, "", compound ("right-flush"), doc);
    return tree (MARK, var, doc);
  }
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var, bool block, bool flush) {
  if (is_atomic (t)) {
    if (src_style == STYLE_SCHEME)
      return tree (CONCAT,
		   tree (WITH, COLOR, src_tag_color, "\20"),
		   var,
		   tree (WITH, COLOR, src_tag_color, "\21"));
    return var;
  }
  switch (L(t)) {
  case UNINIT:
    if (src_special >= SPECIAL_NORMAL)
      return tree (MARK, var, highlight ("?", "", TYPE_INVALID));
    else return rewrite_inactive_default (t, var, block, flush);
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
  case STYLE_WITH:
    return rewrite_inactive_style_with (t, var, block, flush, true);
  case VAR_STYLE_WITH:
    return rewrite_inactive_style_with (t, var, block, flush, false);
  case STYLE_ONLY:
    return rewrite_inactive_active (t, var, block, flush);
  case VAR_STYLE_ONLY:
    return rewrite_inactive_var_active (t, var, block, flush);
  case ACTIVE:
    return rewrite_inactive_active (t, var, block, flush);
  case VAR_ACTIVE:
    return rewrite_inactive_var_active (t, var, block, flush);
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
  // cout << "Rewrite inactive " << t << ", " << var << "\n";
  recover_env= tuple ();
  bool block= (inactive_mode >= INACTIVE_BLOCK_RECURSE);
  tree r= rewrite_inactive (t, var, block, block);
  if (is_multi_paragraph (r)) {
    r= tree (WITH, PAR_PAR_SEP, "0fn", r);
    r= tree (SURROUND, "", tree (VSPACE, "0.5fn"), r);
  }
  if ((inactive_mode == INACTIVE_INLINE_RECURSE) ||
      (inactive_mode == INACTIVE_BLOCK_RECURSE))
    r= tree (WITH, MODE, "src", r);
  // cout << "---> " << r << "\n\n";
  return r;
}
