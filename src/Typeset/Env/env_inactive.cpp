
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

#define COMPACT_ALL           0
#define COMPACT_INLINE_ARGS   1
#define COMPACT_INLINE_START  2
#define COMPACT_INLINE        3
#define COMPACT_NONE          4

static bool normal_format = true;
static bool compact_close = false;
static int  compact_mode  = COMPACT_INLINE_START;

static tree
subvar (tree var, int i) {
  tree svar= copy (var);
  return svar << as_string (i);
}

inline tree
blue (tree t) {
  return tree (WITH, "color", "blue", t);
}

static tree
inline_open (string var) {
  return blue ("(" * var);
}

static tree
inline_middle (string var, bool first) {
  (void) var; (void) first;
  return blue (", ");
}

static tree
inline_close (string var) {
  (void) var;
  return blue (")");
}

static tree
long_open (string var) {
  return blue ("(\\" * var);
}

static tree
long_middle (string var) {
  return blue ("(|" * var);
}

static tree
long_close (string var) {
  return blue ("(/" * var);
}

static tree
short_middle (string var, bool first) {
  (void) var; (void) first;
  return blue (", ");
}

static tree
short_close (string var) {
  (void) var;
  return blue (")");
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var, bool flush) {
  if (is_atomic (t)) return var;
  else if (is_concat (t) &&
	   normal_format &&
	   (compact_mode != COMPACT_NONE))
    {
      int i, n= N(t);
      tree r (CONCAT, n);
      for (i=0; i<n; i++)
	r[i]= rewrite_inactive (t[i], subvar (var, i), false);
      return r;
    }
  else if (is_document (t) &&
	   normal_format &&
	   (compact_mode != COMPACT_ALL))
    {
      int i, n= N(t);
      tree r (DOCUMENT, n);
      for (i=0; i<n; i++)
	r[i]= rewrite_inactive (t[i], subvar (var, i), flush || (i<n-1));
      return r;
    }
  else if ((compact_mode == COMPACT_ALL) ||
	   (!is_multi_paragraph (t)) && (compact_mode != COMPACT_NONE))
    {
      string op= as_string (L(t));
      tree r (CONCAT, inline_open (op));
      int i, n= N(t);
      for (i=0; i<n; i++) {
	r << inline_middle (op, i==0);
	r << rewrite_inactive (t[i], subvar (var, i), false);
      }
      r << inline_close (op);
      return tree (MARK, var, r);
    }
  else {
    string op= as_string (L(t));
    tree doc (DOCUMENT);
    int i=0, n= N(t);
    bool compact= (compact_mode < COMPACT_INLINE);

    if ((!compact) || ((n>0) && is_multi_paragraph (t[0]))) {
      tree tag= long_open (op);
      if (compact_mode < COMPACT_INLINE)
	tag= tree (CONCAT, tag, short_close (op));
      doc << tag;
    }

    for (i=0; i<n; i++) {
      if (compact && (!is_multi_paragraph (t[i]))) {
	int start= i;
	for (; i<n; i++)
	  if (is_multi_paragraph (t[i])) break;
	int end= i;
	tree hor (CONCAT);
	if (start==0) hor << long_open (op);
	else if (end == n) hor << long_close (op);
	else hor << long_middle (op);
	for (i=start; i<end; i++) {
	  tree next= rewrite_inactive (t[i], subvar (var, i), false);
	  hor << short_middle (op, i==start) << next;
	}
	hor << short_close (op);
	doc << hor;
	compact= (compact_mode < COMPACT_INLINE_START);
	i= end-1;
      }
      else {
	tree tag;
	tree next= rewrite_inactive (t[i], subvar (var, i), !compact_close);
	if (i<n-1) tag= long_middle (op);
	else tag= long_close (op);
	if (compact_mode < COMPACT_INLINE)
	  tag= tree (CONCAT, tag, short_close (op));
	if (compact_close)
	  doc << tree (SURROUND, "", tag, compound ("indent", next));
	else doc << compound ("indent", next) << tag;
      }
    }

    if (flush) doc= tree (SURROUND, "", compound ("rightflush"), doc);
    return tree (MARK, var, doc);
  }
}

tree
edit_env_rep::rewrite_inactive (tree t, tree var) {
  string compact= get_string (SRC_COMPACT);
  if (compact == "all") compact_mode= COMPACT_ALL;
  else if (compact == "inline args") compact_mode= COMPACT_INLINE_ARGS;
  else if (compact == "normal") compact_mode= COMPACT_INLINE_START;
  else if (compact == "inline") compact_mode= COMPACT_INLINE;
  else compact_mode= COMPACT_NONE;
  string close= get_string (SRC_CLOSE);
  compact_close= (close == "compact");
  tree r= rewrite_inactive (t, var, true);
  return r;
}
