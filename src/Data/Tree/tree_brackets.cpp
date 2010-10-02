
/******************************************************************************
* MODULE     : tree_brackets.cpp
* DESCRIPTION: upgrade to tree with matching brackets
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_brackets.hpp"

#ifdef UPGRADE_BRACKETS

/******************************************************************************
* Tokenize mathematical concats and recomposition
******************************************************************************/

static array<tree>
concat_tokenize (tree t) {
  array<tree> r;
  if (is_atomic (t)) {
    language lan= math_language ("std-math");
    int i= 0;
    while (i<N(t->label)) {
      int start= i;
      (void) lan->advance (t, i);
      r << t->label (start, i);
    }
  }
  else if (is_concat (t))
    for (int i=0; i<N(t); i++)
      r << concat_tokenize (t[i]);
  else if (is_func (t, BIG, 1) && t[0] == "."); // NOTE: discard old <big|.>
  else r << t;
  return r;
}

static tree
concat_recompose (array<tree> a) {
  array<tree> r;
  string s;
  for (int i=0; i<N(a); i++)
    if (is_atomic (a[i])) s << a[i]->label;
    else {
      if (s != "") r << s;
      r << a[i];
      s= "";
    }
  if (s != "") r << s;
  if (N(r) == 0) return "";
  else if (N(r) == 1) return r[0];
  else return tree (CONCAT, r);
}

/******************************************************************************
* Determine symbol type
******************************************************************************/

#define SYMBOL_DELETED           -1
#define SYMBOL_BASIC              0
#define SYMBOL_PREFIX             1
#define SYMBOL_POSTFIX            2
#define SYMBOL_INFIX              3
#define SYMBOL_SEPARATOR          4
#define SYMBOL_OPEN_BIG           5
#define SYMBOL_CLOSE_BIG          6
#define SYMBOL_OPEN               7
#define SYMBOL_MIDDLE             8
#define SYMBOL_CLOSE              9
#define SYMBOL_PROBABLE_OPEN     10
#define SYMBOL_PROBABLE_MIDDLE   11
#define SYMBOL_PROBABLE_CLOSE    12
#define SYMBOL_DUBIOUS_OPEN      13
#define SYMBOL_DUBIOUS_MIDDLE    14
#define SYMBOL_DUBIOUS_CLOSE     15

static int
bracket_type (language lan, tree t) {
  if (is_atomic (t)) {
    int pos= 0;
    text_property prop= lan->advance (t, pos);
    switch (prop->op_type) {
    case OP_PREFIX:
      return SYMBOL_PREFIX;
    case OP_POSTFIX:
      return SYMBOL_POSTFIX;
    case OP_INFIX:
    case OP_LEFT_ASS_INFIX:
    case OP_RIGHT_ASS_INFIX:
    case OP_ASS_INFIX:
      return SYMBOL_INFIX;
    case OP_SEPARATOR:
      return SYMBOL_SEPARATOR;
    case OP_OPENING_BRACKET:
      return SYMBOL_PROBABLE_OPEN;
    case OP_MIDDLE_BRACKET:
      return SYMBOL_PROBABLE_MIDDLE;
    case OP_CLOSING_BRACKET:
      return SYMBOL_PROBABLE_CLOSE;
    default:
      return SYMBOL_BASIC;
    }
  }
  else if (is_func (t, OPEN)) return SYMBOL_OPEN;
  else if (is_func (t, MIDDLE)) return SYMBOL_MIDDLE;
  else if (is_func (t, CLOSE)) return SYMBOL_CLOSE;
  else if (is_func (t, BIG, 1) && t[0] == ".") return SYMBOL_CLOSE_BIG;
  else if (is_func (t, BIG)) return SYMBOL_OPEN_BIG;
  // TODO: extra bracket markup from vdh.ts and elsewhere
  else return SYMBOL_BASIC;
}

static void
downgrade_dubious (array<int>& tp) {
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN && tp[i] <= SYMBOL_PROBABLE_CLOSE) {
      if (i == 0 ||
	  tp[i-1] == SYMBOL_PREFIX ||
	  tp[i-1] == SYMBOL_INFIX ||
	  tp[i-1] == SYMBOL_SEPARATOR)
	{
	  if (tp[i] == SYMBOL_PROBABLE_MIDDLE) tp[i]= SYMBOL_DUBIOUS_MIDDLE;
	  if (tp[i] == SYMBOL_PROBABLE_CLOSE) tp[i]= SYMBOL_DUBIOUS_CLOSE;
	}
      if (i == N(tp)-1 ||
	  tp[i+1] == SYMBOL_POSTFIX ||
	  tp[i+1] == SYMBOL_INFIX ||
	  tp[i+1] == SYMBOL_SEPARATOR)
	{
	  if (tp[i] == SYMBOL_PROBABLE_OPEN) tp[i]= SYMBOL_DUBIOUS_OPEN;
	  if (tp[i] == SYMBOL_PROBABLE_MIDDLE) tp[i]= SYMBOL_DUBIOUS_MIDDLE;
	}
    }
}

static void
upgrade_probable (array<int>& tp) {
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN && tp[i] <= SYMBOL_DUBIOUS_CLOSE) {
      if (i == 0 ||
	  tp[i-1] == SYMBOL_PREFIX ||
	  tp[i-1] == SYMBOL_INFIX ||
	  tp[i-1] == SYMBOL_SEPARATOR)
	tp[i]= SYMBOL_PROBABLE_OPEN;
      if (i == N(tp)-1 ||
	  tp[i+1] == SYMBOL_POSTFIX ||
	  tp[i+1] == SYMBOL_INFIX ||
	  tp[i+1] == SYMBOL_SEPARATOR)
	tp[i]= SYMBOL_PROBABLE_CLOSE;
    }
}

/******************************************************************************
* Heuristic determination of several bracket notations
******************************************************************************/

static void
detect_french_interval (array<tree> a, array<int>& tp) {
  upgrade_probable (tp);
  int last_open= -1, last_comma= -1;
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_SEPARATOR) {
      if (a[i] == "," && last_comma == -1) last_comma= i;
      else last_open= last_comma= -1;
    }
    else if (tp[i] >= SYMBOL_OPEN) {
      if (tp[i] == SYMBOL_OPEN || tp[i] == SYMBOL_PROBABLE_OPEN) {
	last_open= i;
	last_comma= -1;
      }
      else if (tp[i] == SYMBOL_CLOSE || tp[i] == SYMBOL_PROBABLE_CLOSE) {
	if (last_open != -1 && last_comma != -1) {
	  tp[last_open]= SYMBOL_OPEN;
	  tp[i]= SYMBOL_CLOSE;
	}
	else last_open= last_comma= -1;
      }
      else if (tp[i] == SYMBOL_MIDDLE || tp[i] == SYMBOL_PROBABLE_MIDDLE);
      else last_open= last_comma= -1;
    }
}

static void
detect_absolute (array<tree> a, array<int>& tp, bool insist) {
  upgrade_probable (tp);
  int last_open= -1;
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_SEPARATOR) last_open= -1;
    else if (tp[i] >= SYMBOL_OPEN) {
      if (tp[i] == SYMBOL_PROBABLE_OPEN ||
	  (last_open == -1 && tp[i] == SYMBOL_PROBABLE_MIDDLE))
	last_open= i;
      else if (tp[i] == SYMBOL_PROBABLE_CLOSE ||
	       tp[i] == SYMBOL_DUBIOUS_CLOSE ||
	       (last_open != -1 && tp[i] == SYMBOL_PROBABLE_MIDDLE))
	{
	  if (a[i] == a[last_open])
	    if (insist ||
		(a[last_open] == SYMBOL_PROBABLE_OPEN) ||
		(a[i] == SYMBOL_PROBABLE_CLOSE))
	      {
		tp[last_open]= SYMBOL_OPEN;
		tp[i]= SYMBOL_CLOSE;
	      }
	  last_open= -1;
	}
      else last_open= -1;
    }
}

static void
detect_probable (array<tree> a, array<int>& tp) {
  upgrade_probable (tp);
  int last_open= -1;
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_OPEN) {
      if (tp[i] == SYMBOL_OPEN || tp[i] == SYMBOL_PROBABLE_OPEN)
	last_open= i;
      else if (tp[i] == SYMBOL_CLOSE || tp[i] == SYMBOL_PROBABLE_CLOSE) {
	if (last_open != -1) {
	  tp[last_open]= SYMBOL_OPEN;
	  tp[i]= SYMBOL_CLOSE;
	}
	else last_open= -1;
      }
      else if (tp[i] == SYMBOL_MIDDLE || tp[i] == SYMBOL_PROBABLE_MIDDLE);
      else last_open= -1;
    }
}

static void
detect_missing (array<tree> a, array<int>& tp) {
}

/******************************************************************************
* 
******************************************************************************/

static void
simplify_matching (array<tree>& a, array<int>& tp, bool french, int level) {
  int last_open= -1;
  for (int i=0; i<N(tp); i++) {
    if (tp[i] == SYMBOL_OPEN) last_open= i;
    else if (tp[i] >= SYMBOL_PROBABLE_OPEN) last_open= -1;
    else if (tp[i] == SYMBOL_CLOSE && last_open != -1) {
      array<tree> b= range (a, last_open+1, i);
      b= upgrade_brackets (b, french, level+1);
      tree body= concat_recompose (b);
      a[last_open]= tree (AROUND, a[last_open], body, i);
      tp[last_open]= SYMBOL_BASIC;
      for (int j= last_open+1; j<=i; j++) tp[j]= SYMBOL_DELETED;
      last_open= -1;
    }
  }

  array<tree> a2;
  array<int> tp2;
  for (int i=0; i<N(tp); i++)
    if (tp[i] != SYMBOL_DELETED) {
      a2 << a[i];
      tp2 << t[i];
    }

  if (N(tp2) != N(tp)) {
    a= a2;
    tp= tp2;
    simplify_matching (a, tp, french, level);
  }
}

static array<tree>
upgrade_brackets (array<tree> t, bool french, int level) {
  
}

/******************************************************************************
* Master routines
******************************************************************************/

static tree
upgrade_brackets (tree t, bool math, bool french) {
  if (math && (is_atomic (t) || is_concat (t))) {
    array<tree> a= concat_tokenize (t);
    a= upgrade_brackets (a, french);
    return concat_recompose (a);
  }
  else return t;
}

tree
upgrade_brackets (drd_info drd, tree t, bool math, bool french) {
  if (is_atomic (t))
    return upgrade_brackets (t, math, french);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      // TODO: mode
      // TODO: check type != ADHOC, RAW.
      // (in particular: do not enter AROUND, LEFT, MIDDLE, ...)
      r[i]= upgrade_brackets (drd, t[i], math, french);
    }
    return upgrade_brackets (r, math, french);
  }
}

#endif
