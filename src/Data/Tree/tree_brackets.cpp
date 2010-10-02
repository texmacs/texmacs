
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
#include "language.hpp"

static array<tree> upgrade_brackets (array<tree> a, int level);

/******************************************************************************
* Tokenize mathematical concats and recomposition
******************************************************************************/

static array<tree>
concat_tokenize (tree t) {
  array<tree> r;
  if (is_atomic (t)) {
    static language lan= math_language ("std-math");
    int i= 0;
    while (i<N(t->label)) {
      int start= i;
      (void) lan->advance (t, i);
      r << tree (t->label (start, i));
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
      if (s != "") r << tree (s);
      r << a[i];
      s= "";
    }
  if (s != "") r << tree (s);
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
bracket_type (tree t) {
  static language lan= math_language ("std-math");
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
  else if (is_func (t, LEFT)) return SYMBOL_OPEN;
  else if (is_func (t, MID)) return SYMBOL_MIDDLE;
  else if (is_func (t, RIGHT)) return SYMBOL_CLOSE;
  else if (is_func (t, BIG, 1) && t[0] == ".") return SYMBOL_CLOSE_BIG;
  else if (is_func (t, BIG)) return SYMBOL_OPEN_BIG;
  // TODO: extra bracket markup from vdh.ts and elsewhere
  else return SYMBOL_BASIC;
}

static array<int>
bracket_types (array<tree> a) {
  array<int> tp (N(a));
  for (int i=0; i<N(a); i++)
    tp[i]= bracket_type (a[i]);
  return tp;
}

static array<int>
downgrade_dubious (array<int> tp_in) {
  array<int> tp= copy (tp_in);
  // FIXME: combinations such as OPEN MIDDLE
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
  return tp;
}

static array<int>
upgrade_probable (array<int> tp_in) {
  array<int> tp= copy (tp_in);
  // FIXME: combinations such as OPEN MIDDLE
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN) {
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
  return tp;
}

static array<int>
confirm_all (array<int> tp_in) {
  array<int> tp= upgrade_probable (tp_in);
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_PROBABLE_OPEN) tp[i]= SYMBOL_OPEN;
    else if (tp[i] == SYMBOL_PROBABLE_MIDDLE) tp[i]= SYMBOL_MIDDLE;
    else if (tp[i] == SYMBOL_PROBABLE_CLOSE) tp[i]= SYMBOL_CLOSE;
  return tp;
}

static bool
admits_brackets (array<int> tp) {
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_OPEN) return true;
  return false;
}

/******************************************************************************
* Heuristic determination of several bracket notations
******************************************************************************/

static array<int>
detect_french_interval (array<tree> a, array<int> tp_in) {
  // FIXME: only allow [ and ]
  array<int> tp= upgrade_probable (tp_in);
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
  return tp;
}

static array<int>
detect_absolute (array<tree> a, array<int> tp_in, bool insist) {
  array<int> tp= upgrade_probable (tp_in);
  int last_open= -1;
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_SEPARATOR) last_open= -1;
    else if (tp[i] >= SYMBOL_OPEN) {
      if (tp[i] == SYMBOL_PROBABLE_OPEN ||
	  (last_open == -1 && tp[i] == SYMBOL_PROBABLE_MIDDLE))
	last_open= i;
      else if (tp[i] == SYMBOL_PROBABLE_CLOSE ||
	       (last_open != -1 && tp[i] == SYMBOL_PROBABLE_MIDDLE))
	{
	  if (a[i] == a[last_open] &&
	      (insist ||
	       (a[last_open] == SYMBOL_PROBABLE_OPEN) ||
	       (a[i] == SYMBOL_PROBABLE_CLOSE)))
	    {
	      tp[last_open]= SYMBOL_OPEN;
	      tp[i]= SYMBOL_CLOSE;
	    }
	  else if (tp[i] == SYMBOL_PROBABLE_MIDDLE) last_open= i;
	  else last_open= -1;
	}
      else last_open= -1;
    }
  return tp;
}

static array<int>
detect_probable (array<tree> a, array<int> tp_in) {
  array<int> tp= upgrade_probable (tp_in);
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
  return tp;
}

/******************************************************************************
* Find matching brackets
******************************************************************************/

static array<tree>
simplify_matching (array<tree> a, array<int> tp_in, int level) {
  array<int> tp= copy (tp_in);
  int last_open= -1;
  for (int i=0; i<N(tp); i++) {
    if (tp[i] == SYMBOL_OPEN) last_open= i;
    else if (tp[i] >= SYMBOL_PROBABLE_OPEN) last_open= -1;
    else if (tp[i] == SYMBOL_CLOSE && last_open != -1) {
      array<tree> b= range (a, last_open+1, i);
      b= upgrade_brackets (b, level+1);
      tree body= concat_recompose (b);
      a[last_open]= tree (AROUND, a[last_open], body, a[i]);
      tp[last_open]= SYMBOL_BASIC;
      for (int j= last_open+1; j<=i; j++) tp[j]= SYMBOL_DELETED;
      last_open= -1;
    }
  }

  array<tree> r;
  for (int i=0; i<N(tp); i++)
    if (tp[i] != SYMBOL_DELETED)
      r << a[i];
  return r;
}

static array<tree>
add_missing_left (array<tree> a, array<int> tp) {
  array<tree> b;
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_CLOSE) {
      tree body= concat_recompose (b);
      b= array<tree> ();
      if (is_atomic (a[i])) b << tree (AROUND, "<lnone>", body, a[i]);
      else b << tree (AROUND, tree (LEFT, "."), body, a[i]);
    }
    else b << a[i];
  return b;
}

static array<tree>
add_missing_right (array<tree> a, array<int> tp) {
  array<tree> b;
  for (int i=N(tp)-1; i>=0; i--)
    if (tp[i] == SYMBOL_OPEN) {
      tree body= concat_recompose (reverse (b));
      b= array<tree> ();
      if (is_atomic (a[i])) b << tree (AROUND, a[i], body, "<rnone>");
      else b << tree (AROUND, a[i], body, tree (RIGHT, "."));
    }
    else b << a[i];
  return reverse (b);
}

static array<tree>
upgrade_brackets (array<tree> a, int level) {
  array<int> tp= bracket_types (a);
  if (admits_brackets (tp)) {
    array<tree> r= simplify_matching (a, downgrade_dubious (tp), level);
    if (r != a) return upgrade_brackets (r, level);
    r= simplify_matching (a, detect_french_interval (a, tp), level);
    if (r != a) return upgrade_brackets (r, level);
    r= simplify_matching (a, detect_absolute (a, tp, false), level);
    if (r != a) return upgrade_brackets (r, level);
    r= simplify_matching (a, detect_absolute (a, tp, true), level);
    if (r != a) return upgrade_brackets (r, level);
    r= simplify_matching (a, detect_probable (a, tp), level);
    if (r != a) return upgrade_brackets (r, level);
    r= simplify_matching (a, confirm_all (tp), level);
    if (r != a) return upgrade_brackets (r, level);
    r= add_missing_left (a, tp);
    if (r != a) return upgrade_brackets (r, level);
    r= add_missing_right (a, tp);
    if (r != a) return upgrade_brackets (r, level);
  }
  return a;
}

/******************************************************************************
* Master routines
******************************************************************************/

static tree
upgrade_brackets (tree t, bool math) {
  if (math && (is_atomic (t) || is_concat (t))) {
    array<tree> a= concat_tokenize (t);
    a= upgrade_brackets (a, 0);
    return concat_recompose (a);
  }
  else return t;
}

tree
upgrade_brackets (drd_info drd, tree t, bool math) {
  if (is_atomic (t))
    return upgrade_brackets (t, math);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      // TODO: mode
      // TODO: check type != ADHOC, RAW.
      // (in particular: do not enter AROUND, LEFT, MIDDLE, ...)
      r[i]= upgrade_brackets (drd, t[i], math);
    }
    return upgrade_brackets (r, math);
  }
}
