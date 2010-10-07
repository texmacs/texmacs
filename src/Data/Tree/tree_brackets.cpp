
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
#include "analyze.hpp"
#include "vars.hpp"
#include "Scheme/object.hpp"

hashmap<string,tree> get_style_env (tree style);
drd_info get_style_drd (tree style);
static array<tree> upgrade_brackets (array<tree> a, int level);

/******************************************************************************
* Tokenize mathematical concats and recomposition
******************************************************************************/

static array<tree>
concat_tokenize (tree t) {
  static language lan= math_language ("std-math");
  array<tree> r;
  if (is_atomic (t)) {
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
#define SYMBOL_SKIP               5
#define SYMBOL_SCRIPT             6
#define SYMBOL_OPEN_BIG           7
#define SYMBOL_CLOSE_BIG          8
#define SYMBOL_OPEN               9
#define SYMBOL_MIDDLE            10
#define SYMBOL_CLOSE             11
#define SYMBOL_PROBABLE_OPEN     12
#define SYMBOL_PROBABLE_MIDDLE   13
#define SYMBOL_PROBABLE_CLOSE    14
#define SYMBOL_DUBIOUS_OPEN      15
#define SYMBOL_DUBIOUS_MIDDLE    16
#define SYMBOL_DUBIOUS_CLOSE     17

static int
symbol_type (tree t) {
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
  else if (is_func (t, LSUB)) return SYMBOL_SCRIPT;
  else if (is_func (t, LSUP)) return SYMBOL_SCRIPT;
  else if (is_func (t, LPRIME)) return SYMBOL_SCRIPT;
  else if (is_func (t, RSUB)) return SYMBOL_SCRIPT;
  else if (is_func (t, RSUP)) return SYMBOL_SCRIPT;
  else if (is_func (t, RPRIME)) return SYMBOL_SCRIPT;
  else if (is_func (t, SPACE)) return SYMBOL_SKIP;
  else if (is_func (t, HSPACE)) return SYMBOL_SKIP;
  else if (is_func (t, VSPACE)) return SYMBOL_SKIP;
  else if (is_func (t, VAR_VSPACE)) return SYMBOL_SKIP;
  else if (is_func (t, LABEL)) return SYMBOL_SKIP;
  else if (is_compound (t, "text")) return SYMBOL_SKIP;
  else if (is_compound (t, "eq-number")) return SYMBOL_SKIP;
  else if (is_compound (t, "bl")) return SYMBOL_OPEN;
  else if (is_compound (t, "br")) return SYMBOL_CLOSE;
  else return SYMBOL_BASIC;
}

static array<int>
symbol_types (array<tree> a) {
  array<int> tp (N(a));
  for (int i=0; i<N(a); i++)
    tp[i]= symbol_type (a[i]);
  return tp;
}

static array<int>
downgrade_dubious (array<int> tp_in) {
  array<int> tp= copy (tp_in);
  // NOTE: we also might forbid combinations such as OPEN MIDDLE
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN && tp[i] <= SYMBOL_PROBABLE_CLOSE) {
      int j= i-1;
      while (j >= 0 && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
	j--;
      if (j < 0 ||
	  tp[j] == SYMBOL_PREFIX ||
	  tp[j] == SYMBOL_INFIX ||
	  tp[j] == SYMBOL_SEPARATOR)
	{
	  if (tp[i] == SYMBOL_PROBABLE_MIDDLE) tp[i]= SYMBOL_DUBIOUS_MIDDLE;
	  if (tp[i] == SYMBOL_PROBABLE_CLOSE) tp[i]= SYMBOL_DUBIOUS_CLOSE;
	}
      j= i+1;
      while (j < N(tp) && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
	j++;
      if (j >= N(tp) ||
	  tp[j] == SYMBOL_POSTFIX ||
	  tp[j] == SYMBOL_INFIX ||
	  tp[j] == SYMBOL_SEPARATOR)
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
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN) {
      int j= i-1;
      while (j >= 0 && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
	j--;
      if (j < 0 ||
	  tp[j] == SYMBOL_PREFIX ||
	  tp[j] == SYMBOL_INFIX ||
	  tp[j] == SYMBOL_SEPARATOR)
	tp[i]= SYMBOL_PROBABLE_OPEN;
      j= i+1;
      while (j < N(tp) && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
	j++;
      if (j >= N(tp) ||
	  tp[j] == SYMBOL_POSTFIX ||
	  tp[j] == SYMBOL_INFIX ||
	  tp[j] == SYMBOL_SEPARATOR)
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

static bool
admits_bigops (array<int> tp) {
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_OPEN_BIG) return true;
  return false;
}

/******************************************************************************
* Determine symbol priority
******************************************************************************/

#define PRIORITY_SEPARATOR          0
#define PRIORITY_ASSIGN             1
#define PRIORITY_FLUX               2
#define PRIORITY_MODELS             3
#define PRIORITY_IMPLY              4
#define PRIORITY_OR                 5
#define PRIORITY_AND                6
#define PRIORITY_RELATION           7
#define PRIORITY_ARROW              8
#define PRIORITY_UNION              9
#define PRIORITY_INTERSECTION      10
#define PRIORITY_PLUS              11
#define PRIORITY_TIMES             12
#define PRIORITY_POWER             13
#define PRIORITY_RADICAL           14

static int
symbol_priority (tree t) {
  static language lan= math_language ("std-math");
  if (is_atomic (t)) {
    string g= lan->get_group (t->label);
    if (starts (g, "Separator")) return PRIORITY_ASSIGN;
    if (starts (g, "Assign")) return PRIORITY_ASSIGN;
    if (starts (g, "Flux")) return PRIORITY_FLUX;
    if (starts (g, "Models")) return PRIORITY_MODELS;
    if (starts (g, "Imply")) return PRIORITY_IMPLY;
    if (starts (g, "Or")) return PRIORITY_OR;
    if (starts (g, "And")) return PRIORITY_AND;
    if (starts (g, "Relation")) return PRIORITY_RELATION;
    if (starts (g, "Arrow")) return PRIORITY_ARROW;
    if (starts (g, "Union")) return PRIORITY_UNION;
    if (starts (g, "Exclude")) return PRIORITY_UNION;
    if (starts (g, "Intersection")) return PRIORITY_INTERSECTION;
    if (starts (g, "Plus")) return PRIORITY_PLUS;
    if (starts (g, "Minus")) return PRIORITY_PLUS;
    if (starts (g, "Times")) return PRIORITY_TIMES;
    if (starts (g, "Over")) return PRIORITY_TIMES;
    if (starts (g, "Power")) return PRIORITY_POWER;
    return PRIORITY_RADICAL;
  }
  else if (is_func (t, BIG, 1) and is_atomic (t[0])) {
    string s= t[0]->label;
    if (s == "parallel") return PRIORITY_SEPARATOR;
    if (s == "interleave") return PRIORITY_SEPARATOR;
    if (s == "vee") return PRIORITY_OR;
    if (s == "curlyvee") return PRIORITY_OR;
    if (s == "wedge") return PRIORITY_AND;
    if (s == "curlywedge") return PRIORITY_AND;
    if (s == "cup") return PRIORITY_UNION;
    if (s == "sqcup") return PRIORITY_UNION;
    if (s == "amalg") return PRIORITY_UNION;
    if (s == "uplus") return PRIORITY_UNION;
    if (s == "box") return PRIORITY_UNION;
    if (s == "cap") return PRIORITY_INTERSECTION;
    if (s == "sqcap") return PRIORITY_INTERSECTION;
    if (s == "int") return PRIORITY_PLUS;
    if (s == "oint") return PRIORITY_PLUS;
    if (s == "intlim") return PRIORITY_PLUS;
    if (s == "ointlim") return PRIORITY_PLUS;
    if (s == "sum") return PRIORITY_PLUS;
    if (s == "oplus") return PRIORITY_PLUS;
    if (s == "triangledown") return PRIORITY_PLUS;
    if (s == "prod") return PRIORITY_TIMES;
    if (s == "otimes") return PRIORITY_TIMES;
    if (s == "odot") return PRIORITY_TIMES;
    if (s == "triangleup") return PRIORITY_TIMES;
  }
  else return PRIORITY_RADICAL;
}

static array<int>
symbol_priorities (array<tree> a) {
  array<int> tp (N(a));
  for (int i=0; i<N(a); i++)
    tp[i]= symbol_priority (a[i]);
  return tp;
}

/******************************************************************************
* Heuristic determination of several bracket notations
******************************************************************************/

static array<int>
detect_french_interval (array<tree> a, array<int> tp_in) {
  // NOTE: we might only allow [ and ]
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
	  if (last_open != -1 &&
	      a[i] == a[last_open] &&
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
* Process matching brackets
******************************************************************************/

static tree
make_small (tree br) {
  if (is_atomic (br)) return br;
  if (is_func (br, LEFT) ||
      is_func (br, MID) ||
      is_func (br, RIGHT) ||
      is_func (br, BIG))
    if (N(br) > 0 && is_atomic (br[0])) {
      string s= br[0]->label;
      if (s == ".") return "<nomid>";
      if (N(s) <= 1) return s;
      return "<" * s * ">";
    }
  return "<nomid>";
}

static tree
make_around (tree l, tree m, tree r) {
  tree_label kind= VAR_AROUND;
  if (is_atomic (l) && is_atomic (r)) kind= AROUND;
  return tree (kind, make_small (l), m, make_small (r));
}

static tree
make_around (tree l, tree m) {
  return tree (BIG_AROUND, make_small (l), m);
}

static array<tree>
simplify_matching (array<tree> a, array<int> tp_in, int level) {
  //cout << "Simplify matching " << a << ", " << tp_in << "\n";
  array<int> tp= copy (tp_in);
  int last_open= -1;
  for (int i=0; i<N(tp); i++) {
    if (tp[i] == SYMBOL_OPEN) last_open= i;
    else if (tp[i] >= SYMBOL_PROBABLE_OPEN) last_open= -1;
    else if (tp[i] == SYMBOL_CLOSE && last_open != -1) {
      array<tree> b= range (a, last_open+1, i);
      b= upgrade_brackets (b, level+1);
      tree body= concat_recompose (b);
      a[last_open]= make_around (a[last_open], body, a[i]);
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
      if (is_atomic (a[i])) b << make_around ("<nomid>", body, a[i]);
      else b << make_around (tree (LEFT, "."), body, a[i]);
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
      if (is_atomic (a[i])) b << make_around (a[i], body, "<nomid>");
      else b << make_around (a[i], body, tree (RIGHT, "."));
    }
    else b << a[i];
  return reverse (b);
}

/******************************************************************************
* Splitting concats with big operators
******************************************************************************/

static array<tree>
prefix_split (array<tree> a, array<int> tp, int level) {
  for (int i=1; i<N(tp); i++)
    if (tp[i] == SYMBOL_OPEN_BIG) {
      array<tree> r= range (a, 0, i);
      r << upgrade_brackets (range (a, i, N(tp)), level);
      return r;
    }
  return a;
}

static array<tree>
infix_split (array<tree> a, array<int> tp_in, array<int> pri, int level) {
  array<int> tp= upgrade_probable (tp_in);
  int weakest= PRIORITY_RADICAL;
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_OPEN_BIG)
      weakest= min (weakest, pri[i]);
    else if (tp[i] == SYMBOL_INFIX ||
	     tp[i] == SYMBOL_SEPARATOR ||
	     tp[i] == SYMBOL_MIDDLE ||
	     tp[i] == SYMBOL_PROBABLE_MIDDLE)
      if (pri[i] <= weakest) {
	array<tree> r= upgrade_brackets (range (a, 0, i), level);
	r << range (a, i, i+1);
	r << upgrade_brackets (range (a, i+1, N(a)), level);
	return r;
      }
  return a;
}

static array<tree>
postfix_split (array<tree> a, array<int> tp_in, int level) {
  array<int> tp= upgrade_probable (tp_in);
  int i= N(a);
  while (i>0 &&
	 (tp[i-1] == SYMBOL_PREFIX ||
	  tp[i-1] == SYMBOL_INFIX ||
	  tp[i-1] == SYMBOL_SEPARATOR ||
	  tp[i-1] == SYMBOL_OPEN ||
	  tp[i-1] == SYMBOL_MIDDLE ||
	  tp[i-1] == SYMBOL_PROBABLE_OPEN ||
	  tp[i-1] == SYMBOL_PROBABLE_MIDDLE ||
	  tp[i-1] == SYMBOL_SKIP ||
	  a[i-1] == "."))
    i--;
  if (i != N(a)) {
    array<tree> r= upgrade_brackets (range (a, 0, i), level);
    r << range (a, i, N(a));
    return r;
  }
  return a;
}

/******************************************************************************
* Extra subroutines for big operators
******************************************************************************/

static bool
is_concat_big (tree t) {
  if (!is_concat (t) || N(t) == 0 || !is_func (t[0], BIG)) return false;
  for (int i=1; i<N(t); i++)
    if (!is_func (t[i], RSUB, 1) && !is_func (t[i], RSUP, 1))
      return false;
  return true;
}

tree
upgrade_above_below (tree t) {
  if (is_atomic (t)) return t;
  else if (is_concat (t)) {
    tree r (CONCAT);
    for (int i=0; i<N(t); i++) {
      tree x= upgrade_above_below (t[i]);
      if (is_concat (x)) r << A(x);
      else r << x;
    }
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_above_below (t[i]);
    if (is_func (r, ABOVE, 2)) {
      if (is_func (r[0], BIG))
	r= tree (CONCAT, r[0], tree (RSUP, r[1]));
      else if (is_concat_big (r[0]))
	r= tree (r[0] * tree (CONCAT, tree (RSUP, r[1])));
    }
    if (is_func (r, BELOW, 2)) {
      if (is_func (r[0], BIG))
	r= tree (CONCAT, r[0], tree (RSUB, r[1]));
      else if (is_concat_big (r[0]))
	r= tree (r[0] * tree (CONCAT, tree (RSUB, r[1])));
    }
    return r;
  }
}

/******************************************************************************
* Master routines
******************************************************************************/

static array<tree>
upgrade_brackets (array<tree> a, int level) {
  array<int> tp= symbol_types (a);
  //cout << "Upgrade " << a << ", " << tp << "\n";
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
  if (admits_bigops (tp)) {
    array<tree> r= prefix_split (a, tp, level);
    if (r != a) return upgrade_brackets (r, level);
    r= infix_split (a, tp, symbol_priorities (a), level);
    if (r != a) return upgrade_brackets (r, level);
    r= postfix_split (a, tp, level);
    if (r != a) return upgrade_brackets (r, level);
    ASSERT (tp[0] == SYMBOL_OPEN_BIG, "invalid situation");
    r= upgrade_brackets (range (a, 1, N(a)), level + 1);
    tree body= concat_recompose (r);
    r= array<tree> ();
    r << make_around (a[0], body);
    return r;
  }
  return a;
}

static tree
upgrade_brackets (drd_info drd, tree t, string mode) {
  //cout << "Upgrade " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      tree tmode= drd->get_env_child (t, i, MODE, mode);
      string smode= (is_atomic (tmode)? tmode->label: string ("text"));
      int type= drd->get_type_child (t, i);
      if (is_compound (t, "body", 1)) type= TYPE_REGULAR;
      if (is_concat (t)) {
	if (is_atomic (t[i]) ||
	    is_func (t[i], AROUND) ||
	    is_func (t[i], VAR_AROUND) ||
	    is_func (t[i], BIG_AROUND) ||
	    is_func (t[i], LEFT) ||
	    is_func (t[i], MID) ||
	    is_func (t[i], RIGHT) ||
	    is_func (t[i], BIG) ||
	    is_compound (t[i], "bl") ||
	    is_compound (t[i], "br"))
	  r[i]= t[i];
	else
	  r[i]= upgrade_brackets (drd, t[i], smode);
      }
      else
	switch (type) {
	case TYPE_INVALID:
	case TYPE_REGULAR:
	case TYPE_GRAPHICAL:
	case TYPE_ANIMATION:
	case TYPE_UNKNOWN:
	  r[i]= upgrade_brackets (drd, t[i], smode);
	  break;
	default:
	  r[i]= t[i];
	  break;
	}
    }
  }
      
  if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= upgrade_brackets (a, 0);
    return concat_recompose (a);
  }
  else return r;
}

tree
upgrade_brackets (tree t, string mode) {
  if (call ("get-preference", "matching brackets") == object ("on")) {
    //cout << "Upgrade " << t << "\n";
    drd_info drd= get_style_drd (tree (TUPLE, "generic"));
    t= upgrade_above_below (t);
    return upgrade_brackets (drd, t, mode);
  }
  else return t;
}
