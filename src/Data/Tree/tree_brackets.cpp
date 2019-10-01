
/******************************************************************************
* MODULE     : tree_brackets.cpp
* DESCRIPTION: upgrade to tree with matching brackets
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_correct.hpp"
#include "tree_analyze.hpp"
#include "scheme.hpp"

static array<tree> upgrade_brackets (array<tree> a, int level);

/******************************************************************************
* Extra routines for symbol types
******************************************************************************/

static bool
is_dubious_open_middle (array<int> tp, int j) {
  j--;
  while (j >= 0 && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
    j--;
  return
    j < 0 ||
    tp[j] == SYMBOL_PREFIX ||
    tp[j] == SYMBOL_INFIX ||
    tp[j] == SYMBOL_SEPARATOR;
}

static bool
is_dubious_close_middle (array<int> tp, int j) {
  j++;
  while (j < N(tp) && (tp[j] == SYMBOL_SKIP || tp[j] == SYMBOL_SCRIPT))
    j++;
  return
    j >= N(tp) ||
    tp[j] == SYMBOL_POSTFIX ||
    tp[j] == SYMBOL_INFIX ||
    tp[j] == SYMBOL_SEPARATOR;
}

static array<int>
downgrade_dubious (array<int> tp_in) {
  array<int> tp= copy (tp_in);
  // NOTE: we also might forbid combinations such as OPEN MIDDLE
  for (int i=0; i<N(tp); i++)
    if (tp[i] >= SYMBOL_PROBABLE_OPEN && tp[i] <= SYMBOL_PROBABLE_CLOSE) {
      if (is_dubious_open_middle (tp, i)) {
        if (tp[i] == SYMBOL_PROBABLE_MIDDLE) tp[i]= SYMBOL_DUBIOUS_MIDDLE;
        if (tp[i] == SYMBOL_PROBABLE_CLOSE) tp[i]= SYMBOL_DUBIOUS_CLOSE;
      }
      if (is_dubious_close_middle (tp, i)) {
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

static array<tree>
replace_dummies (array<tree> a) {
  array<tree> b (N(a));
  for (int i=0; i<N(a); i++)
    if (a[i] == "<cdot>") b[i]= "<cdummy>";
    else b[i]= a[i];
  return b;
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
        if (last_open != -1 && last_comma != -1 && last_comma != i-1) {
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
  //cout << "    a = " << a << "\n";
  //cout << "    in= " << tp_in << "\n";
  //cout << "    tp= " << tp << "\n";
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
              ((tp[last_open] == SYMBOL_PROBABLE_OPEN) ||
               (tp[i] == SYMBOL_PROBABLE_CLOSE) ||
               //(i == last_open + 2 &&
               //a[i-1] == "<cdot>") ||
               (insist &&
                !is_dubious_open_middle (tp, last_open) &&
                !is_dubious_close_middle (tp, i))))
            {
              tp[last_open]= SYMBOL_OPEN;
              tp[i]= SYMBOL_CLOSE;
              last_open= -1;
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
  (void) a;
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
      if (s == ".") return "<nobracket>";
      if (N(s) <= 1) return s;
      return "<" * s * ">";
    }
  return "<nobracket>";
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
      if (is_atomic (a[i])) b << make_around ("<nobracket>", body, a[i]);
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
      if (is_atomic (a[i])) b << make_around (a[i], body, "<nobracket>");
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
* Master routine for upgrading brackets
******************************************************************************/

static array<tree>
upgrade_brackets (array<tree> a, int level) {
  array<int> tp= symbol_types (a);
  //cout << "Upgrade " << a << ", " << tp << "\n";
  if (admits_brackets (tp)) {
    //cout << "  Downgrade dubious\n";
    array<tree> r= simplify_matching (a, downgrade_dubious (tp), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Detect french\n";
    r= simplify_matching (a, detect_french_interval (a, tp), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Detect absolute 1\n";
    r= simplify_matching (a, detect_absolute (a, tp, false), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Detect absolute 2\n";
    r= simplify_matching (a, detect_absolute (a, tp, true), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Detect probable\n";
    r= simplify_matching (a, detect_probable (a, tp), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Detect dummy substitution\n";
    if (replace_dummies (a) != a) {
      array<tree> a2= replace_dummies (a);
      array<int> tp2= symbol_types (a2);
      r= simplify_matching (a2, detect_probable (a2, tp2), level);
      if (r != a2) return upgrade_brackets (r, level);
    }
    //cout << "  Confirm all\n";
    r= simplify_matching (a, confirm_all (tp), level);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Missing left\n";
    if (get_preference ("automatic brackets") != "off")
      r= add_missing_left (a, tp);
    if (r != a) return upgrade_brackets (r, level);
    //cout << "  Missing right\n";
    if (get_preference ("automatic brackets") != "off")
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
upgrade_brackets_bis (tree t, string mode) {
  //cout << "Upgrade " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      tree tmode= the_drd->get_env_child (t, i, MODE, mode);
      string smode= (is_atomic (tmode)? tmode->label: string ("text"));
      if (is_correctable_child (t, i, true))
        r[i]= upgrade_brackets_bis (t[i], smode);
      else r[i]= t[i];
    }
  }
      
  if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= upgrade_brackets (a, 0);
    tree ret= concat_recompose (a);
    //if (ret != r) cout << "< " << r << LF << "> " << ret << LF;
    return ret;
  }
  else return r;
}

tree
upgrade_brackets (tree t, string mode) {
  //cout << "Upgrade " << t << "\n";
  with_drd drd (get_document_drd (t));
  t= upgrade_above_below (t);
  return upgrade_brackets_bis (t, mode);
}

tree
upgrade_big_bis (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_big_bis (t[i]);
  if (is_concat (r))
    for (int j=0; j<N(r); j++)
      if (is_func (r[j], BIG)) {
        array<tree> a= concat_tokenize (r);
        a= upgrade_brackets (a, 0);
        return concat_recompose (a);
      }
  return r;
}

tree
upgrade_big (tree t) {
  with_drd drd (get_document_drd (t));
  return upgrade_big_bis (t);
}

/******************************************************************************
* Downgrading brackets
******************************************************************************/

static tree
downgrade_bracket (tree t, bool large) {
  if (!is_atomic (t)) {
    if (large && N(t) > 0)
      if (is_func (t, LEFT) || is_func (t, MID) || is_func (t, RIGHT))
        return t[0];
    return t;
  }
  string s= t->label;
  if (large) {
    if (t == "<nobracket>") return tree (".");
    if (starts (s, "<") && ends (s, ">")) return s (1, N(s)-1);
  }
  else if (s == "<nobracket>") return "";
  return t;
}

tree
downgrade_brackets (tree t, bool delete_missing, bool big_dot) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= downgrade_brackets (t[i], delete_missing, big_dot);
  if (is_func (r, AROUND, 3)) {
    if (delete_missing && r[0] == "<nobracket>" && r[2] == "<nobracket>")
      return concat (r[0], r[1], r[2]);
    tree lb= downgrade_bracket (r[0], false);
    tree rb= downgrade_bracket (r[2], false);
    r= concat (lb, r[1], rb);
  }
  if (is_func (r, VAR_AROUND, 3)) {
    tree lb= tree (LEFT, downgrade_bracket (r[0], true));
    tree rb= tree (RIGHT, downgrade_bracket (r[2], true));
    if (delete_missing) {
      if (lb == tree (LEFT, ".") && rb == tree (RIGHT, "."));
      else if (lb == tree (LEFT, ".")) lb= "";
      else if (rb == tree (RIGHT, ".")) rb= "";
    }
    r= concat (lb, r[1], rb);
  }
  if (is_func (r, BIG_AROUND, 2)) {
    tree op= downgrade_bracket (r[0], true);
    if (big_dot) r= concat (tree (BIG, op), r[1], tree (BIG, "."));
    else r= concat (tree (BIG, op), r[1]);
  }
  if (is_concat (r)) r= concat_recompose (concat_decompose (r));
  return r;
}

tree
downgrade_big (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= downgrade_big (t[i]);
  if (is_func (r, BIG_AROUND, 2)) {
    tree op= downgrade_bracket (r[0], true);
    r= concat (tree (BIG, op), r[1]);
  }
  if (is_concat (r)) r= concat_recompose (concat_decompose (r));
  return r;
}

/******************************************************************************
* Moving wrongly brackets across 'math' tag boundary
******************************************************************************/

static bool
is_simple_opening (string s) {
  return s == "(" || s == "[" || s == "{" || s == "|";
}

static bool
is_simple_closing (string s) {
  return s == ")" || s == "]" || s == "}" || s == "|";
}

static bool
is_simple_matching (string l, string r) {
  return
    (l == "(" && r == ")") ||
    (l == "[" && r == "]") ||
    (l == "{" && r == "}") ||
    (l == "|" && r == "|");
}

static tree
move_brackets_sub (tree t, bool in) {
  //cout << t << INDENT << LF;
  if (is_compound (t)) {
    int i, n= N(t);
    tree r= tree (t, n);
    for (i=0; i<n; i++)
      r[i]= move_brackets_sub (t[i], in);
    t= r;
  }

  while (true) {
    tree r= t;
    bool search= true;
    if (is_concat (t))
      for (int i=0; i<N(t) && search; i++)
        if (is_compound (t[i], "math")) {
          array<tree> a= concat_tokenize (t[i][0]);
          for (int j=0; j<N(a) && search; j++)
            if (is_atomic (a[j]) && is_simple_opening (a[j]->label))
              for (int k= i+1; k<N(t) && search; k++)
                if (is_atomic (t[k])) {
                  string s= t[k]->label;
                  for (int l=0; l<N(s) && search; tm_char_forwards (s, l))
                    if (is_simple_matching (a[j]->label, s (l, l+1))) {
                      if (k == i+1 && l == 0 && in) {
                        array<tree> c= concat_decompose (t);
                        a << tree (s (0, 1));
                        c[i]= compound ("math", concat_recompose (a));
                        c[i]= upgrade_brackets (c[i]);
                        c[i+1]= s (1, N(s));
                        r= move_brackets_sub (concat_recompose (c), in);
                        search= false;
                      }
                      else if (j == 0 && !in) {
                        tree x= a[0];
                        array<tree> c= concat_decompose (t);
                        a= range (a, 1, N(a));
                        c[i]= compound ("math", concat_recompose (a));
                        c= append (range (c, 0, i),
                                   append (x, range (c, i, N(c))));
                        r= move_brackets_sub (concat_recompose (c), in);
                        search= false;
                      }
                    }
                }
          for (int j=N(a)-1; j>=0 && search; j--)
            if (is_atomic (a[j]) && is_simple_closing (a[j]->label))
              for (int k= i-1; k>=0 && search; k--)
                if (is_atomic (t[k])) {
                  string s= t[k]->label;
                  for (int l=N(s); l>0 && search; tm_char_backwards (s, l))
                    if (is_simple_matching (s (l-1, l), a[j]->label)) {
                      if (k == i-1 && l == N(s) && in) {
                        array<tree> c= concat_decompose (t);
                        a= append (tree (s (l-1, l)), a);
                        c[i]= compound ("math", concat_recompose (a));
                        c[i]= upgrade_brackets (c[i]);
                        c[i-1]= s (0, l-1);
                        r= move_brackets_sub (concat_recompose (c), in);
                        search= false;
                      }
                      else if (j == N(a)-1 && !in) {
                        tree x= a[j];
                        array<tree> c= concat_decompose (t);
                        a= range (a, 0, j);
                        c[i]= compound ("math", concat_recompose (a));
                        c= append (range (c, 0, i+1),
                                   append (x, range (c, i+1, N(c))));
                        r= move_brackets_sub (concat_recompose (c), in);
                        search= false;
                      }
                    }
                }
        }
    if (search) break;
    else {
      //cout << "< " << t << LF;
      //cout << "> " << r << LF;
      t= r;
    }
  }
  //cout << UNINDENT << "Done" << LF;
  return t;
}

tree
move_brackets (tree t) {
  return move_brackets_sub (move_brackets_sub (t, true), false);
}
