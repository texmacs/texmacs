
/******************************************************************************
* MODULE     : tree_math_stats.cpp
* DESCRIPTION: compile statistics for math formulas in documents
* COPYRIGHT  : (C) 2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_math_stats.hpp"
#include "tree_analyze.hpp"
#include "scheme.hpp"
#include "packrat.hpp"
#include "analyze.hpp"

/******************************************************************************
* Compile statistics
******************************************************************************/

struct math_stats {
  hashmap<tree,int> occurrences;
  hashmap<tree,int> roles;

protected:
  void compile (array<tree> a, tree parent);

public:
  math_stats (): occurrences (0), roles (0) {}
  void compile (tree t, tree parent, string mode);
};

static tree
strip_decorations (tree t) {
  if (is_func (t, WIDE) || is_func (t, VAR_WIDE))
    return strip_decorations (t[0]);
  else if (is_func (t, NEG))
    return strip_decorations (t[0]);
  else if (is_atomic (t) && is_numeric (t->label))
    return "1";
  else if (is_func (t, SQRT))
    return tree (SQRT, "1");
  else if (is_func (t, FRAC))
    return tree (FRAC, "1", "1");
  else return t;
}

static tree
tmconcat (tree a1, tree a2) {
  tree c= tree (CONCAT, a1, a2);
  if (is_atomic (a1) && is_atomic (a2)) c= a1->label * a2->label;
  return c;
}

static tree
tmconcat (tree a1, tree a2, tree a3) {
  tree c= tree (CONCAT, a1, a2, a3);
  if (is_atomic (a1) && is_atomic (a2) && is_atomic (a3))
    c= a1->label * a2->label * a3->label;
  else if (is_atomic (a1) && is_atomic (a2))
    c= tree (CONCAT, a1->label * a2->label, a3);
  else if (is_atomic (a2) && is_atomic (a3))
    c= tree (CONCAT, a1, a2->label * a3->label);
  return c;
}

void
math_stats::compile (array<tree> a, tree parent) {
  //array<int> tp= symbol_types (a);
  for (int i=0; i<N(a); i++) {
    occurrences (a[i])= occurrences [a[i]] + 1;
    if (i == 0 && is_atomic (a[i]) &&
        (is_func (parent, RSUB) || is_func (parent, RSUP))) {
      tree u (L(parent), a[i]);
      roles (u)= roles[u] + 1;
    }
    if (i == 0 && is_atomic (a[i]) &&
        (is_func (parent, AROUND) || is_func (parent, VAR_AROUND))) {
      tree u (L(parent), copy (parent[0]), a[i], copy (parent[2]));
      roles (u)= roles[u] + 1;
    }
    if ((i+2) < N(a) &&
        //is_atomic (a[i]) && tp[i] == SYMBOL_BASIC &&
        //is_atomic (a[i+2]) && tp[i+2] == SYMBOL_BASIC &&
        is_atomic (a[i+1])) {
      int j=i;
      while (j>0 && (is_func (a[j], RSUB) ||
                     is_func (a[j], RSUP) ||
                     is_func (a[j], RPRIME))) j--;
      tree l= strip_decorations (a[j]);
      tree op= a[i+1];
      tree r= strip_decorations (a[i+2]);
      if (op == "-") op= "+";
      tree C= tmconcat (a[j], op, a[i+2]);
      roles (C)= roles[C] + 1;
      tree c= tmconcat (l, op, r);
      if (c != C) roles (c)= roles[c] + 1;
      c= tmconcat (l, op);
      roles (c)= roles[c] + 1;
    }
    if ((i+1) < N(a) &&
        //is_atomic (a[i]) && tp[i] == SYMBOL_BASIC &&
        (is_func (a[i+1], AROUND, 3) || is_func (a[i+1], VAR_AROUND, 3))) {
      tree l= strip_decorations (a[i]);
      tree b= a[i+1];
      tree C (CONCAT, a[i], tree (AROUND, b[0], b[1], b[2]));
      roles (C)= roles[C] + 1;
      tree c (CONCAT, l, tree (AROUND, b[0], strip_decorations (b[1]), b[2]));
      if (c != C) roles (c)= roles[c] + 1;
      c= tree (CONCAT, l, tree (AROUND, b[0], "", b[2]));
      roles (c)= roles[c] + 1;
    }
    if ((i+1) < N(a) &&
        //is_atomic (a[i]) && tp[i] == SYMBOL_BASIC &&
        (is_func (a[i+1], RSUB, 1) || is_func (a[i+1], RSUP, 1))) {
      tree l= strip_decorations (a[i]);
      tree s= a[i+1];
      tree C (CONCAT, a[i], s);
      roles (C)= roles[C] + 1;
      tree c (CONCAT, l, tree (L(s), strip_decorations (s[0])));
      if (c != C) roles (c)= roles[c] + 1;
      c= tree (CONCAT, l, tree (L(s), ""));
      roles (c)= roles[c] + 1;
    }
  }
}

static string
get_submode (tree t, int i, string mode) {
  if (is_func (t, WITH) && i == N(t)-1)
    for (int j=0; j<N(t)-1; j+=2)
      if (t[j] == MATH_FONT_FAMILY) return "text";
  tree tmode= the_drd->get_env_child (t, i, MODE, mode);
  return (is_atomic (tmode)? tmode->label: string ("text"));
}

void
math_stats::compile (tree t, tree parent, string mode) {
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      if (is_func (t, WITH) && i != N(t)-1);
      else if (is_correctable_child (t, i))
        compile (t[i], t, smode);
    }
  }
  if (mode == "math")
    compile (concat_tokenize (t), parent);
}

/******************************************************************************
* User interface
******************************************************************************/

static hashmap<string,math_stats> stats_table;

void
compile_stats (string id, tree t, string mode) {
  math_stats stats;
  stats.compile (t, tree (DOCUMENT, t), mode);
  stats_table (id)= stats;
}

int
number_occurrences (string id, tree t) {
  if (!stats_table->contains (id)) return 0;
  return stats_table[id].occurrences[t];
}

int
number_in_role (string id, tree t) {
  if (!stats_table->contains (id)) return 0;
  return stats_table[id].roles[t];
}
