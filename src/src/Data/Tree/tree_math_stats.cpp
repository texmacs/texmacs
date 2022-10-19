
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

protected:
  bool is_letter_like (string s);
  void compile (array<tree> a);
  void propose (tree& best, int& nr, tree propose);
  void improve (tree& best, int& nr, string s);

public:
  math_stats (): occurrences (0) {}
  void compile (tree t, string mode);
  void improve (tree& best, int& nr, tree propose);
  tree best_variant (string s);
};

bool
math_stats::is_letter_like (string s) {
  static language lan= math_language ("std-math");
  if (s != "" && is_iso_alpha (s)) return true;
  return lan->get_group (s) == "Letter-symbol";
}

void
math_stats::compile (array<tree> a) {
  array<int> tp= symbol_types (a);
  for (int i=0; i<N(a); i++) {
    if (is_atomic (a[i]))
      occurrences (a[i])= occurrences [a[i]] + 1;
    else if ((is_compound (a[i], "math-ss", 1) ||
              is_compound (a[i], "math-tt", 1)) &&
             is_atomic (a[i][0]) &&
             is_letter_like (a[i][0]->label))
      occurrences (a[i])= occurrences [a[i]] + 1;
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
math_stats::compile (tree t, string mode) {
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      if (is_func (t, WITH) && i != N(t)-1);
      else if (is_correctable_child (t, i))
        compile (t[i], smode);
    }
  }
  if (mode == "math")
    compile (concat_tokenize (t));
}

/******************************************************************************
* Best variants of letters
******************************************************************************/

void
math_stats::propose (tree& best, int& nr, tree propose) {
  if (occurrences [propose] > nr) {
    best= propose;
    nr= occurrences [propose];
  }
}

void
math_stats::improve (tree& best, int& nr, string s) {
  string rad= s;
  if (N(rad) >= 2) rad= rad (1, N(rad)-1);
  propose (best, nr, s);
  propose (best, nr, tree ("<up-" * rad * ">"));
  propose (best, nr, tree ("<cal-" * rad * ">"));
  propose (best, nr, tree ("<frak-" * rad * ">"));
  propose (best, nr, tree ("<bbb-" * rad * ">"));
  propose (best, nr, tree ("<b-" * rad * ">"));
  propose (best, nr, tree ("<b-up-" * rad * ">"));
  propose (best, nr, tree ("<b-cal-" * rad * ">"));
  propose (best, nr, tree ("<b-frak-" * rad * ">"));
  propose (best, nr, compound ("math-ss", s));
  propose (best, nr, compound ("math-tt", s));
}

tree
math_stats::best_variant (string s) {
  tree best= "";
  int nr= 0;
  improve (best, nr, s);
  if (N(s) == 1) s= upcase_first (s);
  else s= "<" * upcase_first (s (1, N(s)-1)) * ">";
  improve (best, nr, s);
  return best;
}

/******************************************************************************
* User interface
******************************************************************************/

static hashmap<string,math_stats> stats_table;

void
compile_stats (string id, tree t, string mode) {
  math_stats stats;
  stats.compile (t, mode);
  stats_table (id)= stats;
}

int
number_occurrences (string id, tree t) {
  if (!stats_table->contains (id)) return 0;
  return stats_table[id].occurrences[t];
}

tree
best_variant (string id, string s) {
  if (!stats_table->contains (id)) return "";
  return stats_table[id].best_variant (s);
}
