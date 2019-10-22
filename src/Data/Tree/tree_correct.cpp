
/******************************************************************************
* MODULE     : tree_correct.cpp
* DESCRIPTION: make a tree syntactically match a drd
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_correct.hpp"
#include "tree_analyze.hpp"
#include "scheme.hpp"
#include "packrat.hpp"

/******************************************************************************
* DRD based correction
******************************************************************************/

tree
drd_correct (drd_info drd, tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    if (drd->contains (as_string (L(t))) &&
        !drd->correct_arity (L(t), n))
      return "";
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= drd_correct (drd, t[i]);
    return r;
  }
}

/******************************************************************************
* Add missing explicit document tags when necessary
******************************************************************************/

static bool
is_block_pseudo_code (tree t) {
  if (!is_compound (t) || N(t) == 0) return false;
  string s= as_string (L(t));
  if (!starts (s, "algo-")) return false;
  return
    s == "algo-procedure" ||
    s == "algo-function" ||
    s == "algo-for" ||
    s == "algo-forall" ||
    s == "algo-foreach" ||
    s == "algo-while" ||
    s == "algo-repeat" ||
    s == "algo-loop" ||
    s == "algo-body" ||
    s == "algo-begin" ||
    s == "algo-inputs" ||
    s == "algo-outputs" ||
    s == "algo-if" ||
    s == "algo-else-if" ||
    s == "algo-else";
}

tree
correct_missing_block (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= correct_missing_block (t[i]);
  t= r;
  if (is_block_pseudo_code (t))
    if (!is_document (t[N(t)-1]))
      t[N(t)-1]= tree (DOCUMENT, t[N(t)-1]);
  return t;
}

/******************************************************************************
* Correct incorrectly concatenated block content
******************************************************************************/

static tree
to_concat (tree t) {
  if (t == "") return tree (CONCAT);
  else if (is_concat (t)) return t;
  else return tree (CONCAT, t);
}

static tree
from_concat (tree t) {
  if (N(t) == 0) return "";
  else if (N(t) == 1) return t[0];
  else return t;
}

static bool
is_migratable (tree t) {
  return
    t == " " || is_compound (t, "mlx", 2) ||
    is_func (t, VAR_VSPACE) || is_func (t, VSPACE) ||
    (is_compound (t) && L(t) >= VAR_PAGE_BREAK && L(t) <= NEW_DPAGE);
}

bool expand_needs_surrounding (string s); // defined in upgrade.cpp

static bool
is_basic_environment (tree t) {
  return // TODO: to be completed using DRD properties
    (is_compound (t) && N(t) == 1 &&
     expand_needs_surrounding (as_string (L(t)))) ||
    is_compound (t, "equation", 1) || is_compound (t, "equation*", 1);
}

static tree
migrate_surround (tree bef, tree aft, tree body) {
  if (is_document (body) && N(body) > 0) {
    if (N(body) == 1)
      return tree (DOCUMENT, migrate_surround (bef, aft, body[0]));
    tree first (DOCUMENT, migrate_surround (bef, "", body[0]));
    tree last  (DOCUMENT, migrate_surround ("", aft, body[N(body)-1]));
    return first * body (1, N(body)-1) * last;
  }
  else
    return from_concat (to_concat (bef) * to_concat (body) * to_concat (aft));
}

static tree
make_surround (tree bef, tree aft, tree t) {
  if (bef == "" && aft == "") return t;
  else if (N(t) == 1 && is_basic_environment (t))
    return tree (L(t), migrate_surround (bef, aft, t[0]));
  return tree (SURROUND, bef, aft, t);
}

tree
correct_concat_block (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= correct_concat_block (t[i]);
  t= r;
  if (is_concat (t)) {
    tree doc (DOCUMENT);
    int start= 0;
    for (i=0; i<n; )
      if (is_multi_paragraph (t[i])) {
        int bef_i= i;
        while (bef_i > start && is_migratable (t[bef_i-1])) bef_i--;
        int aft_i= i+1;
        while (aft_i < n && is_migratable (t[aft_i])) aft_i++;
        if (start < bef_i) doc << from_concat (t (start, bef_i));
        tree bef= from_concat (t (bef_i, i));
        tree aft= from_concat (t (i+1, aft_i));
        doc << make_surround (bef, aft, t[i]);
        i= aft_i;
        start= i;
      }
      else i++;
    if (start < n) doc << from_concat (t (start, n));
    if (N(doc) == 1) return doc[0];
    else return doc;
  }
  else if (is_document (t)) {
    tree doc (DOCUMENT);
    for (i=0; i<n; i++)
      if (is_document (t[i])) doc << A(t[i]);
      else doc << t[i];
    return doc;
  }
  else if (N(t) == 1 && is_basic_environment (t) && !is_document (t[0]))
    return tree (L(t), tree (DOCUMENT, t[0]));
  else return t;
}

/******************************************************************************
* Correct WITHs or WITH-like macros
******************************************************************************/

tree
with_correct (tree t) {
  if (is_atomic (t)) return t;
  else {
    //cout << "Correcting " << t << LF << INDENT;
    tree u (t, N(t));
    for (int k=0; k<N(t); k++)
      u[k]= with_correct (t[k]);
    array<tree> a= concat_decompose (u);
    int i, n= N(a);
    array<tree> r;
    for (i=0; i<n; i++) {
      if (is_with_like (a[i])) {
        array<tree> b= with_decompose (a[i], with_body (a[i]));
        int p= N(b), k1, k2;
        for (k1=0; k1<p ; k1++)
          if (is_with_like (b[k1]) && with_similar_type (a[i], b[k1]));
          else break;
        for (k2=p; k2>k1; k2--)
          if (is_with_like (b[k2-1]) && with_similar_type (a[i], b[k2-1]));
          else break;
        array<tree> x;
        if (0  < k1) x << range (b, 0, k1);
        if (k1 < k2) x << with_recompose (a[i], range (b, k1, k2));
        if (k2 < p ) x << range (b, k2, p);
        if (N(x) == 0) continue;
        if (N(r) != 0 &&
            is_with_like (r[N(r)-1]) &&
            with_same_type (r[N(r)-1], x[0]))
          {
            array<tree> c= concat_decompose (with_body (r[N(r)-1]));
            c << concat_decompose (with_body (x[0]));
            r[N(r)-1]= with_recompose (x[0], c);
            r << range (x, 1, N(x));
          }
        else r << x;
      }
      else r << a[i];
    }
    //cout << UNINDENT << "Corrected " << t << " -> "
    //<< concat_recompose (r) << LF;
    return concat_recompose (r);
  }
}

static tree
superfluous_with_correct (tree t, tree env) {
  if (is_atomic (t)) return t;
  else {
    //cout << "Superfluous correcting " << t << ", " << env << LF;
    if (is_compound (t, "body", 1))
      return compound ("body", superfluous_with_correct (t[0], env));
    if (is_func (t, WITH) && ((N(t) & 1) == 0))
      t= t * tree (WITH, "");
    tree r (t, N(t));
    for (int i=0; i<N(t); i++)
      r[i]= superfluous_with_correct
              (t[i], the_drd->get_env_child (t, i, env));
    if (is_compound (r, "math", 1) && r[0] == "") return "";
    else if (is_compound (r, "text", 1) && r[0] == "") return "";
    else if (is_compound (r, "math", 1) && drd_env_read (env, MODE) == "math")
      return r[0];
    else if (is_compound (r, "text", 1) && drd_env_read (env, MODE) == "text")
      return r[0];
    else if (is_func (r, WITH)) {
      for (int i=0; i+1<N(r); i+=2)
        if (!is_atomic (r[i])) return r;
        else if (drd_env_read (env, r[i]->label) != r[i+1]) return r;
      return r[N(r)-1];
    }
    else if (is_func (r, CONCAT)) {
      array<tree> a= concat_decompose (r);
      return concat_recompose (a);
    }
    return r;
  }
}

tree
superfluous_with_correct (tree t) {
  with_drd drd (get_document_drd (t));
  return superfluous_with_correct (t, tree (WITH, MODE, "text"));
}

/******************************************************************************
* Replace symbols by appropriate homoglyphs
******************************************************************************/

static array<tree>
homoglyph_correct (array<tree> a) {
  array<int>  tp= symbol_types (a);
  array<tree> r;
  //cout << a << ", " << tp << "\n";
  for (int i=0; i<N(a); i++)
    if (a[i] == "<minus>") r << tree ("-");
    else if (a[i] == "\\" || a[i] == "<backslash>") {
      int j1, j2;
      for (j1= i-1; j1>=0; j1--)
        if (tp[j1] != SYMBOL_SKIP && tp[j1] != SYMBOL_SCRIPT) break;
      for (j2= i+1; j2<N(a); j2++)
        if (tp[j2] != SYMBOL_SKIP && tp[j2] != SYMBOL_SCRIPT) break;
      if (j1 < 0 || j2 >= N(a));
      else if ((a[i] == "\\" ||
                a[i] == "<backslash>") &&
               ((tp[j1] == SYMBOL_BASIC) ||
                (tp[j1] == SYMBOL_POSTFIX)) &&
               ((tp[j2] == SYMBOL_BASIC) ||
                (tp[j2] == SYMBOL_PREFIX)))
        r << tree ("<setminus>");
      else r << a[i];
    }
    else if (is_func (a[i], NEG, 1) && is_atomic (a[i][0])) {
      string s= a[i][0]->label;
      if (s == "=") r << tree ("<neq>");
      else if (s == "<less>") r << tree ("<nless>");
      else if (s == "<gtr>") r << tree ("<ngtr>");
      else if (s == "<leq>") r << tree ("<nleq>");
      else if (s == "<geq>") r << tree ("<ngeq>");
      else if (s == "<leqslant>") r << tree ("<nleqslant>");
      else if (s == "<geqslant>") r << tree ("<ngeqslant>");
      else if (s == "<prec>") r << tree ("<nprec>");
      else if (s == "<succ>") r << tree ("<nsucc>");
      else if (s == "<preceq>") r << tree ("<npreceq>");
      else if (s == "<succeq>") r << tree ("<nsucceq>");
      else if (s == "<preccurlyeq>") r << tree ("<npreccurlyeq>");
      else if (s == "<succcurlyeq>") r << tree ("<nsucccurlyeq>");
      else if (s == "<rightarrow>") r << tree ("<nrightarrow>");
      else if (s == "<Rightarrow>") r << tree ("<nRightarrow>");
      else if (s == "<leftarrow>") r << tree ("<nleftarrow>");
      else if (s == "<Leftarrow>") r << tree ("<nLeftarrow>");
      else if (s == "<leftrightarrow>") r << tree ("<nleftrightarrow>");
      else if (s == "<Leftrightarrow>") r << tree ("<nLeftrightarrow>");
      else if (s == "<equiv>") r << tree ("<nequiv>");
      else if (s == "<sim>") r << tree ("<nsim>");
      else if (s == "<simeq>") r << tree ("<nsimeq>");
      else if (s == "<approx>") r << tree ("<napprox>");
      else if (s == "<cong>") r << tree ("<ncong>");
      else if (s == "<asymp>") r << tree ("<nasymp>");
      else if (s == "<in>") r << tree ("<nin>");
      else if (s == "<ni>") r << tree ("<nni>");
      else if (s == "<subset>") r << tree ("<nsubset>");
      else if (s == "<supset>") r << tree ("<nsupset>");
      else if (s == "<subseteq>") r << tree ("<nsubseteq>");
      else if (s == "<supseteq>") r << tree ("<nsupseteq>");
      else if (s == "<sqsubset>") r << tree ("<nsqsubset>");
      else if (s == "<sqsupset>") r << tree ("<nsqsupset>");
      else if (s == "<sqsubseteq>") r << tree ("<nsqsubseteq>");
      else if (s == "<sqsupseteq>") r << tree ("<nsqsupseteq>");
      else if (s == "<leadsto>") r << tree ("<nleadsto>");
      else r << a[i];
    }
    else if (a[i] == ":" && i+1 < N(a) && a[i+1] == "=") {
      r << tree ("<assign>");
      i++;
    }
    else r << a[i];
  return r;
}

static string
get_submode (tree t, int i, string mode) {
  if (is_func (t, WITH) && i == N(t)-1)
    for (int j=0; j<N(t)-1; j+=2)
      if (t[j] == MATH_FONT_FAMILY) return "text";
  tree tmode= the_drd->get_env_child (t, i, MODE, mode);
  return (is_atomic (tmode)? tmode->label: string ("text"));
}

static tree
homoglyph_correct (tree t, string mode) {
  //cout << "Correct " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      if (is_correctable_child (t, i))
        r[i]= homoglyph_correct (t[i], smode);
      else r[i]= t[i];
    }
  }

  if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= homoglyph_correct (a);
    tree ret= concat_recompose (a);
    //if (ret != r) cout << "< " << r << " >" << LF
    //<< "> " << ret << " <" << LF;
    return ret;
  }
  else return r;
}

tree
homoglyph_correct (tree t) {
  with_drd drd (get_document_drd (t));
  return homoglyph_correct (t, "text");
}

/******************************************************************************
* Remove incorrect spaces and multiplications
******************************************************************************/

static array<tree>
superfluous_invisible_correct (array<tree> a) {
  array<int>  tp= symbol_types (a);
  array<tree> r;
  //cout << a << ", " << tp << "\n";
  for (int i=0; i<N(a); i++)
    if (a[i] == " " || a[i] == "*") {
      int j1, j2;
      for (j1= i-1; j1>=0; j1--)
        if (tp[j1] != SYMBOL_SKIP && tp[j1] != SYMBOL_SCRIPT) break;
        else if (a[j1] == " ") break;
      for (j2= i+1; j2<N(a); j2++)
        if (tp[j2] != SYMBOL_SKIP && tp[j2] != SYMBOL_SCRIPT)
          if (a[j2] != " " && a[j2] != "*") break;
      //cout << "  " << i << ": " << j1 << ", " << j2
      //<< "; " << tp[j1] << ", " << tp[j2] << "\n";
      if (j1 < 0 || j2 >= N(a));
      else if (a[j1] == " " || a[j1] == "*");
      else if (tp[j1] == SYMBOL_PREFIX ||
               tp[j1] == SYMBOL_INFIX ||
               tp[j1] == SYMBOL_PREFIX_INFIX ||
               tp[j1] == SYMBOL_SEPARATOR ||
               tp[j1] == SYMBOL_PROBABLE_MIDDLE);
      else if (tp[j2] == SYMBOL_POSTFIX ||
               tp[j2] == SYMBOL_INFIX ||
               tp[j2] == SYMBOL_PREFIX_INFIX ||
               tp[j2] == SYMBOL_SEPARATOR ||
               tp[j2] == SYMBOL_PROBABLE_MIDDLE);
      else r << a[i];
    }
    else if (is_func (a[i], SQRT, 2) && a[i][1] == "")
      r << tree (SQRT, a[i][0]);
    else if (is_script (a[i]) && a[i][0] == "")
      r << tree (L(a[i]), "<nosymbol>");
    else r << a[i];
  return r;
}

static tree
superfluous_invisible_correct (tree t, string mode) {
  //cout << "Correct " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      //cout << "  " << i << ": " << is_correctable_child (t, i)
      //<< ", " << smode << "\n";
      if (is_func (t, WITH) && i != N(t)-1)
        r[i]= t[i];
      else if (is_correctable_child (t, i))
        r[i]= superfluous_invisible_correct (t[i], smode);
      else r[i]= t[i];
    }
  }
  
  if (is_func (r, CONCAT)) {
    bool ok= true;
    int i, found= -1;
    for (i=0; i<N(r); i++)
      if (is_compound (r[i], "hide-preamble") ||
          is_compound (r[i], "show-preamble"))
        {
          ok= (found == -1);
          found= i;
        }
      else if (!is_atomic (r[i])) ok= false;
      else {
        string s= r[i]->label;
        for (int j=0; j<N(s); j++)
          if (s[j] != ' ') ok= false;
      }
    if (ok) r= r[found];
  }

  if (is_func (r, INACTIVE, 1) && is_func (r[0], RIGID))
    return r[0];
  else if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= superfluous_invisible_correct (a);
    tree ret= concat_recompose (a);
    //if (ret != r) cout << "< " << r << " >" << LF
    //<< "> " << ret << " <" << LF;
    return ret;
  }
  else return r;
}

tree
superfluous_invisible_correct (tree t) {
  with_drd drd (get_document_drd (t));
  return superfluous_invisible_correct (t, "text");
}

/******************************************************************************
* Insert missing multiplications or function applications
******************************************************************************/

#define SURE_NOTHING     0
#define SURE_TIMES       1
#define SURE_SPACE       2
#define PROBABLE_TIMES   3
#define PROBABLE_SPACE   4
#define BOTH_WAYS        5

struct invisible_corrector {
  int force;
  hashmap<string,int> times_before;
  hashmap<string,int> times_after;
  hashmap<string,int> space_before;
  hashmap<string,int> space_after;

protected:
  bool is_letter_like (string s);
  bool contains_infix (tree t);
  bool contains_plus_like (tree t);
  void count_invisible (array<tree> a);
  void count_invisible (tree t, string mode);
  int  get_status (tree t, bool left, bool script_flag);
  array<tree> correct (array<tree> a);

public:
  inline invisible_corrector (tree t, int force2):
    force (force2), times_before (0), times_after (0), space_after (0) {
      space_after ("o")= 1000000;
      space_after ("O")= 1000000;
      count_invisible (t, "text"); }
  tree correct (tree t, string mode);
};

bool
invisible_corrector::is_letter_like (string s) {
  static language lan= math_language ("std-math");
  if (s != "" && is_iso_alpha (s)) return true;
  return lan->get_group (s) == "Letter-symbol";
}

bool
invisible_corrector::contains_infix (tree t) {
  array<int> tp= symbol_types (concat_tokenize (t));
  for (int i=0; i<N(tp); i++)
    if (tp[i] == SYMBOL_INFIX || tp[i] == SYMBOL_PREFIX_INFIX)
      return true;
  return false;
}

bool
invisible_corrector::contains_plus_like (tree t) {
  array<tree> a= concat_tokenize (t);
  for (int i=1; i<N(a)-1; i++)
    if (a[i] == "+" || a[i] == "-")
      return true;
  return false;
}

void
invisible_corrector::count_invisible (array<tree> a) {
  array<int>  tp= symbol_types (a);
  for (int i=0; i<N(a); i++)
    if (is_atomic (a[i]) && is_letter_like (a[i]->label)) {
      int j1, j2;
      for (j1= i-1; j1>=0; j1--)
        if (tp[j1] != SYMBOL_SKIP && tp[j1] != SYMBOL_SCRIPT) break;
        else if (a[j1] == " ") break;
      for (j2= i+1; j2<N(a); j2++)
        if (tp[j2] != SYMBOL_SKIP && tp[j2] != SYMBOL_SCRIPT) break;
        else if (a[j2] == " ") break;
      string s= a[i]->label;
      if (j1 >= 0) {
        if (a[j1] == "*")
          times_before (s)= times_before[s] + 1;
        if (a[j1] == " ")
          space_before (s)= space_before[s] + 1;
      }
      if (j2 < N(a)) {
        if (a[j2] == "*")
          times_after (s)= times_after[s] + 1;
        if (a[j2] == " ")
          space_after (s)= space_after[s] + 1;
        // NOTE: this heuristic might not be a good idea,
        // because it inhibits the correction of QR -> Q*R,
        // if Q is a polynomial which is applied somewhere Q(1).
        // We might introduce a table 'apply_after'.
        //if (is_around (a[j2]) && a[j2][0] == "(" &&
        //!contains_infix (a[j2][1]))
        //space_after (s)= space_after[s] + 1;
      }
    }
}

void
invisible_corrector::count_invisible (tree t, string mode) {
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      if (is_func (t, WITH) && i != N(t)-1);
      else if (is_correctable_child (t, i))
        count_invisible (t[i], smode);
    }
  }
  if (mode == "math")
    count_invisible (concat_tokenize (t));
}

int
invisible_corrector::get_status (tree t, bool left, bool script_flag) {
  if (is_atomic (t)) {
    static language lan= math_language ("std-math");
    string s= t->label;
    string g= lan->get_group (t->label);
    if (is_numeric (s))
      return (left? SURE_TIMES: PROBABLE_TIMES);
    else if (starts (g, "Unary-operator-textual"))
      return (left? SURE_SPACE: BOTH_WAYS);
    else if (starts (g, "Binary-operator"))
      return SURE_SPACE;
    else if (starts (g, "N-ary-operator"))
      return (left? SURE_SPACE: BOTH_WAYS);
    else if (is_letter_like (s)) {
      if (left) {
        if (times_after[s] > 0 && space_after[s] == 0)
          return SURE_TIMES;
        else if (space_after[s] > 0 && times_after[s] == 0)
          return SURE_SPACE;
        else if (times_after[s] > space_after[s])
          return PROBABLE_TIMES;
        else if (space_after[s] > times_after[s])
          return PROBABLE_SPACE;
        else if (N(s)>1 && is_iso_alpha (s))
          return PROBABLE_SPACE;
        else if (script_flag)
          return PROBABLE_TIMES;
        else return BOTH_WAYS;
      }
      else {
        if (times_before[s] > space_before[s])
          return PROBABLE_TIMES;
        else if (times_after[s] > 0 && space_after[s] == 0)
          return PROBABLE_TIMES;
        else if (script_flag && (N(s) == 1 || !is_iso_alpha (s)))
          return PROBABLE_TIMES;
        else return BOTH_WAYS;
      }
    }
    else if (s == "<cdots>" || s == "<ldots>")
      return PROBABLE_TIMES;
    else return ((force > 0)? BOTH_WAYS: SURE_NOTHING);
  }
  else {
    if (is_around (t)) {
      if (left && contains_plus_like (t[1]))
        return ((force > 0)? SURE_TIMES: PROBABLE_TIMES);
      else if (contains_plus_like (t[1]))
        return ((force > 0)? PROBABLE_TIMES: BOTH_WAYS);
      else if (!contains_infix (t[1]))
        return (left? BOTH_WAYS: SURE_SPACE);
      else return BOTH_WAYS;
    }
    else if (is_func (t, FRAC) ||
             is_func (t, SQRT))
      return (left? SURE_TIMES: BOTH_WAYS);
    else if (!left && is_func (t, BIG_AROUND, 2) &&
             (t[0] == "<sum>" || t[0] == "<amalg>" ||
              t[0] == "<oplus>" || t[0] == "<uplus>" ||
              t[0] == "<int>" || t[0] == "<oint>" ||
              t[0] == "<intlim>" || t[0] == "<ointlim>" ||
              t[0] == "<prod>" || t[0] == "<odot>" || t[0] == "<otimes>"))
      return PROBABLE_TIMES;
    else if (is_func (t, WIDE, 2))
      return get_status (t[0], left, script_flag);
    else if (is_func (t, WITH))
      return get_status (t[N(t)-1], left, script_flag);
    else if (N(t) == 0 && L(t) >= START_EXTENSIONS) {
      tree def= the_drd->get_syntax (L(t));
      if (is_func (def, MACRO, 1))
        return get_status (def[0], left, script_flag);
      else return SURE_NOTHING;
    }
    else return SURE_NOTHING;
  }
}

static bool
admits_script (array<int> tp, int i) {
  i++;
  while (i<N(tp))
    if (tp[i] == SYMBOL_SCRIPT) return true;
    else if (tp[i] == SYMBOL_SKIP) i++;
    else return false;
  return false;
}

array<tree>
invisible_corrector::correct (array<tree> a) {
  //cout << "Correct " << a << "\n";
  array<tree> r;
  array<int> tp= symbol_types (a);
  for (int i=0; i<N(a); i++) {
    r << a[i];
    if (a[i] != " " && tp[i] == SYMBOL_BASIC) {
      int j;
      for (j= i+1; j<N(a); j++)
        if (tp[j] != SYMBOL_SKIP && tp[j] != SYMBOL_SCRIPT) break;
        else if (a[j] == " ") break;
      if (j >= N(a) || a[j] == " " || tp[j] != SYMBOL_BASIC)
        continue;
      
      string ins= "";
      int sti= get_status (a[i], true, admits_script (tp, i));
      int stj= get_status (a[j], false, admits_script (tp, j));
      //cout << "Pair (" << a[i] << ", " << a[j] << ")"
      //<< " -> (" << sti << ", " << stj << ")" << LF;
      if (sti == SURE_NOTHING || stj == SURE_NOTHING)
        ins= "";
      else if (sti == SURE_TIMES && stj != SURE_SPACE)
        ins= "*";
      else if (sti == SURE_SPACE && stj != SURE_TIMES)
        ins= " ";
      else if (sti == PROBABLE_TIMES && stj == PROBABLE_TIMES)
        ins= "*";
      else if (sti == PROBABLE_SPACE && stj == PROBABLE_SPACE)
        ins= " ";
      else if (sti == PROBABLE_TIMES && stj == BOTH_WAYS)
        ins= "*";
      else if (sti == PROBABLE_SPACE && stj == BOTH_WAYS)
        ins= " ";
      else if (sti == BOTH_WAYS && stj == PROBABLE_TIMES)
        ins= "*";
      else if (sti == BOTH_WAYS && stj == PROBABLE_SPACE)
        ins= " ";
      else if (sti == BOTH_WAYS && stj == BOTH_WAYS && force == 1 &&
               (is_atomic (a[i]) || is_around (a[i])) &&
               (is_atomic (a[j]) || is_around (a[j])))
        ins= "*";

      if (is_around (a[j]))
        if (ins == " " || (ins == "*" && force == -1))
          ins= "";
      if (a[j] == ".") ins= "";
      while (i+1 < N(a) && (is_func (a[i+1], RSUB, 1) ||
                            is_func (a[i+1], RSUP, 1) ||
                            is_func (a[i+1], RPRIME, 1))) {
        i++;
        r << a[i];
      }
      if (ins != "") r << tree (ins);
    }
  }
  return r;
}

tree
invisible_corrector::correct (tree t, string mode) {
  //cout << "Correct " << t << ", " << mode << "\n";
  tree r= t;
  if (is_compound (t)) {
    int i, n= N(t);
    r= tree (t, n);
    for (i=0; i<n; i++) {
      string smode= get_submode (t, i, mode);
      if (is_func (t, WITH) && i != N(t)-1)
        r[i]= t[i];
      else if (is_correctable_child (t, i))
        r[i]= correct (t[i], smode);
      else r[i]= t[i];
    }
  }
  
  if (mode == "math") {
    array<tree> a= concat_tokenize (r);
    a= correct (a);
    tree ret= concat_recompose (a);
    //if (ret != r)
    //  cout << "<< " << r << " >>" << LF
    //       << ">> " << ret << " <<" << LF;
    return ret;
  }
  else return r;
}

tree
missing_invisible_correct (tree t, int force) {
  // force = -1, only correct when sure, and when old markup is incorrect
  // force = 0 , only correct when pretty sure
  // force = 1 , correct whenever reasonable (used for LaTeX import)
  with_drd drd (get_document_drd (t));
  invisible_corrector corrector (t, force);
  //cout << "Times before " << corrector.times_before << "\n";
  //cout << "Space before " << corrector.space_before << "\n";
  //cout << "Times after  " << corrector.times_after  << "\n";
  //cout << "Space after  " << corrector.space_after  << "\n";
  return corrector.correct (t, "text");
}

tree
missing_invisible_correct_twice (tree t, int force= -1) {
  tree u= missing_invisible_correct (t, force);
  if (u == t) return t;
  return missing_invisible_correct (u, force);
}

/******************************************************************************
* Miscellaneous corrections
******************************************************************************/

tree
misc_math_correct (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "math", 1) && is_func (t[0], RSUB, 1))
    return tree (RSUB, compound ("math", misc_math_correct (t[0][0])));
  else if (is_compound (t, "math", 1) && is_func (t[0], RSUP, 1))
    return tree (RSUP, compound ("math", misc_math_correct (t[0][0])));
  else if (is_func (t, RSUB, 1) && is_func (t[0], RSUB, 1))
    return misc_math_correct (t[0]);
  else if (is_func (t, RSUB, 1) && is_func (t[0], RSUP, 1))
    return misc_math_correct (tree (RSUB, t[0][0]));
  else if (is_func (t, RSUP, 1) && is_func (t[0], RSUB, 1))
    return misc_math_correct (tree (RSUP, t[0][0]));
  else if (is_func (t, RSUP, 1) && is_func (t[0], RSUP, 1))
    return misc_math_correct (t[0]);
  else if (is_func (t, RSUP, 1) && is_func (t[0], RPRIME, 1))
    return misc_math_correct (t[0]);
  else if (is_script (t) && is_compound (t[0], "text", 1) &&
           is_atomic (t[0][0]) && is_alpha (t[0][0]->label))
    {
      if (N(t[0][0]->label) != 1) return tree (L(t), t[0][0]);
      else return tree (L(t), tree (WITH, "math-font-family", "trm",
                                    misc_math_correct (t[0])));
    }
  else if (is_compound (t, "math", 1)) {
    tree arg = misc_math_correct (t[0]);
    tree last= arg;
    if (is_concat (last) && N(last) > 0) last= last[N(last)-1];
    if (is_atomic (last) && N(last->label) > 0 &&
        is_punctuation (last->label [N(last->label)-1]))
      {
        string s= last->label;
        int i= N(s);
        while (i>0 && is_punctuation (s[i-1])) i--;
        if (i == N(s)) return compound ("math", arg);
        string tail= s (i, N(s));
        s= s (0, i);
        if (last == arg) {
          if (N(s) == 0) return tail;
          else return concat (compound ("math", s), tail);
        }
        else {
          tree cc= arg (0, N(arg) - 1);
          if (N(s) != 0) cc << tree (s);
          if (N(cc) == 1) cc= cc[0];
          return concat (compound ("math", cc), tail);
        }        
      }
    else return compound ("math", arg);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= misc_math_correct (t[i]);
    if (is_concat (r))
      r= concat_recompose (concat_decompose (r));
    return r;
  }
}

/******************************************************************************
* Count errors
******************************************************************************/

static int
count_math_formula_errors (tree t, int mode) {
  if (mode == 1) return 1;
  if (packrat_correct ("std-math", "Main", t)) return 0;
  else {
    if (mode == 2) cout << "  ERROR> " << t << "\n";
    return 1;
  }
}

static int
count_math_table_errors (tree t, int mode) {
  if (is_atomic (t)) return 0;
  else if (is_func (t, CELL, 1)) {
    if (t[0] == "" || t[0] == tree (DOCUMENT, "")) return 0;
    if (mode == 1) return 1;
    if (packrat_correct ("std-math", "Cell", t[0])) return 0;
    else {
      if (mode == 2) cout << "  ERROR> " << t << "\n";
      return 1;
    }
  }
  else {
    int sum= 0;
    for (int i=0; i<N(t); i++)
      sum += count_math_table_errors (t[i], mode);
    return sum;
  }
}

int
count_math_errors (tree t, int mode) {
  if (is_atomic (t)) return 0;
  else {
    int sum= 0;
    for (int i=0; i<N(t); i++) {
      tree cmode= the_drd->get_env_child (t, i, MODE, "text");
      if (cmode != "math") sum += count_math_errors (t[i], mode);
      else {
        tree u= t[i];
        while (is_func (u, DOCUMENT, 1) ||
               is_func (u, TFORMAT) ||
               is_func (u, WITH))
          u= u[N(u)-1];
        if (is_func (u, TABLE)) sum += count_math_table_errors (u, mode);
        else sum += count_math_formula_errors (u, mode);
      }
    }
    return sum;
  }
}

/******************************************************************************
* Print mathematical status
******************************************************************************/

static int count_formula= 0;
static int count_initial_errors= 0;
static int count_final_errors= 0;

static int corrected_with= 0;
static int corrected_superfluous_with= 0;
static int corrected_brackets= 0;
static int corrected_move_brackets= 0;
static int corrected_misc= 0;
static int corrected_superfluous_invisible= 0;
static int corrected_homoglyph= 0;
static int corrected_missing_invisible= 0;
static int corrected_zealous_invisible= 0;

void
math_status_cumul_sub (tree t, int& cumul, int& errors) {
  int new_errors= count_math_errors (t);
  cumul += (errors - new_errors);
  errors= new_errors;
}

void
math_status_cumul (tree t) {
  with_drd drd (get_document_drd (t));
  if (is_func (t, DOCUMENT))
    for (int i=0; i<N(t); i++)
      if (is_compound (t[i], "body", 1)) {
        t= t[i][0];
        break;
      }

  int errors= count_math_errors (t);
  count_formula += count_math_errors (t, 1);
  count_initial_errors += errors;
  t= with_correct (t);
  math_status_cumul_sub (t, corrected_with, errors);
  t= superfluous_with_correct (t);
  math_status_cumul_sub (t, corrected_superfluous_with, errors);
  t= upgrade_brackets (t);
  math_status_cumul_sub (t, corrected_brackets, errors);
  t= move_brackets (t);
  math_status_cumul_sub (t, corrected_move_brackets, errors);
  t= misc_math_correct (t);
  math_status_cumul_sub (t, corrected_misc, errors);
  t= superfluous_invisible_correct (t);
  math_status_cumul_sub (t, corrected_superfluous_invisible, errors);
  t= homoglyph_correct (t);
  math_status_cumul_sub (t, corrected_homoglyph, errors);
  t= superfluous_invisible_correct (t);
  math_status_cumul_sub (t, corrected_superfluous_invisible, errors);
  t= missing_invisible_correct (t);
  math_status_cumul_sub (t, corrected_missing_invisible, errors);
  count_final_errors += errors;
  //cout << "Errors= " << errors << "\n";
  //(void) count_math_errors (t, 2);
  t= missing_invisible_correct (t, 1);
  math_status_cumul_sub (t, corrected_zealous_invisible, errors);
}

void
math_status_reset () {
  count_formula= 0;
  count_initial_errors= 0;
  count_final_errors= 0;
  corrected_with= 0;
  corrected_superfluous_with= 0;
  corrected_brackets= 0;
  corrected_move_brackets= 0;
  corrected_misc= 0;
  corrected_superfluous_invisible= 0;
  corrected_homoglyph= 0;
  corrected_missing_invisible= 0;
}

void
math_status_print () {
  cout << "Formulas       : " << count_formula << "\n";
  cout << "Initial errors : " << count_initial_errors << "\n";
  cout << "Final errors   : " << count_final_errors << "\n";
  cout << "\n";
  cout << "With corrected                  : "
       << corrected_with << "\n";
  cout << "Superfluous with corrected      : "
       << corrected_superfluous_with << "\n";
  cout << "Upgraded brackets               : "
       << corrected_brackets << "\n";
  cout << "Moved brackets                  : "
       << corrected_move_brackets << "\n";
  cout << "Miscellaneous corrected         : "
       << corrected_misc << "\n";
  cout << "Superfluous invisible corrected : "
       << corrected_superfluous_invisible << "\n";
  cout << "Homoglyphs corrected            : "
       << corrected_homoglyph << "\n";
  cout << "Missing invisible corrected     : "
       << corrected_missing_invisible << "\n";
  cout << "Zealous invisible corrected     : "
       << corrected_zealous_invisible << "\n";
  cout << "\n";
}

/******************************************************************************
* Master routines
******************************************************************************/

bool
enabled_preference (string s) {
  return call ("get-preference", s) == object ("on");
}

tree
latex_correct (tree t) {
  // NOTE: matching brackets corrected in upgrade_tex
  t= misc_math_correct (t);
  //if (enabled_preference ("remove superfluous invisible"))
  t= superfluous_invisible_correct (t);
  //if (enabled_preference ("homoglyph correct"))
  t= homoglyph_correct (t);
  //if (enabled_preference ("remove superfluous invisible"))
  t= superfluous_invisible_correct (t);
  //if (enabled_preference ("insert missing invisible"))
  t= missing_invisible_correct_twice (t);
  //if (enabled_preference ("insert missing invisible"))
  t= missing_invisible_correct (t, 1);
  t= downgrade_big (t);
  t= correct_missing_block (t);
  t= correct_concat_block (t);
  return t;
}

tree
automatic_correct (tree t, string version) {
  if (version_inf_eq (version, "1.0.7.9")) {
    t= misc_math_correct (t);
    if (enabled_preference ("remove superfluous invisible"))
      t= superfluous_invisible_correct (t);
    if (enabled_preference ("homoglyph correct"))
      t= homoglyph_correct (t);
    if (enabled_preference ("remove superfluous invisible"))
      t= superfluous_invisible_correct (t);
    if (enabled_preference ("insert missing invisible"))
      t= missing_invisible_correct_twice (t);
    if (enabled_preference ("zealous invisible correct"))
      t= missing_invisible_correct (t, 1);
  }
  t= downgrade_big (t);
  return t;
}

tree
manual_correct (tree t) {
  t= with_correct (t);
  t= superfluous_with_correct (t);
  t= upgrade_brackets (t);
  t= misc_math_correct (t);
  if (enabled_preference ("manual remove superfluous invisible"))
    t= superfluous_invisible_correct (t);
  if (enabled_preference ("manual homoglyph correct"))
    t= homoglyph_correct (t);
  if (enabled_preference ("manual remove superfluous invisible"))
    t= superfluous_invisible_correct (t);
  if (enabled_preference ("manual insert missing invisible"))
    t= missing_invisible_correct_twice (t);
  if (enabled_preference ("manual zealous invisible correct"))
    t= missing_invisible_correct (t, 1);
  t= downgrade_big (t);
  return t;
}
