
/******************************************************************************
* MODULE     : upgradetm.cpp
* DESCRIPTION: upgrade old TeXmacs formats
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "converter.hpp"
#include "hashset.hpp"
#include "path.hpp"
#include "vars.hpp"
#include "drd_std.hpp"
#include <stdio.h>
#include "scheme.hpp"
#include "tree_correct.hpp"
#include "merge_sort.hpp"

static bool upgrade_tex_flag= false;
double get_magnification (string s);

/******************************************************************************
* Retrieve older operator hashmap
******************************************************************************/

static void
rename_feature (hashmap<string,int>& H, string old_name, string new_name) {
  H (old_name)= H [new_name];
  H->reset (new_name);
}

static void
new_feature (hashmap<string,int>& H, string new_name) {
  H->reset (new_name);
}

/*static*/ hashmap<string,int>
get_codes (string version) {
  hashmap<string,int> H (UNKNOWN);
  H->join (STD_CODE);

  if (version_inf ("1.0.7.6", version)) return H;

  rename_feature (H, "group", "rigid");
  rename_feature (H, "postscript", "image");

  if (version_inf ("1.0.6.9", version)) return H;

  rename_feature (H, "frozen", "freeze");

  if (version_inf ("1.0.6.2", version)) return H;

  new_feature (H, "expand-as");
  new_feature (H, "locus");
  new_feature (H, "id");
  new_feature (H, "hard-id");
  new_feature (H, "link");
  new_feature (H, "url");
  new_feature (H, "script");

  if (version_inf ("1.0.4.1", version)) return H;

  new_feature (H, "copy");
  new_feature (H, "cm-length");
  new_feature (H, "mm-length");
  new_feature (H, "in-length");
  new_feature (H, "pt-length");
  new_feature (H, "bp-length");
  new_feature (H, "dd-length");
  new_feature (H, "pc-length");
  new_feature (H, "cc-length");
  new_feature (H, "fs-length");
  new_feature (H, "fbs-length");
  new_feature (H, "em-length");
  new_feature (H, "ln-length");
  new_feature (H, "sep-length");
  new_feature (H, "yfrac-length");
  new_feature (H, "ex-length");
  new_feature (H, "fn-length");
  new_feature (H, "fns-length");
  new_feature (H, "bls-length");
  new_feature (H, "spc-length");
  new_feature (H, "xspc-length");
  new_feature (H, "par-length");
  new_feature (H, "pag-length");
  new_feature (H, "tmpt-length");
  new_feature (H, "px-length");
  new_feature (H, "tmlen");

  if (version_inf ("1.0.3.12", version)) return H;

  new_feature (H, "unquote*");

  if (version_inf ("1.0.3.4", version)) return H;

  new_feature (H, "for-each");
  new_feature (H, "quasi");
  rename_feature (H, "hold", "quasiquote");
  rename_feature (H, "release", "unquote");

  if (version_inf ("1.0.3.3", version)) return H;

  new_feature (H, "quote-value");
  new_feature (H, "quote-arg");
  new_feature (H, "mark");
  new_feature (H, "use-package");
  new_feature (H, "style-only");
  new_feature (H, "style-only*");
  new_feature (H, "rewrite-inactive");
  new_feature (H, "inline-tag");
  new_feature (H, "open-tag");
  new_feature (H, "middle-tag");
  new_feature (H, "close-tag");

  if (version_inf ("1.0.2.8", version)) return H;

  rename_feature (H, "raw_data", "raw-data");
  rename_feature (H, "sub_table", "subtable");
  rename_feature (H, "drd_props", "drd-props");
  rename_feature (H, "get_label", "get-label");
  rename_feature (H, "get_arity", "get-arity");
  rename_feature (H, "map_args", "map-args");
  rename_feature (H, "eval_args", "eval-args");
  rename_feature (H, "find_file", "find-file");
  rename_feature (H, "is_tuple", "is-tuple");
  rename_feature (H, "look_up", "look-up");
  rename_feature (H, "var_if", "if*");
  rename_feature (H, "var_inactive", "inactive*");
  rename_feature (H, "var_active", "active*");
  rename_feature (H, "text_at", "text-at");
  rename_feature (H, "var_spline", "spline*");
  rename_feature (H, "old_matrix", "old-matrix");
  rename_feature (H, "old_table", "old-table");
  rename_feature (H, "old_mosaic", "old-mosaic");
  rename_feature (H, "old_mosaic_item", "old-mosaic-item");
  rename_feature (H, "var_expand", "expand*");
  rename_feature (H, "hide_expand", "hide-expand");

  rename_feature (H, "with_limits", "with-limits");
  rename_feature (H, "line_break", "line-break");
  rename_feature (H, "new_line", "new-line");
  rename_feature (H, "line_separator", "line-sep");
  rename_feature (H, "next_line", "next-line");
  rename_feature (H, "no_line_break", "no-break");
  rename_feature (H, "no_first_indentation", "no-indent");
  rename_feature (H, "enable_first_indentation", "yes-indent");
  rename_feature (H, "no_indentation_after", "no-indent*");
  rename_feature (H, "enable_indentation_after", "yes-indent*");
  rename_feature (H, "page_break_before", "page-break*");
  rename_feature (H, "page_break", "page-break");
  rename_feature (H, "no_page_break_before", "no-page-break*");
  rename_feature (H, "no_page_break_after", "no-page-break");
  rename_feature (H, "new_page_before", "new-page*");
  rename_feature (H, "new_page", "new-page");
  rename_feature (H, "new_double_page_before", "new-dpage*");
  rename_feature (H, "new_double_page", "new-dpage");

  if (version_inf ("1.0.2.5", version)) return H;

  new_feature (H, "compound");
  new_feature (H, "xmacro");
  new_feature (H, "get_label");
  new_feature (H, "get_arity");
  new_feature (H, "map_args");
  new_feature (H, "eval_args");
  new_feature (H, "drd_props");

  if (version_inf ("1.0.2.0", version)) return H;

  new_feature (H, "with_limits");
  new_feature (H, "line_break");
  new_feature (H, "new_line");
  new_feature (H, "line_separator");
  new_feature (H, "next_line");
  new_feature (H, "no_line_break");
  new_feature (H, "no_first_indentation");
  new_feature (H, "enable_first_indentation");
  new_feature (H, "no_indentation_after");
  new_feature (H, "enable_indentation_after");
  new_feature (H, "page_break_before");
  new_feature (H, "page_break");
  new_feature (H, "no_page_break_before");
  new_feature (H, "no_page_break_after");
  new_feature (H, "new_page_before");
  new_feature (H, "new_page");
  new_feature (H, "new_double_page_before");
  new_feature (H, "new_double_page");

  if (version_inf ("1.0.1.25", version)) return H;

  new_feature (H, "active");
  new_feature (H, "var_inactive");
  new_feature (H, "var_active");
  new_feature (H, "attr");

  if (version_inf ("1.0.0.20", version)) return H;

  new_feature (H, "text_at");

  if (version_inf ("1.0.0.19", version)) return H;

  new_feature (H, "find_file");

  if (version_inf ("1.0.0.14", version)) return H;

  rename_feature (H, "paragraph", "para");

  if (version_inf ("1.0.0.5", version)) return H;

  new_feature (H, "var_if");
  new_feature (H, "hide_expand");

  if (version_inf ("1.0.0.2", version)) return H;

  new_feature (H, "superpose");
  new_feature (H, "spline");
  new_feature (H, "var_spline");
  new_feature (H, "cspline");
  new_feature (H, "fill");

  if (version_inf ("0.3.5.2", version)) return H;

  new_feature (H, "raw_data");
  new_feature (H, "include");

  if (version_inf ("0.3.5.1", version)) return H;

  new_feature (H, "var_expand");

  if (version_inf ("0.3.4.12", version)) return H;

  new_feature (H, "range");
  new_feature (H, "is_tuple");
  new_feature (H, "look_up");

  if (version_inf ("0.3.4.11", version)) return H;

  new_feature (H, "float");
  new_feature (H, "datoms");
  new_feature (H, "dlines");
  new_feature (H, "dpages");
  new_feature (H, "pageref");

  if (version_inf ("0.3.4.7", version)) return H;

  rename_feature (H, "matrix", "old_matrix");
  rename_feature (H, "table", "old_table");
  rename_feature (H, "mosaic", "old_mosaic");
  rename_feature (H, "mosaic_item", "old_mosaic_item");

  if (version_inf ("0.3.4.6", version)) return H;

  new_feature (H, "tformat");
  new_feature (H, "twith");
  new_feature (H, "cwith");
  new_feature (H, "tmarker");
  new_feature (H, "row");
  new_feature (H, "cell");
  new_feature (H, "sub_table");

  if (version_inf ("0.3.4.0", version)) return H;

  new_feature (H, "tag");
  new_feature (H, "syntax");

  if (version_inf_eq ("0.3.3.15", version)) return H;

  new_feature (H, "uninit");
  new_feature (H, "error");
  new_feature (H, "surround");
  new_feature (H, "hold");
  new_feature (H, "release");
  new_feature (H, "arg");

  if (version_inf_eq ("0.3.3.0", version)) return H;

  new_feature (H, "with");
  new_feature (H, "macro");
  new_feature (H, "eval");
  new_feature (H, "value");
  new_feature (H, "or");
  new_feature (H, "xor");
  new_feature (H, "and");
  new_feature (H, "not");
  new_feature (H, "over");
  new_feature (H, "divide");
  new_feature (H, "modulo");
  new_feature (H, "length");
  new_feature (H, "date");
  new_feature (H, "equal");
  new_feature (H, "unequal");
  new_feature (H, "less");
  new_feature (H, "lesseq");
  new_feature (H, "greater");
  new_feature (H, "greatereq");
  new_feature (H, "if");
  new_feature (H, "case");
  new_feature (H, "for");
  new_feature (H, "while");
  new_feature (H, "extern");
  new_feature (H, "authorize");

  if (version_inf_eq ("0.3.1.8", version)) return H;

  rename_feature (H, "mosaic item", "mosaic_item");
  rename_feature (H, "<>", "symbol");
  rename_feature (H, ";", "backup");
  rename_feature (H, "'", "quote");
  rename_feature (H, ":=", "assign");
  rename_feature (H, "\\", "apply");
  rename_feature (H, "()", "tuple");
  rename_feature (H, "{,}", "collection");
  rename_feature (H, "->", "associate");
  rename_feature (H, "+", "plus");
  rename_feature (H, "-", "minus");
  rename_feature (H, "x", "times");
  rename_feature (H, "*", "merge");
  rename_feature (H, "nr", "number");
  H ("style")= H ["()"];

  return H;
}

/******************************************************************************
* Old style is_expand predicates
******************************************************************************/

static bool
is_expand (tree t) {
  return ((L(t) == EXPAND) || (L(t) == VAR_EXPAND) || (L(t) == HIDE_EXPAND));
}

static bool
is_expand (tree t, string s, int n) {
  return is_expand (t) && (N(t) == n+1) && (t[0] == s);
}

/******************************************************************************
* Old style conversion from TeXmacs strings to TeXmacs trees
******************************************************************************/

static tree
string_to_tree (string s, int& pos, hashmap<string,int> codes) {
  string l ("");
  while ((pos<N(s)) && (s[pos]!='(') && (s[pos]!=',') && (s[pos]!=')')) {
    if ((s[pos]=='\\') && (pos<N(s)-1)) pos++;
    l << s[pos++];
  }
  tree t (l);
  tree_label code= (tree_label) codes [l];
  if ((l == "style") || (code == COLLECTION)) t= tree (code);
  if ((pos<N(s)) && (s[pos]=='(')) {
    if (code != UNKNOWN) t= tree (code);
    else t= tuple (l);
    do {
      pos++;
      t << string_to_tree (s, pos, codes);
    } while ((pos<N(s)) && (s[pos]==','));
    if ((pos<N(s)) && (s[pos]==')')) pos++;
  }
  return t;
}

static tree
un_paragraph (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, PARA, 1)) return t[0];
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) r[i]= un_paragraph (t[i]);
    return r;
  }
}

/*static*/ tree
string_to_tree (string s, string version) {
  int pos=0;
  return un_paragraph (string_to_tree (s, pos, get_codes (version)));
}

/******************************************************************************
* Upgrade large delimiters, big operators and primes
******************************************************************************/

tree
upgrade_textual (tree t, path& mode_stack) {
  if (t == "") return t;
  if (is_atomic (t)) {
    int i, n= N(t->label);
    string s;
    tree r (CONCAT);
    for (i=0; i<n; ) {
      if (t->label[i] == '<') {
        int start= i;
        for (i++; i<n; i++)
          if (t->label[i-1] == '>') break;
        string ss= t->label (start, i);
        if (t->label[i-1] != '>') ss << '>';
        if (starts (ss, "<left-")) {
          if (s != "") { r << s; s= ""; }
          r << tree (LEFT, ss (6, N(ss)-1));
        }
        else if (starts (ss, "<mid-")) {
          if (s != "") { r << s; s= ""; }
          r << tree (MID, ss (5, N(ss)-1));
        }
        else if (starts (ss, "<right-")) {
          if (s != "") { r << s; s= ""; }
          r << tree (RIGHT, ss (7, N(ss)-1));
        }
        else if (starts (ss, "<big-")) {
          if (s != "") { r << s; s= ""; }
          r << tree (BIG, ss (5, N(ss)-1));
        }
        else s << ss;
      }
      else if (((t->label[i] == '\'') || (t->label[i] == '`')) &&
               (!is_nil (mode_stack)) && (mode_stack->item == 1))
        {
          int start= i++;
          while ((i<n) && (t->label[i] == t->label[i-1])) i++;
          if (s != "") { r << s; s= ""; }
          tree_label op= t->label[start] == '`'? LPRIME: RPRIME;
          r << tree (op, t->label (start, i));
        }
      else s << t->label[i++];
    }
    if (s != "") r << s;
    if (N(r) == 1) return r[0];
    return r;
  }
  else {
    int i, n= arity (t);
    tree r (t, 0);
    for (i=0; i<n; i++) {
      tree u= upgrade_textual (t[i], mode_stack);
      if (is_func (u, SET)) {
        if (u == tree (SET, "mode", "text")) mode_stack= path (0, mode_stack);
        if (u == tree (SET, "mode", "math")) mode_stack= path (1, mode_stack);
        if (u == tree (SET, "mode", "prog")) mode_stack= path (2, mode_stack);
      }
      else if (is_func (u, RESET)) {
        if (u == tree (RESET, "mode"))
          if (!is_nil (mode_stack))
            mode_stack= mode_stack->next;
      }
      else if (is_func (u, BEGIN, 1)) {
        if ((u[0] == "equation") ||
            (u[0] == "equation*") ||
            (u[0] == "eqnarray*") ||
            (u[0] == "leqnarray*"))
          mode_stack= path (1, mode_stack);
      }
      else if (is_func (u, END, 1)) {
        if ((u[0] == "equation") ||
            (u[0] == "equation*") ||
            (u[0] == "eqnarray*") ||
            (u[0] == "leqnarray*"))
          if (!is_nil (mode_stack))
            mode_stack= mode_stack->next;
      }
      if (is_concat (t) && is_concat (u)) r << A(u);
      else r << u;
    }
    return r;
  }
}

/******************************************************************************
* Upgrade lambda application -> macro expansion and value keyword
******************************************************************************/

tree
upgrade_apply_expand_value (tree t, hashset<string> H) {
  if (is_atomic (t)) return t;
  else {
    int i, n= arity (t);
    tree r (t, n);
    if (is_func (t, APPLY))
      if ((n >= 1) && is_atomic (t[0]) && H->contains (t[0]->label)) {
        if (n == 1) r= tree (VALUE, n);
        else r= tree (EXPAND, n);
      }
    for (i=0; i<n; i++)
      r[i]= upgrade_apply_expand_value (t[i], H);
    return r;
  }
}

typedef const char* charp;
static charp apply_expand_value_strings[]= {
  "part", "part*", "chapter", "chapter*", "appendix",
  "section", "section*", "subsection", "subsection*",
  "subsubsection", "subsubsection*",
  "paragraph", "paragraph*", "subparagraph", "subparagraph*",
  "footnote", "item*", "overline", "underline",
  "mathord", "mathbin", "mathopen", "mathpunct",
  "mathop", "mathrel", "mathclose", "mathalpha",
  "op", "strong", "em", "tt", "name", "samp", "abbr",
  "dfn", "kbd", "var", "acronym", "person",
  "menu", "submenu", "subsubmenu", "tmdef", "tmref",
  "key", "skey", "ckey", "akey", "mkey", "hkey",
  "include-document", "include-project", "globalize-variable",
  "localize-variable", "assign-variable",
  "gb", "cgb", "gbt", "cgbt", "head", "tail", "hm", "tm", "binom",
  //"ma", "mb", "md", "me", "mf", "mg", "mh", "mi", "mj", "mk",
  //"mm", "mn", "mu", "mv", "mw", "my", "mz",
  //"MA", "MB", "MD", "ME", "MF", "MG", "MH", "MI", "MJ", "MK",
  //"MM", "MN", "MU", "MV", "MW", "MY", "MZ",
  ""
};

tree
upgrade_apply_expand_value (tree t) {
  int i;
  hashset<string> H;
  for (i=0; apply_expand_value_strings[i][0] != '\0'; i++)
    H->insert (apply_expand_value_strings[i]);
  return upgrade_apply_expand_value (t, H);
}

/******************************************************************************
* Subroutines for upgrading set/reset -> with, begin/end -> apply
******************************************************************************/

static bool
matching (tree open, tree close) {
  if (is_func (open, SET, 2))
    return is_func (close, RESET, 1) && (open[0] == close[0]);
  if (is_func (open, BEGIN)) {
    if (is_func (close, END, 1))
      return open[0] == close[0];
    if (is_func (close, END, 2))
      return open[0] == "algo-repeat" && open[0] == close[0];
  }
  return false;
}

static tree
with_replace (tree var, tree val, tree body) {
  if (is_func (body, WITH)) {
    int i, n= N(body);
    tree t= tree (WITH, n+2);
    t[0]= var;
    t[1]= val;
    for (i=0; i<n; i++) t[i+2]= body[i];
    return t;
  }
  else return tree (WITH, var, val, body);
}

static tree
expand_replace (tree begin, tree end, tree body) {
  int i, j, k= N(begin), l= N(end);
  tree expand (EXPAND, k + l);
  for (i=0; i<k; i++) expand[i]= begin[i];
  for (j=1; j<l; i++, j++) expand[i]= end[j];
  expand[i]= body;
  return expand;
}

static void
concat_search (tree t, int& i, tree open= "") {
  bool set_reset= (open == "") || is_func (open, SET) || is_func (open, RESET);
  bool begin_end= (open == "") || is_func (open, BEGIN) || is_func (open, END);
  int n= N(t);
  while (i<n) {
    if (set_reset && is_func (t[i], SET, 2)) return;
    if (set_reset && is_func (t[i], RESET, 1)) return;
    if (begin_end && is_func (t[i], BEGIN)) return;
    if (begin_end && is_func (t[i], END, 1)) return;
    if (begin_end && is_func (t[i], END, 2) &&
        t[i][0] == "algo-repeat") return;
    i++;
  }
}

static tree
concat_replace (tree t, int i1, int i2) {
  int i;
  tree v (CONCAT);
  for (i=i1+1; i<i2; i++) v << t[i];
  if (N(v)==0) v= "";
  else if (N(v)==1) v= v[0];
  if (is_func (t[i1], SET))
    return with_replace (t[i1][0], t[i1][1], v);
  else return expand_replace (t[i1], t[i2], v);
}

static tree
document_explode (tree t) {
  int i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++)
    if (is_concat (t[i])) u[i]= t[i];
    else u[i]= tree (CONCAT, t[i]);
  return u;
}

static tree
document_contract (tree t) {
  int i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++)
    if (N(t[i]) == 0) u[i]= "";
    else if (N(t[i]) == 1) u[i]= t[i][0];
    else u[i]= t[i];
  return u;
}

static void
document_search (tree t, int& i, int& j, tree open= "") {
  bool set_reset= (open == "") || is_func (open, SET) || is_func (open, RESET);
  bool begin_end= (open == "") || is_func (open, BEGIN) || is_func (open, END);
  int n= N(t);
  while (i<n) {
    int k= N(t[i]);
    while (j<k) {
      if (set_reset && is_func (t[i][j], SET, 2)) return;
      if (set_reset && is_func (t[i][j], RESET, 1)) return;
      if (begin_end && is_func (t[i][j], BEGIN)) return;
      if (begin_end && is_func (t[i][j], END, 1)) return;
      if (begin_end && is_func (t[i][j], END, 2) &&
          t[i][j][0] == "algo-repeat") return;
      j++;
    }
    i++;
    j=0;
  }
}

static void
document_inc (tree doc_t, int& doc_i, int& con_i) {
  con_i++;
  if (con_i == N(doc_t[doc_i])) {
    doc_i++;
    con_i= 0;
  }
}

static void
document_inc (tree& doc, tree& con, tree doc_t, int& doc_i, int& con_i) {
  con_i++;
  if (con_i == N(doc_t[doc_i])) {
    doc << con;
    con= tree (CONCAT);
    doc_i++;
    con_i= 0;
  }
}

static void
document_merge (tree& doc, tree& con, tree doc_t,
                int doc_1, int con_1, int doc_2, int con_2)
{
  int doc_i= doc_1, con_i= con_1;
  while ((doc_i<doc_2) || ((doc_i==doc_2) && (con_i<con_2))) {
    con << doc_t[doc_i][con_i];
    document_inc (doc, con, doc_t, doc_i, con_i);
  }
}

static tree
document_replace (tree doc_t, int doc_1, int con_1, int doc_2, int con_2) {
  tree doc_b (DOCUMENT), con_b (CONCAT);
  int doc_i= doc_1, con_i= con_1;
  document_inc (doc_b, con_b, doc_t, doc_i, con_i);
  document_merge (doc_b, con_b, doc_t, doc_i, con_i, doc_2, con_2);
  doc_b << con_b;
  doc_b= document_contract (doc_b);
  bool flag= (doc_1!=doc_2) || ((con_1==0) && (con_2==N(doc_t[doc_2])-1));
  /*
  if (N(doc_b) != (doc_2-doc_1+1))
    cout << (doc_2-doc_1+1) << ", " << doc_b << "\n";
  */
  if ((!flag) && (N(doc_b)==1)) doc_b= doc_b[0];
  if (is_func (doc_t[doc_1][con_1], SET))
    return with_replace (doc_t[doc_1][con_1][0],
                         doc_t[doc_1][con_1][1],
                         doc_b);
  else
    return expand_replace (doc_t[doc_1][con_1],
                           doc_t[doc_2][con_2],
                           doc_b);
}

/******************************************************************************
* Upgrade set/reset -> with, begin/end -> apply
******************************************************************************/

static tree upgrade_set_begin (tree t);

static tree
upgrade_set_begin_default (tree t) {
  int i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++)
    u[i]= upgrade_set_begin (t[i]);
  return u;
}

static tree
upgrade_set_begin_concat_once (tree t) {
  // cout << "in : " << t << "\n";
  int i=0, n= N(t);
  tree u (CONCAT);
  while (i<n) {
    int i0=i, i1, i2;
    concat_search (t, i);
    i1= i;
    for (i=i0; i<i1; i++) u << t[i];
    if (i==n) {
      // cout << "  " << i0 << ", " << i1 << "\n";
      break;
    }
    i++;
    concat_search (t, i, t[i1]);
    i2= i;
    // cout << "  " << i0 << ", " << i1 << ", " << i2 << "\n";
    if ((i2<n) && matching (t[i1], t[i2])) {
      u << concat_replace (t, i1, i2);
      i= i2+1;
    }
    else {
      i= i1;
      if (i == i0) u << t[i++];
    }
  }
  // cout << "out: " << u << "\n";
  // cout << "-------------------------------------------------------------\n";
  // fflush (stdout);
  return u;
}

static tree
upgrade_set_begin_concat (tree t) {
  tree u= t;
  do { t= u; u= upgrade_set_begin_concat_once (t); } while (u != t);
  u= upgrade_set_begin_default (u);
  if (N(u) == 1) return u[0];
  return u;
}

static void
upgrade_verbatim_expand (tree& doc, tree& con, tree ins) {
  tree& body= ins[N(ins)-1];
  if (is_document (body) && (N(body)>1)) {
    int n= N(body);
    int start=0, end=n;
    if (body[0] == "") start= 1;
    if (body[n-1] == "") end= n-1;
    body= body (start, end);
    if (start != 0) {
      doc << con;
      con= tree (CONCAT);
    }
    con << ins;
    if (end != n) {
      doc << con;
      con= tree (CONCAT);
    }
  }
  else con << ins;
}

static void
upgrade_abstract_expand (tree& doc, tree& con, tree ins) {
  (void) doc;
  tree& body= ins[N(ins)-1];
  if (is_document (body) && (N(body) > 1) && (body[0] == ""))
    body= body (1, N(body));
  con << ins;
}

static tree
upgrade_set_begin_document_once (tree doc_t) {
  // cout << "in : " << doc_t << "\n";
  int doc_i=0, con_i=0;
  tree doc (DOCUMENT), con (CONCAT);
  while ((doc_i < N(doc_t)) && (con_i < N(doc_t[doc_i]))) {
    int doc_0= doc_i, con_0= con_i;
    document_search (doc_t, doc_i, con_i);
    int doc_1= doc_i, con_1= con_i;
    // cout << "  0: " << doc_0 << ", " << con_0 << "\n";
    // cout << "  1: " << doc_1 << ", " << con_1 << "\n";
    document_merge (doc, con, doc_t, doc_0, con_0, doc_1, con_1);
    if (doc_i == N(doc_t)) break;
    document_inc (doc_t, doc_i, con_i);
    document_search (doc_t, doc_i, con_i, doc_t[doc_1][con_1]);
    int doc_2= doc_i, con_2= con_i;
    // cout << "  2: " << doc_2 << ", " << con_2 << "\n";
    if ((doc_2 < N(doc_t)) &&
        matching (doc_t[doc_1][con_1], doc_t[doc_2][con_2]))
      {
        tree ins= document_replace (doc_t, doc_1, con_1, doc_2, con_2);
        // cout << "ins: " << ins << "\n";
        if (is_func (ins, EXPAND, 2)) {
          if ((ins[0] == "verbatim") || (ins[0] == "code") ||
              (upgrade_tex_flag && is_verbatim (compound (as_string(ins[0])))))
            upgrade_verbatim_expand (doc, con, ins);
          else if (ins[0] == "abstract")
            upgrade_abstract_expand (doc, con, ins);
          else con << ins;
        }
        else if (is_func (ins, WITH) &&
                 is_func (ins[N(ins)-1], DOCUMENT, 1)) {
          ins[N(ins)-1]= ins[N(ins)-1][0];
          con << ins;
        }
        else con << ins;
        document_inc (doc, con, doc_t, doc_i, con_i);
      }
    else {
      doc_i= doc_1; con_i= con_1;
      if ((doc_i == doc_0) && (con_i == con_0)) {
        con << doc_t[doc_i][con_i];
        document_inc (doc, con, doc_t, doc_i, con_i);     
      }
    }
  }
  // cout << "out: " << doc << "\n";
  // cout << "-------------------------------------------------------------\n";
  fflush (stdout);
  return doc;
}

static tree
upgrade_set_begin_document (tree t) {
  tree u= t;
  do {
    t= u;
    u= document_explode (u);
    u= upgrade_set_begin_document_once (u);
    u= document_contract (u);
  } while (u != t);
  u= upgrade_set_begin_default (u);
  return u;
}

static tree
upgrade_set_begin_surround (tree t, tree search, bool& found) {
  if (t == search) {
    found= true;
    if (upgrade_tex_flag)
      return tree (DOCUMENT, copy (t));
    return copy (t);
  }
  if (is_func (t, WITH) || is_func (t, EXPAND)) {
    tree u= copy (t);
    u[N(u)-1]= upgrade_set_begin_surround (u[N(u)-1], search, found);
    return u;
  }
  if (is_concat (t)) {
    int i, n= N(t), searching= !found;
    tree left (CONCAT), middle, right (CONCAT);
    for (i=0; i<n; i++) {
      middle= upgrade_set_begin_surround (t[i], search, found);
      if (searching && found) break;
      else left << middle;
    }
    if (i==n) return copy (t);
    for (i++; i<n; i++)
      right << upgrade_set_begin_surround (t[i], search, found);
    if (N(left) == 0) left= "";
    else if (N(left) == 1) left= left[0];
    if (N(right) == 0) right= "";
    else if (N(right) == 1) right= right[0];
    return tree (SURROUND, left, right, middle);
  }
  return copy (t);
}

static tree
upgrade_env_args (tree t, tree env) {
  if (is_atomic (t)) return t;
  else if (is_func (t, APPLY, 1)) {
    int i, k= N(env);
    for (i=0; i<k-2; i++)
      if (t[0] == env[i])
        return tree (ARG, t[0]);
    return t;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_env_args (t[i], env);
    return r;
  }
}

static tree
upgrade_set_begin_env (tree t) {
  //cout << "in  : " << t << "\n";
  int i, n= N(t);
  tree u (MACRO, n);
  for (i=0; i<n-2; i++)
    u[i]= upgrade_set_begin (t[i]);
  string s= "body";
  for (i=0; i<n-2; i++)
    if (t[i] == "body") s= "body*";
  u[n-2]= copy (s);

  tree begin= t[n-2], end= t[n-1], body (CONCAT);
  if (begin == "") begin= tree (CONCAT);
  else if (!is_concat (begin)) begin= tree (CONCAT, begin);
  if (end == "") end= tree (CONCAT);
  else if (!is_concat (end)) end= tree (CONCAT, end);
  body << A(begin) << tree (ARG, copy (s)) << A(end);
  //cout << "mid1: " << body << "\n";
  body= upgrade_set_begin_concat (body);
  body= upgrade_env_args (body, t);
  //cout << "mid2: " << body << "\n";
  bool found= false;
  u[n-1]= upgrade_set_begin_surround (body, tree (ARG, s), found);
  //cout << "out : " << u << "\n";
  //cout << "-------------------------------------------------------------\n";
  return u;
}

static tree
upgrade_set_begin (tree t) {
  if (is_atomic (t)) return copy (t);
  else {
    if (is_concat (t)) return upgrade_set_begin_concat (t);
    else if (is_document (t)) return upgrade_set_begin_document (t);
    else if (is_func (t, ENV)) return upgrade_set_begin_env (t);
    else return upgrade_set_begin_default (t);
  }
}

static tree
eliminate_set_begin (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, SET) || is_func (t, RESET) ||
      is_func (t, BEGIN) || is_func (t, END) ||
      is_func (t, ENV)) return "";

  int i, n= N(t);
  if (is_concat (t)) {
    tree r (CONCAT);
    for (i=0; i<n; i++) {
      tree u= eliminate_set_begin (t[i]);
      if (u != "") r << u;
    }
    if (N(r) == 0) return "";
    if (N(r) == 1) return r[0];
    return r;
  }
  else {
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= eliminate_set_begin (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade surround, indentation after and final routine
******************************************************************************/

bool
expand_needs_surrounding (string s) {
  return
    (s == "maketitle") || (s == "abstract") ||
    (s == "theorem") || (s == "proposition") || (s == "lemma") ||
    (s == "corollary") || (s == "proof") || (s == "axiom") ||
    (s == "definition") || (s == "notation") || (s == "conjecture") ||
    (s == "remark") || (s == "note") || (s == "example") ||
    (s == "exercise") || (s == "warning") ||
    (s == "convention") || (s == "acknowledgments") ||
    (s == "code") || (s == "quote") ||
    (s == "quotation") || (s == "verse") || (s == "center") ||
    (s == "indent") || (s == "body") || (s == "description") ||
    starts (s, "itemize") || starts (s, "enumerate");
}

static bool
with_needs_surrounding (string s) {
  return
    (s == "paragraph mode") || (s == "paragraph hyphenation") ||
    (s == "paragraph width") || (s == "left margin") ||
    (s == "right margin") || (s == "first indentation") ||
    (s == "last indentation") || (s == "no first indentation") ||
    (s == "no last indentation") || (s == "interline space") ||
    (s == "horizontal ink separation") || (s == "line stretch") ||
    (s == "interparagraph space");
}

static bool
needs_surrounding (tree t) {
  if (is_multi_paragraph (t)) return true;
  if ((is_func (t, APPLY) || is_func (t, EXPAND)) && is_atomic (t[0])) {
    if (t[0] == "verbatim") return (N(t)==2) && is_multi_paragraph (t[1]);
    return expand_needs_surrounding (t[0]->label);
  }
  if (is_func (t, WITH)) {
    int i, n= N(t)-1;
    for (i=0; i<n; i+=2)
      if (is_atomic (t[i]) && with_needs_surrounding (t[i]->label))
        return true;
  }
  return false;
}

static bool
needs_transfer (tree t) {
  return
    is_func (t, EXPAND, 2) &&
    ((t[0] == "equation") || (t[0] == "equation*") ||
     (t[0] == "eqnarray*") || (t[0] == "leqnarray*"));
}

static tree
upgrade_surround (tree t) {
  if (upgrade_tex_flag || is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_document (t) && is_concat (u) && (N(u)>1)) {
      int j, k= N(u);
      for (j=0; j<k; j++)
        if (needs_surrounding (u[j])) {
          tree before= u (0  , j);
          tree after = u (j+1, k);
          tree body  = upgrade_surround (u[j]);
          if (N(before)==0) before= "";
          if (N(before)==1) before= before[0];
          if (N(after )==0) after = "";
          if (N(after )==1) after = after [0];
          before= upgrade_surround (before);
          after = upgrade_surround (after );
          r[i]= tree (SURROUND, before, after, body);
          break;
        }
        else if (needs_transfer (u[j])) {
          tree temp= upgrade_surround (u[j][1]);
          if (!is_concat (temp)) temp= tree (CONCAT, temp);
          tree body= u (0, j);
          body << A (temp) << A (u (j+1, k));
          r[i]= tree (EXPAND, u[j][0], body);
          break;
        }
      if (j<k) continue;
    }
    r[i]= upgrade_surround (u);
  }
  return r;
}

static tree
upgrade_indent (tree t) {
  if (is_atomic (t)) return t;
  else if (t == tree (ASSIGN, "no first indentation", "true"))
    return tree (FORMAT, "no indentation after");
  else if (t == tree (ASSIGN, "no first indentation", "false"))
    return tree (FORMAT, "enable indentation after");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_indent (t[i]);
    return r;
  }
}

static tree
upgrade_equations (tree t) {
  if (!upgrade_tex_flag || is_atomic (t)) return t;
  else if (needs_transfer (t) && !is_document (t[1])) {
    t[1]= document (upgrade_equations (t[1]));
    return t;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_equations (t[i]);
    return r;
  }
}

static tree
upgrade_new_environments (tree t) {
  t= upgrade_set_begin (t);
  t= eliminate_set_begin (t);
  t= upgrade_surround (t);
  t= upgrade_indent (t);
  t= upgrade_equations (t);
  return t;
}

/******************************************************************************
* Upgrade items
******************************************************************************/

static tree
upgrade_items (tree t) {
  if (is_atomic (t)) return t;
  else if ((t == tree (APPLY, "item")) || (t == tree (VALUE, "item")))
    return tree (EXPAND, "item");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_items (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade resize
******************************************************************************/

static tree
upgrade_resize_arg (tree t, int type) {
  if (!is_atomic (t)) return "";
  string s= t->label;
  if ((s == "same") || (s == "ink")) return "";
  if (type == 1) s= "l[" * s;
  if (type == 2) s= "b[" * s;
  if (type == 3) s= "r]" * s;
  if (type == 4) s= "t]" * s;
  return s;
}

static tree
upgrade_resize (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, RESIZE, 6)) {
    tree r (RESIZE, t[0]);
    int extend= (t[1] == "extend"? 1: 0);
    r << upgrade_resize_arg (t[2], 1 * extend)
      << upgrade_resize_arg (t[3], 2 * extend)
      << upgrade_resize_arg (t[4], 3 * extend)
      << upgrade_resize_arg (t[5], 4 * extend);
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_resize (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade tables
******************************************************************************/

static void
handle_mosaic_format (tree& fm, tree t, int i, int j) {
  string align = as_string (t[1]);
  string hspan = as_string (t[2]);
  string vspan = as_string (t[3]);
  string col   = as_string (t[4]);

  string halign= "l";
  string valign= "B";
  if (N(align)>=2) {
    switch (align[0]) {
    case 'n': valign= "t"; break;
    case 'c': valign= "c"; break;
    case '0': valign= "B"; break;
    case 's': valign= "s"; break;
    }
    switch (align[1]) {
    case 'w': halign= "l"; break;
    case '0': halign= "L"; break;
    case 'c': halign= "c"; break;
    case 'e': halign= "r"; break;
    }
  }
  if ((col == "none") || (col == "")) col= "";
  else col= "foreground";

  tree w (CWITH);
  w << as_string (i+1) << as_string (j+1)
    << as_string (i+1) << as_string (j+1);

  if (halign != "l") {
    tree with= copy (w);
    with << "cell halign" << halign;
    fm << with;
  }
  if (valign != "B") {
    tree with= copy (w);
    with << "cell valign" << valign;
    fm << with;
  }
  if (hspan != "1") {
    tree with= copy (w);
    with << "cell hspan" << hspan;
    fm << with;
  }
  if (vspan != "1") {
    tree with= copy (w);
    with << "cell vspan" << vspan;
    fm << with;
  }
  if (col != "") {
    tree with= copy (w);
    with << "cell background" << col;
    fm << with;
  }
}


static tree
upgrade_table (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, OLD_MATRIX) ||
           is_func (t, OLD_TABLE) ||
           is_func (t, OLD_MOSAIC) ||
           (is_func (t, TFORMAT) && is_func (t[N(t)-1], OLD_MATRIX)))
    {
      tree ft (TFORMAT);
      if (is_func (t, TFORMAT)) {
        ft= t (0, N(t)-1);
        t = t [N(t)-1];
      }
      if (is_func (t, OLD_MOSAIC)) {
        tree with (CWITH);
        with << "1" << "-1" << "1" << "-1" << "cell mode" << "c";
        ft << with;
      }

      int i, j;
      int nr_rows= as_int (t[N(t)-1]);
      int nr_cols= as_int (t[N(t)-2]);
      tree tt (TABLE, nr_rows);
      for (i=0; i<nr_rows; i++) {
        tree rt (ROW, nr_cols);
        for (j=0; j<nr_cols; j++) {
          tree c= upgrade_table (t[i*nr_cols+j]);
          if (is_func (c, OLD_MOSAIC_ITEM)) {
            handle_mosaic_format (ft, c, i, j);
            c= c[0];
          }
          rt[j]= tree (CELL, c);
        }
        tt[i]= rt;
      }

      ft << tt;
      tree xt (EXPAND, "tabular*", ft);
      if (is_func (t, OLD_TABLE)) xt[0]= "block*";
      if (is_func (t, OLD_MOSAIC)) xt[0]= "tabular";
      return xt;
    }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_table (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade splits
******************************************************************************/

static tree
upgrade_split (tree t, bool eq= false) {
  int i, n= N(t);
  if (is_atomic (t)) return t;
  else if (is_func (t, SURROUND, 3) && is_func (t[0], SPLIT)) {
    tree u= t[2];
    if (!is_concat (u)) u= tree (CONCAT, t[0], u);
    else u= tree (CONCAT, t[0]) * u;
    return tree (SURROUND, "", upgrade_split (t[1]), upgrade_split (u));
  }
  else if (is_func (t, SURROUND, 3) && is_concat (t[0])) {
    tree r (CONCAT);
    tree split ("");
    for (i=0; i<N(t[0]); i++)
      if (is_func (t[0][i], SPLIT)) split= t[0][i];
      else r << t[0][i];
    tree u= t[2];
    if (split != "") {
      if (!is_concat (u)) u= tree (CONCAT, split, u);
      else u= tree (CONCAT, split) * u;
    }
    r= tree (SURROUND, upgrade_split (r),
             upgrade_split (t[1]), upgrade_split (u));
    return r;
  }
  else if (is_concat (t)) {
    tree r (CONCAT);
    tree split ("");
    int nr_rows=1, nr_cols=1, sep=1;
    for (i=0; i<n; i++)
      if (is_func (t[i], SPLIT)) split= t[i];
      else {
        tree u= upgrade_split (t[i]);
        if (u == tree (FORMAT, "line separator")) sep++;
        if (u == tree (FORMAT, "next line")) {
          nr_cols= max (sep, nr_cols);
          sep= 1;
          nr_rows++;
        }
        r << u;
      }
    nr_cols= max (sep, nr_cols);
    if (split == "" && nr_cols == 1 && !eq) return r;
    else {
      int col=0, row=0;
      tree T (TABLE, nr_rows);
      for (row=0; row<nr_rows; row++) {
        tree R (ROW, nr_cols);
        for (col=0; col<nr_cols; col++) R[col]= tree (CELL, "");
        T[row]= R;
      }

      tree u (CONCAT);
      row= col= 0;
      for (i=0; i<N(r); i++)
        if ((r[i] == tree (FORMAT, "line separator")) ||
            (r[i] == tree (FORMAT, "next line")))
          {
            if (N(u) == 0) u= "";
            else if (N(u) == 1) u= u[0];
            T[row][col][0]= u;
            u= tree (CONCAT);
            if (r[i] == tree (FORMAT, "line separator")) col++;
            else {
              row++;
              col= 0;
            }
          }
        else u << r[i];
      if (N(u) == 0) u= "";
      else if (N(u) == 1) u= u[0];
      T[row][col][0]= u;
      r= T;
    }

    tree tf (TFORMAT);
    if (split != "") {
      tf << tree (TWITH, "table hyphen", "y")
         << tree (TWITH, "table width", "1par")
         << tree (TWITH, "table min cols", as_string (N (split)))
         << tree (TWITH, "table max cols", as_string (N (split)))
         << tree (CWITH, "1", "-1", "1", "1", "cell lsep", "0spc")
         << tree (CWITH, "1", "-1", "-1", "-1", "cell rsep", "0spc")
         << tree (CWITH, "1", "-1", "1", "-1", "cell bsep", "0sep")
         << tree (CWITH, "1", "-1", "1", "-1", "cell tsep", "0sep")
         << tree (CWITH, "1", "-1", "1", "1", "cell hyphen", "b")
         << tree (CWITH, "1", "-1", "-1", "-1", "cell hyphen", "t");
      if (split[0] == "right")
        tf << tree (CWITH, "1", "-1", "1", "1", "cell hpart", "1");
      if ((split[N(split)-1] == "left") || (split[N(split)-1] == "justify"))
        tf << tree (CWITH, "1", "-1", "-1", "-1", "cell hpart", "1");
      for (i=0; i<N(split); i++) {
        tree with (CWITH);
        int j= (i==N(split)-1)? -1: i+1;
        with << "1" << "-1" << as_string (j) << as_string (j) << "cell halign";
        if (split[i] == "right") with << "r";
        else if (split[i] == "center") with << "c";
        else with << "l";
        tf << with;
      }
    }
    if (r == tree (CONCAT)) r= "";
    else if (is_func (r, CONCAT, 1)) r= r[0];
    tf << r;
    if ((split != "") && is_func (r, TABLE))
      return tree (EXPAND, "tabular*", tf);
    return tf;
  }
  else {
    tree r (t, n);
    if (n == 1 || is_func (t, EXPAND, 2)) {
      string s= as_string (L(t));
      if (is_func (t, EXPAND, 2) && is_atomic (t[0])) s= t[0]->label;
      if (ends (s, "*")) s= s (0, N(s)-1);
      if (s == "eqnarray" || s == "align" || s == "multline" ||
          s == "gather" || s == "eqsplit") {
        tree arg= t[n-1];
        if (is_func (arg, DOCUMENT, 1)) arg= arg[0];
        if (!is_concat (arg)) arg= tree (CONCAT, arg);
        r= copy (t);
        r[n-1]= upgrade_split (arg, true);
        return r;
      }
    }
    else if (upgrade_tex_flag && (n == 2 || is_func (t, EXPAND, 3))) {
      string s= as_string (L(t));
      if (is_func (t, EXPAND, 3) && is_atomic (t[0])) s= t[0]->label;
      if (ends (s, "*")) s= s (0, N(s)-1);
      if (s == "alignat") {
        tree arg= t[n-1];
        if (is_func (arg, DOCUMENT, 1)) arg= arg[0];
        if (!is_concat (arg)) arg= tree (CONCAT, arg);
        r= copy (t);
        r[n-1]= upgrade_split (arg, true);
        return r;
      }
    }
    for (i=0; i<n; i++)
      r[i]= upgrade_split (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade projects
******************************************************************************/

static tree
upgrade_project (tree t) {
  if (is_atomic (t)) return t;
  else if (is_expand (t, "include-document", 1))
    return tree (VAR_INCLUDE, t[1]);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_project (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade title
******************************************************************************/

static tree
upgrade_title (tree t, tree& tit, tree& auth, tree& meta) {
  if (is_atomic (t)) return t;
  else if (is_func (t, APPLY, 2) ||
           is_func (t, EXPAND, 2)) {
    if (t[0] == "title") {
      tit << tree (EXPAND, "title", t[1]); return ""; }
    if (t[0] == "author") {
      auth << tree (EXPAND, "author", t[1]); return ""; }
    if (t[0] == "address") {
      auth << tree (EXPAND, "address", t[1]); return ""; }
    if (t[0] == "urladdr") {
      auth << tree (EXPAND, "title-web", t[1]); return ""; }
    if (t[0] == "title-email") {
      auth << tree (EXPAND, "title-email", t[1]); return ""; }
    if (t[0] == "title-thanks") {
      meta << tree (EXPAND, "title-thanks", t[1]); return ""; }
    if (t[0] == "keywords") {
      meta << tree (EXPAND, "title-keywords", t[1]); return ""; }
    if (t[0] == "subjclass") {
      meta << tree (EXPAND, "title-ams-class", t[1]); return ""; }
    if (t[0] == "classification") {
      meta << tree (EXPAND, "title-ams-class", t[1]); return ""; }
  }
  else if (is_func (t, APPLY, 3) ||
           is_func (t, EXPAND, 3)) {
    if (t[0] == "subjclass*") {
      meta << tree (EXPAND, "title-ams-class", t[2]); return ""; }
  }
  else if ((t == tree (APPLY, "maketitle")) ||
           (t == tree (EXPAND, "maketitle")))
    {
      tree doc (DOCUMENT);
      doc << A (tit);
      doc << A (auth);
      doc << A (meta);
      doc << tree (EXPAND, "title-date", tree (_DATE, ""));
      return tree (EXPAND, "make-title", doc);
    }

  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_title (t[i], tit, auth, meta);
  return r;
}

static tree
upgrade_title (tree t) {
  tree tit (DOCUMENT), auth (DOCUMENT), meta (DOCUMENT);
  return simplify_correct (upgrade_title (t, tit, auth, meta));
}

/******************************************************************************
* Upgrade cas
******************************************************************************/

static void
upgrade_cas_search (tree t, tree& style) {
  if (is_atomic (t));
  else if (is_expand (t, "session", 3)) {
    if (!is_atomic (t[1])) return;
    string l= copy (t[1]->label);
    if (l == "scheme") return;
    if (l == "shell") return;
    if (l == "gTybalt") l= "gtybalt";
    if (l == "Macaulay2") l= "macaulay2";
    int i, n= N(style);
    for (i=0; i<n; i++)
      if (style[i] == l) return;
    style << l;
  }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      upgrade_cas_search (t[i], style);
  }
}

static void
set_document_attribute (tree doc, string attr, tree val) {
  int i, n= arity (doc);
  for (i=0; i<n; i++)
    if ((is_func (doc[i], EXPAND, 2) || is_func (doc[i], APPLY, 2)) &&
        (doc[i][0] == attr))
      {
        doc[i][1]= val;
        return;
      }
  doc << tree (EXPAND, attr, val);
}

static tree
upgrade_cas (tree doc) {
  tree style= copy (extract (doc, "style"));
  upgrade_cas_search (doc, style);
  doc= copy (doc);
  set_document_attribute (doc, "style", style);
  return doc;
}

/******************************************************************************
* Upgrade modified symbols
******************************************************************************/

static bool
is_with (tree t, string var, string val) {
  return is_func (t, WITH, 3) && (t[0] == var) && (t[1] == val);
}

static bool
is_var_with (tree t, string var, string val) {
  return is_with (t, var, val) || is_with (t, replace (var, " ", "-"), val);
}

static bool
is_alpha (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return (N(s) == 1) && is_alpha (s[0]);
}

static bool
is_alpha_numeric (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return (N(s) == 1) && (is_alpha (s[0]) || is_numeric (s[0]));
}

static bool
is_upper (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return (N(s) == 1) && (s[0] >= 'A') && (s[0] <= 'Z');
}

static bool
is_bold (tree t) {
  if (is_compound (t)) return false;
  if (is_alpha_numeric (t)) return true;
  string s= locase_all (t->label);
  return
    (s == "<alpha>") || (s == "<beta>") || (s == "<gamma>") ||
    (s == "<delta>") || (s == "<epsilon>") || (s == "<zeta>") ||
    (s == "<eta>") || (s == "<theta>") || (s == "<iota>") ||
    (s == "<kappa>") || (s == "<lambda>") || (s == "<mu>") ||
    (s == "<nu>") || (s == "<xi>") || (s == "<omicron>") ||
    (s == "<pi>") || (s == "<rho>") || (s == "<sigma>") ||
    (s == "<tau>") || (s == "<upsilon>") || (s == "<phi>") ||
    (s == "<psi>") || (s == "<chi>") || (s == "<omega>") ||
    (s == "<varepsilon>") || (s == "<vartheta>") || (s == "<varkappa>") ||
    (s == "<varpi>") || (s == "<varrho>") || (s == "<varsigma>") ||
    (s == "<varphi>") || (s == "<backepsilon>") || (s == "<mho>") ||
    (s == "<Backepsilon>") || (s == "<Mho>") || (s == "<ell>");
}

static tree
upgrade_mod_symbol (string prefix, string s) {
  if (N(s) == 1) return "<" * prefix * s * ">";
  else return "<" * prefix * s (1, N(s)-1) * ">";
}

static tree
upgrade_mod_symbols (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, WITH) && N(t) > 3 &&
      (t[0] == "mode" ||
       (is_atomic (t[0]) && starts (t[0]->label, "math"))) &&
      is_atomic (t[2]) && starts (t[2]->label, "math")) {
    tree u (WITH, t[0], t[1], t (2, N(t)));
    tree r= upgrade_mod_symbols (u);
    if (is_atomic (r)) return r;
    if (is_with (r, "mode", "math") && is_atomic (r[2])) return r;
  }
  if (is_var_with (t, "math font series", "bold") && is_bold (t[2]))
    return upgrade_mod_symbol ("b-", t[2]->label);
  else if (is_var_with (t, "math font", "cal") && is_upper (t[2]))
    return upgrade_mod_symbol ("cal-", t[2]->label);
  else if (is_var_with (t, "math font", "Euler") && is_alpha (t[2]))
    return upgrade_mod_symbol ("frak-", t[2]->label);
  else if (is_var_with (t, "math font", "Bbb") && is_alpha (t[2]))
    return upgrade_mod_symbol ("bbb-", t[2]->label);
  else if (is_var_with (t, "math font", "Bbb*") && is_alpha (t[2]))
    return upgrade_mod_symbol ("bbb-", t[2]->label);
  else if (is_var_with (t, "math font series", "bold") &&
           is_var_with (t[2], "math font", "cal") && is_upper (t[2][2]))
    return upgrade_mod_symbol ("b-cal-", t[2][2]->label);
  else if (is_var_with (t, "math font", "cal") &&
           is_var_with (t[2], "math font series", "bold") && is_upper (t[2][2]))
    return upgrade_mod_symbol ("b-cal-", t[2][2]->label);
  //else if ((is_func (t, VALUE, 1) || is_func (t, EXPAND, 1) ||
  //         is_func (t, APPLY, 1)) && (is_atomic (t[0]))) {
  //  string s= t[0]->label;
  //  if ((N(s) == 2) && ((s[0]=='m') && (s[1]>='a') && s[1]<='z'))
  //    return upgrade_mod_symbol ("frak-", s(1,2));
  //  if ((N(s) == 2) && ((s[0]=='M') && (s[1]>='A') && s[1]<='Z'))
  //    return upgrade_mod_symbol ("frak-", s(1,2));
  //  return t;
  //}
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_mod_symbols (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrading menus in the help
******************************************************************************/

static tree
upgrade_menus_in_help (tree t) {
  if (is_atomic (t)) return t;
  if (is_expand (t, "menu", 1) || is_expand (t, "submenu", 2) ||
      is_expand (t, "subsubmenu", 3) || is_expand (t, "subsubsubmenu", 4)) {
    int i, n= N(t);
    tree r (APPLY, n);
    r[0]= "menu";
    for (i=1; i<n; i++) r[i]= t[i];
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_menus_in_help (t[i]);
    return r;
  }
}

static tree
capitalize_sub (tree t) {
  if (is_atomic (t)) return upcase_first (t->label);
  else return t;
}

static tree
upgrade_capitalize_menus (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, APPLY) && (t[0] == "menu")) {
    int i, n= N(t);
    tree r (APPLY, n);
    r[0]= "menu";
    for (i=1; i<n; i++) r[i]= capitalize_sub (t[i]);
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_capitalize_menus (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade branches
******************************************************************************/

static tree
upgrade_traverse_branch (tree t) {
  if (is_atomic (t)) return t;
  else if (is_expand (t, "branch", 3) ||
           (is_func (t, APPLY, 4) && (t[0] == "branch")))
    return tree (APPLY, t[0], t[1], t[3]);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_traverse_branch (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade sessions
******************************************************************************/

static tree
upgrade_session (tree t) {
  if (is_atomic (t)) return t;
  else if (is_expand (t, "session", 3)) {
    tree u= tree (EXPAND, "session", t[3]);
    tree w= tree (WITH);
    w << PROG_LANGUAGE << t[1] << PROG_SESSION << t[2] << u;
    return w;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_session (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade sessions
******************************************************************************/

static tree
upgrade_formatting (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, FORMAT, 1)) {
    string name= replace (t[0]->label, " ", "-");
    if (name == "line-separator") name= "line-sep";
    else if (name == "no-line-break") name= "no-break";
    else if (name == "no-first-indentation") name= "no-indent";
    else if (name == "enable-first-indentation") name= "yes-indent";
    else if (name == "no-indentation-after") name= "no-indent*";
    else if (name == "enable-indentation-after") name= "yes-indent*";
    else if (name == "page-break-before") name= "page-break*";
    else if (name == "no-page-break-before") name= "no-page-break*";
    else if (name == "no-page-break-after") name= "no-page-break";
    else if (name == "new-page-before") name= "new-page*";
    else if (name == "new-double-page-before") name= "new-dpage*";
    else if (name == "new-double-page") name= "new-dpage";
    return tree (as_tree_label (name));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_formatting (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade expand
******************************************************************************/

static tree
upgrade_expand (tree t, tree_label WHICH_EXPAND) {
  if (is_atomic (t)) return t;
  else if (is_func (t, WHICH_EXPAND) && is_atomic (t[0])) {
    int i, n= N(t)-1;
    string s= t[0]->label;
    if (s == "quote") s= s * "-env";
    tree_label l= make_tree_label (s);
    tree r (l, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_expand (t[i+1], WHICH_EXPAND);
    return r;
  }
  else if (is_func (t, ASSIGN, 2) &&
           (t[0] == "quote") &&
           is_func (t[1], MACRO)) {
    tree arg= upgrade_expand (t[1], WHICH_EXPAND);
    return tree (ASSIGN, t[0]->label * "-env", arg);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_expand (t[i], WHICH_EXPAND);
    return r;
  }
}

static tree
upgrade_xexpand (tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    if (is_expand (t))
      r= tree (COMPOUND, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_xexpand (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade apply
******************************************************************************/

static tree
upgrade_apply (tree t) {
  if (is_atomic (t)) return t;
  /*
  if (is_func (t, APPLY))
    cout << t[0] << "\n";
  */
  if (is_func (t, APPLY) && is_atomic (t[0])) {
    int i, n= N(t)-1;
    string s= t[0]->label;
    tree_label l= make_tree_label (s);
    tree r (l, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_apply (t[i+1]);
    return r;
  }

  int i, n= N(t);
  tree r (t, n);
  if (is_func (t, APPLY))
    r= tree (COMPOUND, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_apply (t[i]);
  return r;
}

static tree
upgrade_function_arg (tree t, tree var) {
  if (is_atomic (t)) return t;
  else if ((t == tree (APPLY, var)) || (t == tree (VALUE, var)))
    return tree (ARG, var);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_function_arg (t[i], var);
    return r;
  }
}

static tree
upgrade_function (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, ASSIGN, 2) && is_func (t[1], FUNC)) {
    int i, n= N(t[1])-1;
    for (i=0; i<n; i++)
      if (ends (as_string (t[1][i]), "*"))
        cout << "TeXmacs] Deprecated argument list '" << t[1][i]
             << "' in function '" << t[0] << "'\n"
             << "TeXmacs] You should use the 'xmacro' primitive now\n";
  }
  /*
  if (is_func (t, ASSIGN, 2) && is_func (t[1], FUNC) && (N(t[1])>1)) {
    cout << "Function: " << t[0] << "\n";
  }
  */
  if (is_func (t, FUNC)) {
    int i, n= N(t)-1;
    tree u= t[n], r (MACRO, n+1);
    for (i=0; i<n; i++) {
      u= upgrade_function_arg (u, t[i]);
      r[i]= copy (t[i]);
    }
    r[n]= upgrade_function (u);
    /*
    if (n > 0) {
      cout << "t= " << t << "\n";
      cout << "r= " << r << "\n";
      cout << HRULE;
    }
    */
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_function (t[i]);
    return r;
  }
}

/******************************************************************************
* Renaming environment variables
******************************************************************************/

static charp var_rename []= {
  "shrinking factor", "sfactor",
  "info flag", "info-flag",

  "font family", "font-family",
  "font series", "font-series",
  "font shape", "font-shape",
  "font size", "font-size",
  "font base size", "font-base-size",
  "background color", "bg-color",
  "atom decorations", "atom-decorations",
  "line decorations", "line-decorations",
  "page decorations", "page-decorations",
  "xoff decorations", "xoff-decorations",
  "yoff decorations", "yoff-decorations",

  "math language", "math-language",
  "math font", "math-font",
  "math font family", "math-font-family",
  "math font series", "math-font-series",
  "math font shape", "math-font-shape",
  "index level", "math-level",
  "formula style", "math-display",
  "math condensed", "math-condensed",
  "vertical position", "math-vpos",

  "prog language", "prog-language",
  "prog font", "prog-font",
  "prog font family", "prog-font-family",
  "prog font series", "prog-font-series",
  "prog font shape", "prog-font-shape",
  "this session", "prog-session",

  "paragraph mode", "par-mode",
  "paragraph hyphenation", "par-hyphen",
  "paragraph width", "par-width",
  "left margin", "par-left",
  "right margin", "par-right",
  "first indentation", "par-first",
  "no first indentation", "par-no-first",
  "interline space", "par-sep",
  "horizontal ink separation", "par-hor-sep",
  "line stretch", "par-line-sep",
  "interparagraph space", "par-par-sep",
  "interfootnote space", "par-fnote-sep",
  "nr columns", "par-columns",
  "column separation", "par-columns-sep",

  "page medium", "page-medium",
  "page type", "page-type",
  "page orientation", "page-orientation",
  "page breaking", "page-breaking",
  "page flexibility", "page-flexibility",
  "page number", "page-nr",
  "thepage", "page-the-page",
  "page width", "page-width",
  "page height", "page-height",
  "odd page margin", "page-odd",
  "even page margin", "page-even",
  "page right margin", "page-right",
  "page top margin", "page-top",
  "page bottom margin", "page-bot",
  "page extend", "page-extend",
  "page shrink", "page-shrink",
  "page header separation", "page-head-sep",
  "page footer separation", "page-foot-sep",
  "odd page header", "page-odd-header",
  "odd page footer", "page-odd-footer",
  "even page header", "page-even-header",
  "even page footer", "page-even-footer",
  "this page header", "page-this-header",
  "this page footer", "page-this-footer",
  "reduction page left margin", "page-reduce-left",
  "reduction page right margin", "page-reduce-right",
  "reduction page top margin", "page-reduce-top",
  "reduction page bottom margin", "page-reduce-bot",
  "show header and footer", "page-show-hf",
  "footnote separation", "page-fnote-sep",
  "footnote bar length", "page-fnote-barlen",
  "float separation", "page-float-sep",
  "marginal note separation", "page-mnote-sep",
  "marginal note width", "page-mnote-width",

  "table width", "table-width",
  "table height", "table-height",
  "table hmode", "table-hmode",
  "table vmode", "table-vmode",
  "table halign", "table-halign",
  "table valign", "table-valign",
  "table row origin", "table-row-origin",
  "table col origin", "table-col-origin",
  "table lsep", "table-lsep",
  "table rsep", "table-rsep",
  "table bsep", "table-bsep",
  "table tsep", "table-tsep",
  "table lborder", "table-lborder",
  "table rborder", "table-rborder",
  "table bborder", "table-bborder",
  "table tborder", "table-tborder",
  "table hyphen", "table-hyphen",
  "table min rows", "table-min-rows",
  "table min cols", "table-min-cols",
  "table max rows", "table-max-rows",
  "table max cols", "table-max-cols",

  "cell format", "cell-format",
  "cell decoration", "cell-decoration",
  "cell background", "cell-background",
  "cell orientation", "cell-orientation",
  "cell width", "cell-width",
  "cell height", "cell-height",
  "cell hpart", "cell-hpart",
  "cell vpart", "cell-vpart",
  "cell hmode", "cell-hmode",
  "cell vmode", "cell-vmode",
  "cell halign", "cell-halign",
  "cell valign", "cell-valign",
  "cell lsep", "cell-lsep",
  "cell rsep", "cell-rsep",
  "cell bsep", "cell-bsep",
  "cell tsep", "cell-tsep",
  "cell lborder", "cell-lborder",
  "cell rborder", "cell-rborder",
  "cell bborder", "cell-bborder",
  "cell tborder", "cell-tborder",
  "cell vcorrect", "cell-vcorrect",
  "cell hyphen", "cell-hyphen",
  "cell row span", "cell-row-span",
  "cell col span", "cell-col-span",
  "cell row nr", "cell-row-nr",
  "cell col nr", "cell-col-nr",

  "line width", "line-width",
  "line style", "line-style",
  "line arrows", "line-arrows",
  "line caps", "line-join",
  "fill mode", "fill-mode",
  "fill color", "fill-color",
  "fill style", "fill-style",

  "graphical frame", "gr-frame",
  "graphical clip", "gr-clip",
  "graphical mode", "gr-mode",
  "graphical color", "gr-color",
  "graphical line width", "gr-line-width",
  
  ""
};

static hashmap<string,string> var_rename_table ("?");

static hashmap<string,string>
cached_renamer (charp* T, hashmap<string,string>& H) {
  if (N (H) == 0) {
    int i;
    for (i=0; T[i][0] != '\0'; i+=2)
      H (T[i])= T[i+1];
  }
  return H;
}


static tree
rename_vars (tree t, hashmap<string,string> H, bool flag) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    static tree_label MARKUP= make_tree_label ("markup");
    for (i=0; i<n; i++) {
      tree u= rename_vars (t[i], H, flag);
      if (is_atomic (u) && H->contains (u->label))
        if (((L(t) == WITH) && ((i%2) == 0) && (i < n-1)) ||
            ((L(t) == ASSIGN) && (i == 0)) ||
            ((L(t) == VALUE) && (i == 0)) ||
            ((L(t) == CWITH) && (i == 4)) ||
            ((L(t) == TWITH) && (i == 0)) ||
            ((L(t) == ASSOCIATE) && (i == 0)) ||
            ((L(t) == MARKUP) && (i == 0)))
          u= copy (H[u->label]);
      r[i]= u;
    }
    if (flag) {
      if (H->contains (as_string (L(t)))) {
        tree_label l= make_tree_label (H[as_string (L(t))]);
        r= tree (l, A(r));
      }
    }
    else {
      if ((n == 0) && H->contains (as_string (L(t)))) {
        string v= H[as_string (L(t))];
        r= tree (VALUE, copy (v));
        if (v == "page-the-page") r= tree (make_tree_label ("page-the-page"));
      }
    }
    return r;
  }
}

tree
upgrade_env_vars (tree t) {
  return rename_vars (t, cached_renamer (var_rename, var_rename_table), false);
}

/******************************************************************************
* Use package primitive for style files
******************************************************************************/

static tree
upgrade_use_package (tree t) {
  tree style= extract (t, "style");
  tree init = extract (t, "initial");
  bool preamble= false;
  int i, n= N(init);
  for (i=0; i<n; i++)
    if (init[i] == tree (ASSOCIATE, PREAMBLE, "true"))
      preamble= true;

  bool no_style= true;
  if (preamble) {
    n= N(t);
    tree r (L(t));
    for (i=0; i<n; i++)
      if (is_compound (t[i], "style")) {
        r << compound ("style", "source");
        no_style= false;
      }
      else if (is_compound (t[i], "body", 1) && is_document (t[i][0])) {
        tree v (USE_PACKAGE);
        v << A (style);
        tree u (DOCUMENT);
        if (N(v) > 0) u << v;
        u << A (t[i][0]);
        if (no_style) r << compound ("style", "source");
        r << compound ("body", u);
      }
      else r << t[i];
    return r;
  }
  else return t;
}

/******************************************************************************
* Normalize names of tags in the style files
******************************************************************************/

static charp style_rename []= {
  "thelabel", "the-label",

  "leftflush", "left-flush",
  "rightflush", "right-flush",
  "mathord", "math-ord",
  "mathopen", "math-open",
  "mathclose", "math-close",
  "mathpunct", "math-punct",
  "mathbin", "math-bin",
  "mathrel", "math-rel",
  "mathop", "math-op",
  "thetoc", "the-toc",
  "theidx", "the-idx",
  "thegly", "the-gly",
  "theitem", "the-item",
  "tocnr", "toc-nr",
  "idxnr", "idx-nr",
  "glynr", "gly-nr",
  "itemnr", "item-nr",
  "itemname", "item-name",
  "newitemize", "new-itemize",
  "newenumerate", "new-enumerate",
  "newdescription", "new-description",

  "nextnumber", "next-number",
  "eqnumber", "eq-number",
  "leqnumber", "leq-number",
  "reqnumber", "req-number",
  "nonumber", "no-number",
  "thefootnote", "the-footnote",
  "theequation", "the-equation",
  "thetheorem", "the-theorem",
  "theproposition", "the-proposition",
  "thelemma", "the-lemma",
  "thecorollary", "the-corollary",
  "theaxiom", "the-axiom",
  "thedefinition", "the-definition",
  "thenotation", "the-notation",
  "theconjecture", "the-conjecture",
  "theremark", "the-remark",
  "theexample", "the-example",
  "thenote", "the-note",
  "thewarning", "the-warning",
  "theconvention", "the-convention",
  "theacknowledgments", "the-acknowledgments",
  "theexercise", "the-exercise",
  "theproblem", "the-problem",
  "thefigure", "the-figure",
  "thetable", "the-table",
  "footnotenr", "footnote-nr",
  "equationnr", "equation-nr",
  "theoremnr", "theorem-nr",
  "propositionnr", "proposition-nr",
  "lemmanr", "lemma-nr",
  "corollarynr", "corollary-nr",
  "axiomnr", "axiom-nr",
  "definitionnr", "definition-nr",
  "notationnr", "notation-nr",
  "conjecturenr", "conjecture-nr",
  "remarknr", "remark-nr",
  "examplenr", "example-nr",
  "notenr", "note-nr",
  "warningnr", "warning-nr",
  "conventionnr", "convention-nr",
  "acknowledgmentsnr", "acknowledgments-nr",
  "exercisenr", "exercise-nr",
  "problemnr", "problem-nr",
  "figurenr", "figure-nr",
  "tablenr", "table-nr",
  "theoremname", "theorem-name",
  "figurename", "figure-name",
  "exercisename", "exercise-name",
  "theoremsep", "theorem-sep",
  "figuresep", "figure-sep",
  "exercisesep", "exercise-sep",
  "footnotesep", "footnote-sep",
  "newtheorem", "new-theorem",
  "newremark", "new-remark",
  "newexercise", "new-exercise",
  "newfigure", "new-figure",

  "theprefix", "the-prefix",
  "thechapter", "the-chapter",
  "theappendix", "the-appendix",
  "thesection", "the-section",
  "thesubsection", "the-subsection",
  "thesubsubsection", "the-subsubsection",
  "theparagraph", "the-paragraph",
  "thesubparagraph", "the-subparagraph",
  "chapternr", "chapter-nr",
  "appendixnr", "appendix-nr",
  "sectionnr", "section-nr",
  "subsectionnr", "subsection-nr",
  "subsubsectionnr", "subsubsection-nr",
  "paragraphnr", "paragraph-nr",
  "subparagraphnr", "subparagraph-nr",
  "sectionsep", "section-sep",
  "subsectionsep", "subsection-sep",
  "subsubsectionsep", "subsubsection-sep",

  "theorem*", "render-theorem",
  "remark*", "render-remark",
  "exercise*", "render-exercise",
  "proof*", "render-proof",
  "small-figure*", "render-small-figure",
  "big-figure*", "render-big-figure",

  "theanswer", "the-answer",
  "thealgorithm", "the-algorithm",
  "answernr", "answer-nr",
  "algorithmnr", "algorithm-nr",

  ""
};

static hashmap<string,string> style_rename_table ("?");

static tree
upgrade_style_rename_sub (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, MERGE, 2) && (t[0] == "the"))
    return tree (MERGE, "the-", t[1]);
  else if (is_func (t, MERGE, 2) && (t[1] == "nr"))
    return tree (MERGE, t[0], "-nr");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_style_rename_sub (t[i]);
    return r;
  }
}

static tree
upgrade_style_rename (tree t) {
  t= upgrade_style_rename_sub (t);
  return
    rename_vars (t, cached_renamer (style_rename, style_rename_table), true);
}

/******************************************************************************
* Remove trailing punctuation in item* tags
******************************************************************************/

static tree
upgrade_item_punct (tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_item_punct (t[i]);
    if (is_compound (r, "item*", 1)) {
      tree& item= r[0];
      if (is_atomic (item)) {
        string s= item->label;
        if (ends (s, ".") || ends (s, ":") || ends (s, " "))
          item= s (0, N(s)-1);
      }
      else if (is_concat (item) && is_atomic (item[N(item)-1])) {
        string s= item [N(item)-1] -> label;
        if ((s == ".") || (s == ":") || (s == " ")) {
          if (N(item) == 2) item= item[0];
          else item= item (0, N(item) - 1);
        }
      }
    }
    return r;
  }
}

/******************************************************************************
* Forget default page parameters
******************************************************************************/

tree
upgrade_page_pars (tree t) {
  if (is_atomic (t)) return t;
  else if (L(t) == COLLECTION) {
    int i, n= N(t);
    tree r (COLLECTION);
    for (i=0; i<n; i++) {
      tree u= t[i];
      if (!is_func (u, ASSOCIATE, 2));
      else if (u == tree (ASSOCIATE, PAGE_TYPE, "a4"));
      else if (u == tree (ASSOCIATE, PAGE_EVEN, "30mm"));
      else if (u == tree (ASSOCIATE, PAGE_ODD, "30mm"));
      else if (u == tree (ASSOCIATE, PAGE_RIGHT, "30mm"));
      else if (u == tree (ASSOCIATE, PAGE_TOP, "30mm"));
      else if (u == tree (ASSOCIATE, PAGE_BOT, "30mm"));
      else if (u == tree (ASSOCIATE, PAR_WIDTH, "150mm"));
      else if (u[0] == "page-reduce-left");
      else if (u[0] == "page-reduce-right");
      else if (u[0] == "page-reduce-top");
      else if (u[0] == "page-reduce-bot");
      else if (u[0] == "sfactor");
      else r << u;
    }
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_page_pars (t[i]);
    return r;
  }
}

/******************************************************************************
* Substitutions
******************************************************************************/

tree
substitute (tree t, tree which, tree by) {
  if (t == which) return by;
  else if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= substitute (t[i], which, by);
    return r;
  }
}

/******************************************************************************
* Upgrading title information
******************************************************************************/

static tree doc_keywords;
static tree doc_ams_class;

static void
abstract_add (tree& data, tree what) {
  if (is_func (what, DOCUMENT, 1)) what= what[0];
  if (is_atomic (what)) {
    string s= what->label;
    int i, start, n= N(s);
    for (i=start=0; i<n; )
      if (s[i] == ',') {
        int next= i+1;
        while ((i>start) && (s[i-1]==' ')) i--;
        data << s (start, i);
        i= next; if (s[i] == ' ') i++;
        start= i;
      }
      else i++;
    while ((i>start) && (s[i-1]==' ')) i--;
    data << s (start, i);
  }
  else data << what;
}

static tree
upgrade_abstract (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "abstract", 1) && is_document (t[0])) {
    int i, n= N(t[0]);
    tree r (DOCUMENT);
    for (i=0; i<n; i++)
      if (is_compound (t[0][i], "keywords", 1))
        abstract_add (doc_keywords, t[0][i][0]);
      else if (is_compound (t[0][i], "AMS-class"))
        abstract_add (doc_ams_class, t[0][i][0]);
      else r << t[0][i];
    if (N(r) == 0) r << "";
    return compound ("abstract", r);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_abstract (t[i]);
    return r;
  }
}

static tree
search_title_tag (tree t, string tag, bool flag= false) {
  if (is_atomic (t)) return tuple ();
  else if (is_compound (t, tag, 1)) return tuple (t[0]);
  else if (flag && is_compound (t, tag)) return tuple (t);
  else {
    int i, n= N(t);
    tree r (TUPLE);
    for (i=0; i<n; i++)
      r << A (search_title_tag (t[i], tag, flag));
    return r;
  }
}

static void
search_abstract_tag (tree t, tree& data, string tag) {
  if (is_atomic (t)) return;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_compound (t[i], tag, 1))
        abstract_add (data, t[i][0]);
  }
}

static void
title_add (tree& data, string tag, tree args, bool flag= false) {
  int i, n= N(args);
  for (i=0; i<n; i++) {
    tree t= args[i];
    if (flag && (!is_document (t))) t= tree (DOCUMENT, t);
    if ((!flag) && is_func (t, DOCUMENT, 1)) t= t[0];
    data << compound (tag, t);
  }
}

static void
add_info (tree& data, tree t, string ntag, string otag,
          bool flag1= false, bool flag2= false) {
  tree u= search_title_tag (t, otag, flag1);
  if (N(t) != 0) title_add (data, ntag, u, flag2);
}

static tree
upgrade_title2 (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "make-title", 1)) {
    //cout << "t= " << t << "\n";
    tree u= t[0];
    if (!is_document (u) && !is_concat (u)) u= tree (DOCUMENT, u);
    tree title_data = compound ("doc-data");
    tree author_data= compound ("doc-author-data");
    tree meta_data  = compound ("doc-data");

    for (int i=0; i<N(u); i++) {
      tree v= u[i];
      add_info (title_data, v, "doc-title", "title");
      add_info (meta_data, v, "doc-date", "title-date");
      add_info (meta_data, v, "doc-running-title", "header-title");
      add_info (meta_data, v, "doc-running-author", "header-author");
      add_info (meta_data, v, "doc-note", "title-thanks", true, true);
      if (N (search_title_tag (v, "author")) != 0) {
        if (N (author_data) != 0 &&
            is_compound (author_data[0], "author-name"))
          title_data << author_data;
        author_data= compound ("doc-author-data");
      }
      add_info (author_data, v, "author-name", "author");
      add_info (author_data, v, "author-address", "address", false, true);
      add_info (author_data, v, "author-email", "title-email");
      add_info (author_data, v, "author-homepage", "title-web");
    }
    if (N (author_data) != 0) title_data << author_data;
    if (N (meta_data) != 0) title_data << A (meta_data);

    tree notice= search_title_tag (t, "made-by-TeXmacs", true);
    if (N (notice) != 0)
      title_data << compound ("doc-note", compound ("with-TeXmacs-text"));    
    search_abstract_tag (t[0], doc_keywords, "title-keywords");
    search_abstract_tag (t[0], doc_ams_class, "title-ams-class");
    if (N (doc_keywords) != 0) title_data << doc_keywords;
    if (N (doc_ams_class) != 0) title_data << doc_ams_class;
    //cout << "r= " << title_data << "\n";
    return title_data;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_title2 (t[i]);
    return r;
  }
}

static tree
upgrade_doc_info (tree t) {
  doc_keywords = compound ("doc-keywords");
  doc_ams_class= compound ("doc-AMS-class");
  t= upgrade_abstract (t);
  t= upgrade_title2 (t);
  return t;
}

/******************************************************************************
* Upgrade bibliographies
******************************************************************************/

tree
upgrade_bibliography (tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_bibliography (t[i]);
    if (is_compound (t, "bibliography") || is_compound (t, "bibliography*")) {
      int l= N(r)-1;
      if (is_func (r[l], DOCUMENT, 1) && is_compound (r[l][0], "bib-list"));
      else if (is_compound (r[l], "bib-list"));
      else r[l]= tree (DOCUMENT, compound ("bib-list", "[99]", r[l]));
    }
    return r;
  }
}

/******************************************************************************
* Upgrade switches
******************************************************************************/

tree
upgrade_switch (tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_switch (t[i]);
    if (is_compound (r, "switch", 2)) {
      int i, n= N(r[1]);
      tree u (make_tree_label ("switch"), n);
      for (i=0; i<n; i++)
        if (is_compound (r[1][i], "tmarker", 0)) u[i]= r[0];
        else u[i]= compound ("hidden", r[1][i]);
      return u;
    }
    if (is_compound (r, "fold", 2))
      return compound ("folded", r[0], r[1]);
    if (is_compound (r, "unfold", 2))
      return compound ("unfolded", r[0], r[1]);
    if (is_compound (r, "fold-bpr", 2) ||
        is_compound (r, "fold-text", 2) ||
        is_compound (r, "fold-proof", 2) ||
        is_compound (r, "fold-exercise", 2))
      return compound ("summarized", r[0], r[1]);
    if (is_compound (r, "unfold-bpr", 2) ||
        is_compound (r, "unfold-text", 2) ||
        is_compound (r, "unfold-proof", 2) ||
        is_compound (r, "unfold-exercise", 2))
      return compound ("detailed", r[0], r[1]);
    if (is_compound (r, "fold-algorithm", 2))
      return compound ("summarized-algorithm", r[0], r[1]);
    if (is_compound (r, "unfold-algorithm", 2))
      return compound ("detailed-algorithm", r[0], r[1]);
    if (is_func (r, ASSIGN, 2) && r[0] == "fold-algorithm")
      return tree (ASSIGN, "summarized-algorithm", r[1]);
    if (is_func (r, ASSIGN, 2) && r[0] == "unfold-algorithm")
      return tree (ASSIGN, "detailed-algorithm", r[1]);
    return r;
  }
}

/******************************************************************************
* Upgrade graphics
******************************************************************************/

static int
find_attr_pos (tree t, string name) {
  int i, n= N(t);
  for (i=0; i<n; i+=2)
    if (t[i] == name && i%2 == 0 && i+1<n) return i;
  return -1;
}

static bool
find_attr (tree t, string name) {
  return find_attr_pos (t, name) != -1;
}

static tree
get_attr (tree t, string name, tree ifnotfound) {
  int i= find_attr_pos (t, name);
  return i == -1 ? ifnotfound : t[i+1];
}

static tree
add_attr (tree t, string name, tree value) {
  int i, n= N(t);
  tree r (t, n+2);
  for (i=0; i<n-1; i++)
    r[i]= t[i];
  r[n-1]= name;
  r[n]= value;
  r[n+1]= t[n-1];
  return r;
}

static tree
set_attr (tree t, string name, tree value) {
  int i= find_attr_pos (t, name);
  if (i != -1)
    t[i+1]= value;
  else
    t= add_attr (t, name, value);
  return t;
}

static tree
remove_attr (tree t, string name) {
  int i= find_attr_pos (t, name);
  if (i == -1)
    return t;
  else {
    int j, n= N(t);
    tree r (t, n-2);
    for (j=0; j<i; j++)
      r[j]= t[j];
    for (j=i+2; j<n; j++)
      r[j-2]= t[j];
    return r;
  }
}

tree
upgrade_fill (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_compound (t, "with")) {
    if ((i= find_attr_pos (t, "gr-mode")) != -1
     && is_tuple (t[i+1],"edit-prop"))
      t[i+1]= tuple ("group-edit","props");

    tree fm= get_attr (t, "fill-mode", tree ("none"));
    t= remove_attr (t, "fill-mode");
    t= remove_attr (t, "gr-fill-mode");
    if (fm == "none")
      t= remove_attr (t, "fill-color");
    if (fm == "inside")
      t= set_attr (t, "color", tree ("none"));
  }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_fill (t[i]);
  return r;
}

static void
length_split (string &num, string &unit, string l) {
  int i, n= N(l), nu= 0;
  i= n-1;
  while (i>=0 && is_alpha (l[i])) i--, nu++;
  num= l (0, n-nu);
  unit= l (n-nu, n);
}

static string
length_minus (string l) {
  if (l[0] == '+') l[0]= '-';
  else
  if (l[0] == '-') l[0]= '+';
  else
    l= "-" * l;
  return l;
}

static string
length_abs (string l) {
  if (l[0] == '-') l[0]= '+';
  return l;
}

static int length_add_error;

static string
length_add (string l1, string l2) {
  length_add_error= 0;
  string n1, u1, n2, u2;
  length_split (n1, u1, l1);
  length_split (n2, u2, l2);
  double i1= as_double (n1);
  double i2= as_double (n2);
  if (i1 == 0.0) {
    i1= i2;
    i2= 0.0;
    string u= u1;
    u1= u2;
    u2= u;
  }
  if (u1 == "par" && (u2 == "par" || i2 == 0.0))
    return as_string (i1 + i2) * "par";
  else
  if ((u1 == "cm" || u1 == "mm")
   && (u2 == "cm" || u2 == "mm" || i2 == 0.0)) {
    if (u1 == "cm" && u2 == "mm") i2 /= 10;
    if (u1 == "mm" && u2 == "cm") i2 *= 10;
    return as_string (i1 + i2) * u1;
  }
  else {
    length_add_error= 1;
    return "0cm";
  }
}

tree
upgrade_graphics (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_compound (t, "with") &&
      (find_attr (t, "gr-frame") || find_attr (t, "gr-clip"))) {
    tree fr= get_attr (t, "gr-frame",
                          tuple ("scale", "1cm",
                                 tree (TUPLE, "0.5par", "0cm")));
    tree clip= get_attr (t, "gr-clip",
                            tuple ("clip",
                                   tuple ("0par", "-0.3par"),
                                   tuple ("1par", "0.3par")));
    t= remove_attr (t, "gr-clip");

    string ox= as_string (fr[2][0]), oy= as_string (fr[2][1]);
    string cg= as_string (clip[1][0]), cb= as_string (clip[1][1]);
    string cd= as_string (clip[2][0]), ch= as_string (clip[2][1]);
    string w= length_add (cd, length_minus (cg));
    string h= length_add (ch, length_minus (cb));

    fr[2][0]= tree (length_add (length_abs (cg) , ox));
    if (length_add_error)
      fr[2][0]= tree (PLUS, length_abs (cg) , ox);
    fr[2][1]= tree (length_add (length_abs (cb) , oy));
    if (length_add_error)
      fr[2][1]= tree (PLUS, length_abs (cb) , oy);
    tree geom= tuple ("geometry", tree (w), tree (h));
    t= add_attr (t, "gr-geometry", geom);
    t= set_attr (t, "gr-frame", fr);
  }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_graphics (t[i]);
  return r;
}

tree
upgrade_textat (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_compound (t, "text-at") && N(t) == 4) {
    tree t0= t;
    t= tree (WITH, tree (TEXT_AT, t[0], t[1]));
    t= set_attr (t, "text-at-halign", t0[2]);
    t= set_attr (t, "text-at-valign", t0[3]);
  }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_textat (t[i]);
  return r;
}

/******************************************************************************
* Upgrade cell alignment
******************************************************************************/

tree
upgrade_cell_alignment (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_func (t, CWITH) && (N(t) >= 2))
    if (t[N(t)-2] == CELL_HALIGN)
      if (t[N(t)-1] == "." || t[N(t)-1] == ",") {
        tree r= copy (t);
        r[N(t)-1]= "L" * t[N(t)-1]->label;
        return r;
      }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_cell_alignment (t[i]);
  return r;
}

/******************************************************************************
* Renaming primitives
******************************************************************************/

tree
rename_primitive (tree t, string which, string by) {
  int i;
  if (is_atomic (t)) return t;
  int n= N(t);
  tree r (t, n);
  if (is_compound (t, which))
    r= tree (make_tree_label (by), n);
  for (i=0; i<n; i++)
    r[i]= rename_primitive (t[i], which, by);
  return r;
}

/******************************************************************************
* Upgrade label assignment
******************************************************************************/

tree
upgrade_label_assignment (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_func (t, ASSIGN, 2) && t[0] == "the-label")
    return tree (SET_BINDING, t[1]);
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_label_assignment (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade scheme documentation
******************************************************************************/

tree
upgrade_scheme_doc (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "scm-fun", 1) ||
           is_compound (t, "scm-macro", 1))
    return compound ("scm", t[0]);
  else if (is_compound (t, "explain-scm-fun") ||
           is_compound (t, "explain-scm-macro"))
    {
      tree r (CONCAT);
      r << "(" << t[0];
      for (int i=1; i<N(t); i++)
        r << " " << t[i];
      r << ")";
      return compound ("scm", simplify_concat (r));
    }
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_scheme_doc (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade Mathemagix tag
******************************************************************************/

tree
upgrade_mmx (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "mmx", 0) || t == tree (VALUE, "mmx"))
    return compound ("mathemagix");
  else if (is_compound (t, "mml", 0) || t == tree (VALUE, "mml"))
    return compound ("mmxlib");
  else if (is_compound (t, "scheme", 0) || t == tree (VALUE, "scheme"))
    return compound ("scheme");
  else if (is_compound (t, "cpp", 0) || t == tree (VALUE, "cpp"))
    return compound ("c++");
  else if (is_compound (t, "scheme-code", 1))
    return compound ("scm", upgrade_mmx (t[0]));
  else if (is_compound (t, "scheme-fragment", 1))
    return compound ("scm-fragment", upgrade_mmx (t[0]));
  else if (is_compound (t, "cpp-code", 1))
    return compound ("cpp", upgrade_mmx (t[0]));
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_mmx (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade sessions
******************************************************************************/

static tree
upgrade_session (tree t, tree lan, tree ses) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "session", 1))
    return compound ("session", copy (lan), copy (ses),
                     upgrade_session (t[0], lan, ses));
  else if (is_func (t, WITH, 5) &&
           t[0] == PROG_LANGUAGE &&
           t[2] == PROG_SESSION)
    return upgrade_session (t[4], t[1], t[3]);
  else {
    int i, n= N(t);
    tree r (L(t));
    for (i=0; i<n; i++) {
      if (is_document (t) && is_compound (t[i], "input", 2)) {
        bool m = is_compound (t[i][1], "math", 1);
        tree in= (m? t[i][1][0]: t[i][1]);
        if ((i+1)<n && is_compound (t[i+1], "output", 1)) {
          const char* op= (m? "unfolded-io-math": "unfolded-io");
          r << compound (op, t[i][0], in, t[i+1][0]);
          i++;
        }
        else {
          const char* op= (m? "input-math": "input");
          r << compound (op, t[i][0], in);
        }
      }
      else r << upgrade_session (t[i], lan, ses);
    }
    return r;
  }
}

/******************************************************************************
* Upgrade presentation style
******************************************************************************/

tree
upgrade_presentation (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "style") || is_compound (t, "tuple")) {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      if (t[i] == "presentation") r[i]= "presentation-ridged-paper";
      else r[i]= upgrade_presentation (t[i]);
    return r;
  }
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_presentation (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade mathematical formulas
******************************************************************************/

static hashset<string> existing_styles;

static void
declare_style (url u) {
  if (is_or (u)) {
    declare_style (u[1]);
    declare_style (u[2]);
  }
  else if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    if (dir == "CVS" || dir == ".svn");
    else declare_style (u[2]);
  }
  else if (is_atomic (u)) {
    string s= as_string (u);
    if (ends (s, ".ts") && !starts (s, "source")) {
      existing_styles->insert (s(0,N(s)-3));
      if (starts (s, "old-"))
        existing_styles->insert (s(4,N(s)-3));
      if (starts (s, "old2-"))
        existing_styles->insert (s(5,N(s)-3));
    }
  }
}

bool
is_non_style_document (tree doc) {
  tree style= extract (doc, "style");
  if (!is_tuple (style) || N(style) == 0 || !is_atomic (style[0]))
    return false;
  for (int i=0; i<N(style); i++)
    if (style[i] == "source") return false;
  string ms= style[0]->label;
  if (N(existing_styles) == 0) {
    url sty_u= descendance ("$TEXMACS_STYLE_ROOT");
    declare_style (sty_u);
    //cout << "Styles: " << existing_styles << LF;
  }
  return existing_styles->contains (ms);
}

tree
upgrade_math (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_func (t, WITH, 3) && t[0] == MODE && t[1] == "math")
    return compound ("math", upgrade_math (t[2]));
  else if (is_func (t, WITH, 3) && t[0] == MODE && t[1] == "text")
    return compound ("text", upgrade_math (t[2]));
  else if (is_func (t, WITH) && N(t) >= 5 && t[0] == MODE && t[1] == "math")
    return compound ("math", upgrade_math (t (2, N(t))));
  else if (is_func (t, WITH) && N(t) >= 5 && t[0] == MODE && t[1] == "text")
    return compound ("text", upgrade_math (t (2, N(t))));
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_math (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade resize and clipped
******************************************************************************/

tree
upgrade_resize_arg (tree t) {
  if (is_func (t, MERGE, 2)) {
    if (is_atomic (t[0]) && is_atomic (t[1]))
      return upgrade_resize_arg (t[0]->label * t[1]->label);
    tree u= upgrade_resize_arg (t[0]);
    if (is_func (u, PLUS, 2) || is_func (u, MINUS, 2) ||
        is_func (u, MINIMUM, 2) || is_func (u, MAXIMUM, 2))
      if (u[1] == "") {
        u[1]= upgrade_resize_arg (t[1]);
        return u;
      }
    cout << "TeXmacs] warning, resize argument " << t << " not upgraded\n";
    return t;
  }
  if (is_func (t, ARG, 1) ||
      is_func (t, PLUS, 2) || is_func (t, MINUS, 2) ||
      is_func (t, MINIMUM, 2) || is_func (t, MAXIMUM, 2))
    return t;
  if (!is_atomic (t)) {
    cout << "TeXmacs] warning, resize argument " << t << " not upgraded\n";
    return t;
  }
  string s= t->label;
  if (starts (s, "c")) {
    cout << "TeXmacs] warning, resize argument " << t << " not upgraded\n";
    return t;
  }
  if (s == "l") return "1l";
  if (s == "r") return "1r";
  if (s == "t") return "1t";
  if (s == "b") return "1b";
  if (N(s) < 2) return t;
  if (s[0] != 'l' && s[0] != 'b' && s[0] != 'r' && s[0] != 't') return t;
  string s1= "1" * s (0, 1);
  string s2= s (2, N(s));
  if (s[1] == '+') return tree (PLUS, s1, s2);
  if (s[1] == '-') return tree (MINUS, s1, s2);
  if (s[1] == '[') return tree (MINIMUM, s1, s2);
  if (s[1] == ']') return tree (MAXIMUM, s1, s2);
  return t;
}

tree
upgrade_resize_clipped (tree t) {
  if (is_atomic (t)) return t;
  else if (N(t) >= 5 && (is_func (t, RESIZE) || is_func (t, CLIPPED))) {
    if (is_func (t, CLIPPED))
      t= tree (CLIPPED, t[4], t[0], t[1], t[2], t[3]);
    int i, n= 5;
    tree r (t, n);
    r[0]= upgrade_resize_clipped (t[0]);
    for (i=1; i<n; i++)
      r[i]= upgrade_resize_arg (t[i]);
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_resize_clipped (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade images
******************************************************************************/

tree
upgrade_image_length (tree t, string unit) {
  if (!is_atomic (t)) return t;
  string s= t->label;
  if (starts (s, "*") || starts (s, "/")) {
    double mag= get_magnification (s);
    return as_string (mag) * unit;
  }
  else return t;
}

tree
upgrade_image (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, IMAGE, 7))
    return tree (IMAGE, t[0],
                 upgrade_image_length (t[1], "w"),
                 upgrade_image_length (t[2], "h"),
                 "", "");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_image (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade root switches
******************************************************************************/

tree
upgrade_root_switch (tree t, bool top= true) {
  if (is_func (t, DOCUMENT) &&
      (top || N(t) == 1 ||
       (N(t) == 2 && is_compound (t[0], "hide-preamble")))) {
    int i, n= N(t);
    tree r (DOCUMENT, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_root_switch (t[i], false);
    return r;
  }
  else if (is_compound (t, "body", 1))
    return compound ("body", upgrade_root_switch (t[0], false));
  else if (is_compound (t, "switch"))
    return compound ("screens", A(t));
  else return t;
}

/******************************************************************************
* Upgrade hyphenation
******************************************************************************/

tree
upgrade_hyphenation (tree t) {
  tree style= copy (extract (t, "style"));
  tree init = extract (t, "initial");
  if (is_atomic (style)) style= tuple (style);
  if (style == tree (TUPLE)) style= tuple ("generic");
  if (!is_tuple (style) || N(style) != 1 || !is_atomic (style[0])) return t;
  string ms= style[0]->label;
  if (ms == "article" || ms == "beamer" || ms == "book" || ms == "exam" ||
      ms == "generic" || ms == "letter" || ms == "seminar")
    {
      hashmap<string,tree> h (UNINIT, init);
      if (!h->contains (PAR_HYPHEN)) h (PAR_HYPHEN)= "normal";
      tree new_init= make_collection (h);
      return change_doc_attr (t, "initial", new_init);
    }
  else return t;
}

/******************************************************************************
* Renaming of symbols
******************************************************************************/

tree
rename_symbols (tree t, hashmap<string,string> h) {
  if (is_atomic (t)) {
    bool same= true;
    int pos;
    string s= t->label;
    for (pos=0; pos<N(s); ) {
      int j=pos;
      tm_char_forwards (s, pos);
      if (j+2 < pos && h->contains (s (j, pos))) {
        same= false;
        break;
      }
    }
    if (same) return t;
    string r;
    for (pos=0; pos<N(s); ) {
      int j=pos;
      tm_char_forwards (s, pos);
      if (h->contains (s (j, pos))) r << h [s (j, pos)];
      else r << s (j, pos);
    }
    return r;
  }
  else if (is_func (t, RAW_DATA)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= rename_symbols (t[i], h);
    return r;
  }
}

/******************************************************************************
* Rewrite bodies of algorithms
******************************************************************************/

tree
upgrade_algorithm (tree t, bool flag= true) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "algo", 1))
    return compound ("tt", upgrade_algorithm (t[0]));
  else if (is_compound (t, "algorithm", 2))
    return compound ("named-algorithm-old",
                     upgrade_algorithm (t[0]), upgrade_algorithm (t[1]));
  else if (flag && is_compound (t, "body", 1))
    return compound ("algorithm-body", upgrade_algorithm (t[0]));
  else if (is_compound (t, "pile", 1))
    return compound ("tabbed", upgrade_algorithm (t[0]));
  else if (is_compound (t, "scm-fragment", 1))
    return compound ("scm-code", upgrade_algorithm (t[0]));
  else if (is_compound (t, "scheme-fragment", 1))
    return compound ("scm-code", upgrade_algorithm (t[0]));
  else if (is_compound (t, "mmx-fragment", 1))
    return compound ("mmx-code", upgrade_algorithm (t[0]));
  else if (is_compound (t, "cpp-fragment", 1))
    return compound ("cpp-code", upgrade_algorithm (t[0]));
  else if (is_compound (t, "shell-fragment", 1))
    return compound ("shell-code", upgrade_algorithm (t[0]));
  else {
    int i, n= N(t);
    tree r (t, n);
    flag= true;
    if (is_document (t))
      for (i=0; i<n; i++)
        if (is_compound (t[i], "TeXmacs", 1) ||
            is_compound (t[i], "style") ||
            is_compound (t[i], "initial") ||
            is_compound (t[i], "references"))
          flag= false;
    for (i=0; i<n; i++)
      r[i]= upgrade_algorithm (t[i], flag);
    return r;
  }
}

/******************************************************************************
* Upgrade mathematical operators
******************************************************************************/

tree
upgrade_math_ops (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_math_ops (t[i]);
  if (is_func (t, WITH, 3) &&
      is_atomic (t[2]) &&
      is_alpha (t[2]->label) &&
      t[0] == "math-font-family")
    {
      if (t[1] == "trm") return compound ("math-up", t[2]);
      if (t[1] == "tss") return compound ("math-ss", t[2]);
      if (t[1] == "ttt") return compound ("math-tt", t[2]);
      if (t[1] == "rm") return compound ("math-up", t[2]);
      if (t[1] == "up") return compound ("math-up", t[2]);
      if (t[1] == "bf") return compound ("math-bf", t[2]);
      if (t[1] == "sl") return compound ("math-sl", t[2]);
      if (t[1] == "it") return compound ("math-it", t[2]);
      if (t[1] == "ss") return compound ("math-ss", t[2]);
      if (t[1] == "tt") return compound ("math-tt", t[2]);
    }
  if (n == 1 && starts (as_string (L(t)), "math-")) {
    if (is_compound (t, "math-ord")) return compound ("math-ordinary", t[0]);
    if (is_compound (t, "math-punct")) return compound ("math-separator", t[0]);
    if (is_compound (t, "math-bin")) return compound ("math-plus", t[0]);
    if (is_compound (t, "math-rel")) return compound ("math-relation", t[0]);
    if (is_compound (t, "math-op")) return compound ("math-big", t[0]);
  }
  return r;
}

/******************************************************************************
* Cleaning spurious spaces in the document
******************************************************************************/

static bool
only_spaces (string s) {
  for (int i=0; i<N(s); i++)
    if (s[i] != ' ') return false;
  return true;
}

static bool
eat_spaces (tree t, bool after) {
  (void) after;
  if (is_atomic (t)) return false;
  return
    is_compound (t, "hide-preamble") ||
    is_compound (t, "doc-data") ||
    is_compound (t, "abstract") ||
    is_compound (t, "bibliography") ||
    is_compound (t, "bibitem") ||
    is_compound (t, "bibitem*");
}

static tree
clean_spaces (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= clean_spaces (t[i]);
  if (!is_func (r, CONCAT)) return r;
  t= r;
  r= tree (CONCAT);
  for (i=0; i<n; i++)
    if (!is_atomic (t[i])) r << t[i];
    else {
      string s= t[i]->label;
      if (i>0 && eat_spaces (t[i-1], true))
        while (starts (s, " ")) s= s (1, N(s));
      if (i<N(t)-1 && eat_spaces (t[i+1], false))
        while (ends (s, " ")) s= s (0, N(s)-1);
      if (s != "") r << tree (s);
    }
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

/******************************************************************************
* Cleaning the document header
******************************************************************************/

static tree
search_header_tag (tree t, string which, tree& h) {
  if (is_compound (t, which)) {
    h << t;
    return "";
  }
  if (is_func (t, DOCUMENT)) {
    tree r (DOCUMENT);
    for (int i=0; i<N(t); i++) {
      if (is_compound (t[i], which)) h << t[i];
      else if (is_func (t[i], SURROUND, 3)) {
        tree x= search_header_tag (t[i], which, h);
        if (x != "") r << x;
      }
      else r << t[i];
    }
    return r;
  }
  if (is_func (t, SURROUND, 3)) {
    tree r (SURROUND, 3);
    r[0]= search_header_tag (t[0], which, h);
    r[2]= search_header_tag (t[2], which, h);
    r[1]= search_header_tag (t[1], which, h);
    if (r[0] == "" && r[1] == "") r= r[2];
    else if (r[0] == "" && r[2] == "") r= r[1];
    else if (r[1] == "" && r[2] == "") r= r[0];
    return r;
  }
  return t;
}

tree
clean_header (tree t) {
  if (!is_func (t, DOCUMENT)) return t;
  tree r        (DOCUMENT);
  tree preamble (DOCUMENT);
  tree title    (DOCUMENT);
  tree abstract (DOCUMENT);
  t= search_header_tag (t, "hide-preamble", preamble);
  t= search_header_tag (t, "doc-data", title);
  t= search_header_tag (t, "abstract", abstract);
  while (N(t) > 0 && is_atomic (t[0]) && only_spaces (t[0]->label))
    t= t (1, N(t));
  r << A (preamble);
  r << A (title);
  r << A (abstract);
  r << A (t);
  return r;
}

/******************************************************************************
* Upgrade graphical attributes
******************************************************************************/

static void
replace_dash_style (tree& t, string var) {
  if (find_attr (t, var)) {
    tree val= get_attr (t, var, "default");
    if (is_func (val, TUPLE)) {
      string nval;
      for (int i=0; i<N(val); i++)
        if (val[i] == "0") nval << "0";
        else nval << "1";
      t= set_attr (t, var, tree (nval));
    }
  }
}

static tree
encode_arrow (tree t) {
  if (is_func (t, WITH, 3) && t[0] == "dash-style") {
    if (t[2] == tree (LINE,
                      tuple ("10ln", "6ln"),
                      tuple ("0ln", "0ln"),
                      tuple ("10ln", "-6ln")))
      return "<less>";
    if (t[2] == tree (LINE,
                      tuple ("-10ln", "6ln"),
                      tuple ("0ln", "0ln"),
                      tuple ("-10ln", "-6ln")))
      return "<gtr>";
  }
  return t;
}

static void
replace_line_arrows (tree& t, string var, string begin, string end) {
  if (find_attr (t, var)) {
    tree val= get_attr (t, var, "default");
    t= set_attr (t, "arrow-length", "10ln");
    t= set_attr (t, "arrow-height", "6ln");
    if (is_func (val, TUPLE, 1))
      t= set_attr (t, end, encode_arrow (val[0]));
    if (is_func (val, TUPLE, 2)) {
      t= set_attr (t, begin, encode_arrow (val[0]));
      t= set_attr (t, end, encode_arrow (val[1]));
    }
    t= remove_attr (t, var);
  }
}

static void
replace_magnification (tree& t, string var, string repl) {
  tree body= t[N(t)-1];
  if (find_attr (t, var))
    if (is_func (body, GRAPHICS) ||
        is_func (body, GR_GROUP) ||
        is_func (body, TEXT_AT) ||
        is_func (body, _POINT) ||
        is_func (body, LINE) ||
        is_func (body, CLINE) ||
        is_func (body, SPLINE) ||
        is_func (body, CSPLINE) ||
        is_func (body, ARC) ||
        is_func (body, CARC) ||
        is_func (body, VAR_SPLINE))
      {
        tree val= get_attr (t, var, "1");
        if (is_func (val, TIMES, 2) &&
            is_func (val[1], VALUE, 1) &&
            val[1][0] == var)
          val= val[0];
        t= set_attr (t, repl, val);
        t= remove_attr (t, var);
      }
}

static tree
upgrade_gr_attributes (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_func (t, WITH)) {
    replace_dash_style (t, "dash-style");
    replace_dash_style (t, "gr-dash-style");
    replace_line_arrows (t, "line-arrows", "arrow-begin", "arrow-end");
    replace_line_arrows (t, "gr-line-arrows",
                            "gr-arrow-begin", "gr-arrow-end");
    replace_magnification (t, "magnification", "magnify");
  }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_gr_attributes (t[i]);
  return r;
}

/******************************************************************************
* Upgrade cursor tag
******************************************************************************/

static tree
upgrade_cursor (tree t) {
  int i;
  if (is_atomic (t)) return t;
  if (is_func (t, VALUE)) {
    if (t == tree (VALUE, "cursor")) return compound ("cursor");
    if (t == tree (VALUE, "math-cursor")) return compound ("math-cursor");
  }
  int n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= upgrade_cursor (t[i]);
  return r;
}

/******************************************************************************
* Upgrade encoding to unicode whenever cyrillic font is used
******************************************************************************/

bool
become_cyrillic (string s1, string s2) {
  return (s1 == "font" && s2 == "cyrillic");}

bool
become_other (string s1, string s2) {
  return (s1 == "font" && s2 != "cyrillic");
}

tree
upgrade_cyrillic_encoding (tree t, bool cyrillic) {
  if (is_atomic (t)) {
    if (cyrillic) return cyrillic_subset_in_t2a_to_code_point (as_string(t));
    else          return t;
  }
  else {
    int i = 0;
    if (is_func(t, WITH)) {
      for (i = 0 ; i < N(t) - 1 ; i+=2) {
        if (!cyrillic
            && become_cyrillic (as_string (t[i]), as_string (t[i+1])))
          cyrillic = true;
        else if (cyrillic
            && become_other (as_string (t[i]), as_string (t[i+1])))
          cyrillic = false;
      }
      t[N(t)-1] = upgrade_cyrillic_encoding (t[N(t)-1], cyrillic);
    }
    else {
      for (i = 0 ; i < N(t) ; i++) 
        t[i] = upgrade_cyrillic_encoding (t[i], cyrillic);
    }
    return t;
  }
}

array<tree>
get_childs_by_name (tree t, string s) {
  array<tree> r = array<tree>();
  if (!is_atomic (t))
    for (int i = 0 ; i < N(t) ; i++) {
      if (as_string (L(t[i])) == s) r << t[i];
    }
  return r;
}

static tree
upgrade_cyrillic (tree t) {
  bool cyrillic = false;
  array<tree> initial, collection, associate;
  initial = get_childs_by_name (t, "initial");
  for (int i = 0 ; i < N(initial) ; i++){
    collection = get_childs_by_name (initial[i], "collection");
    for (int j = 0 ; j < N(collection) ; j++){
      associate = get_childs_by_name (collection[j], "associate");
      for (int k = 0 ; k < N(associate) ; k++) {
        if (is_func(associate[k], ASSOCIATE, 2) && associate[k][0] == "font")
          cyrillic = (associate[k][1] == "cyrillic");
      }
    }
  }
  return upgrade_cyrillic_encoding(t, cyrillic);
}

/******************************************************************************
* Upgrade metadata storage format
******************************************************************************/

static tree
correct_metadata (tree t) {
  if (is_atomic (t)) return t;
  tree r;

  if (is_compound (t, "author-misc"))
    r= compound ("author-note");
  else
    r= compound (as_string (L(t)));

  for (int i=0; i<N(t); i++)
    r << correct_metadata (t[i]);
  return r;
}

static tree
upgrade_metadata (tree t) {
  if (is_atomic (t)) return t;
  tree r;
  string l;
  if (is_compound (t, "doc-abstract"))   l= "abstract";
  if (is_compound (t, "author-address")) l= "author-affiliation";
  if (is_compound (t, "doc-AMS-class"))  l= "abstract-msc";
  if (is_compound (t, "doc-msc"))        l= "abstract-msc";
  if (is_compound (t, "doc-keywords"))   l= "abstract-keywords";

  if (is_compound (t, "doc-author-data")) {
    tree r= compound ("author-data");
    for (int i=0; i<N(t); i++)
      r << upgrade_metadata (t[i]);
    return compound ("doc-author", r);
  }
  
  if (l == "") r= compound (as_string (L(t)));
  else         r= compound (l);
  for (int i=0; i<N(t); i++)
    r << upgrade_metadata (t[i]);
  return r;
}

static tree
remove_abstract_data (tree t, array<tree> &abstract_data) {
  // Remove all abstract-msc and abstract-keywords found in a doc-data
  // structure, and store them in abstract_data.
  tree r(L(t));
  if (is_compound (t, "doc-data")) {
    for (int i=0; i<N(t); i++) {
      if (is_compound (t[i], "abstract-msc") ||
          is_compound (t[i], "abstract-keywords"))
        abstract_data << t[i];
      else r << t[i];
    }
  }
  else if (is_atomic (t))
    return t;
  else {
    r= tree (t, N(t));
    for (int i=0; i<N(t); i++)
      r[i]= remove_abstract_data (t[i], abstract_data);
  }
  return r;
}

static array<tree>
sort_abstract_data (array<tree> abstract_data) {
  // Sort and merge abstract-msc and abstract-keywords compounds
  bool has_doc_msc= false, has_doc_keywords= false;
  tree doc_msc (make_tree_label ("abstract-msc"));
  tree doc_keywords (make_tree_label ("abstract-keywords"));
  array<tree> r;

  for (int i=0; i<N(abstract_data); i++) {
    if (is_compound (abstract_data[i], "abstract-msc")) {
      has_doc_msc= true;
      for (int j=0; j<N(abstract_data[i]); j++)
        doc_msc << abstract_data[i][j];
    }
    if (is_compound (abstract_data[i], "abstract-keywords")) {
      has_doc_keywords= true;
      for (int j=0; j<N(abstract_data[i]); j++)
        doc_keywords << abstract_data[i][j];
    }
  }
  if (has_doc_msc) r << doc_msc;
  if (has_doc_keywords) r << doc_keywords;

  return r;
}

static tree
merge_abstract_data (tree t, array<tree> abstract_data, bool &stop) {
  // Merge abstract_data with the first abstract found.
  tree r;
  if (stop || is_atomic (t)) return t;
  else if (is_compound (t, "abstract", 1)) {
    stop= true;
    r= tree (make_tree_label ("abstract-data"));
    r << t;
    for (int i=0; i<N(abstract_data); i++)
      r << abstract_data[i];
  }
  else {
    r= tree (t, N(t));
    for (int i=0; i<N(t); i++)
      r[i]= merge_abstract_data (t[i], abstract_data, stop);
  }
  return r;
}

static tree
add_abstract_data (tree t, array<tree> abstract_data, bool &stop) {
  // Add abstract_data after the first doc-data found.
  if (stop || is_atomic (t)) return t;
  tree r= tree (L(t));
  for (int i=0; i<N(t); i++) {
    if (is_compound (t[i], "doc-data")) {
      stop= true;
      r << t[i];
      r << tree (make_tree_label ("abstract-data"));
      for (int j=0; j<N(abstract_data); j++)
        r[N(r)-1] << abstract_data[j];
    }
    else
      r << add_abstract_data (t[i], abstract_data, stop);
  }
  return r;
}

static tree
upgrade_abstract_data (tree t) {
  // Remove all abstract-msc and abstract-keywords found in a doc-data
  // structure, and merge them with the first abstract found.
  array<tree> abstract_data;
  tree r= remove_abstract_data (t, abstract_data);
  bool stop= false;
  if (N(abstract_data) > 0) {
    abstract_data= sort_abstract_data (abstract_data);
    r= merge_abstract_data (r, abstract_data, stop);
    if (stop) return r;
    else return add_abstract_data (r, abstract_data, stop);
  }
  return t;
}

/******************************************************************************
* Upgrade unroll
******************************************************************************/

tree
upgrade_unroll (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) {
    r[i]= upgrade_unroll (t[i]);
    if (is_compound (t, "unroll") && is_func (r[i], HIDDEN, 1))
      r[i]= compound ("hidden*", r[i][0]);
  }
  return r;
}

/******************************************************************************
* Upgrade style files
******************************************************************************/

tree
upgrade_style (tree t, bool flag) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "style") || is_func (t, TUPLE)) {
    int n= N(t);
    tree r (t, 0);
    bool doc= false;
    for (i=0; i<n; i++)
      if (!flag) r << upgrade_style (t[i], is_compound (t, "style"));
      else if (t[i] == "beamer") r << "old-beamer";
      else if (t[i] == "generic") r << "old-generic";
      else if (t[i] == "help") r << "old-help";
      else if (t[i] == "letter") r << "old-letter";
      else if (t[i] == "seminar") r << "old-seminar";
      else if (t[i] == "tmdoc-keyboard") { if (!doc) r << "doc"; doc= true; }
      else if (t[i] == "tmdoc-markup") { if (!doc) r << "doc"; doc= true; }
      else if (t[i] == "tmdoc-traversal") { if (!doc) r << "doc"; doc= true; }
      else r << upgrade_style (t[i], flag);
    return r;
  }
  else {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_style (t[i], flag);
    return r;
  }
}

/******************************************************************************
* Upgrade document language
******************************************************************************/

tree
upgrade_doc_language (tree t) {
  tree style= extract (t, "style");
  tree init = extract (t, "initial");
  if (init == "" || N(init) == 0) return t;

  string lan= "";
  for (int i=0; i<N(init); i++)
    if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == LANGUAGE)
      lan= as_string (init[i][1]);

  tree new_style= copy (style);
  tree new_init = tree (COLLECTION);
  for (int i=0; i<N(init); i++)
    if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == LANGUAGE)
      new_style << init[i][1];
    else if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == FONT) {
      if (init[i][1] == "cyrillic" &&
          (lan == "bulgarian" || lan == "russian" || lan == "ukrainian"));
      else if ((init[i][1] == "sys-chinese" || init[i][1] == "fireflysung") &&
               (lan == "chinese" || lan == "taiwanese"));
      else if ((init[i][1] == "sys-japanese" || init[i][1] == "ipa") &&
               lan == "japanese");
      else if ((init[i][1] == "sys-korean" || init[i][1] == "unbatang") &&
               lan == "korean");
      else new_init << init[i];
    }
    else new_init << init[i];

  if (new_style != style)
    t= change_doc_attr (t, "style", new_style);
  if (new_init != init)
    t= change_doc_attr (t, "initial", new_init);
  return t;
}

/******************************************************************************
* Rename varsession package
******************************************************************************/

tree
upgrade_varsession (tree t) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "style") || is_func (t, TUPLE)) {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      if (t[i] == "varsession") r[i]= "framed-session";
      else r[i]= upgrade_varsession (t[i]);
    }
    return r;
  }
  else if (is_func (t, DOCUMENT)) {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_varsession (t[i]);
    return r;
  }
  else return t;
}

/******************************************************************************
* Upgrade subsessions
******************************************************************************/

tree
upgrade_subsession (tree t, bool in_session= false) {
  int i;
  if (is_atomic (t)) return t;
  else if (in_session && is_compound (t, "folded", 2))
    return compound ("folded-subsession", upgrade_subsession (t[0]),
                     upgrade_subsession (t[1], true));
  else if (in_session && is_compound (t, "unfolded", 2))
    return compound ("unfolded-subsession", upgrade_subsession (t[0]),
                     upgrade_subsession (t[1], true));
  else {
    bool flag=
      (is_func (t, DOCUMENT) && in_session) ||
      is_compound (t, "session");
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_subsession (t[i], flag);
    return r;
  }
}

/******************************************************************************
* Upgrade quotes
******************************************************************************/

static hashset<string> std_textual_envs;

static array<string>&
operator << (array<string>& a, const char* s) {
  return a << string (s);
}

static bool
is_std_textual_env (string s) {
  if (N(std_textual_envs) == 0) {
    array<string> a;
    a << "part" << "chapter" << "section" << "subsection"
      << "subsubsection" << "paragraph" << "subparagraph" << "appendix"
      << "part*" << "chapter*" << "section*" << "subsection*"
      << "subsubsection*" << "paragraph*" << "subparagraph*" << "appendix*"
      << "abstract" << "theorem" << "proposition" << "lemma"
      << "corollary" << "proof" << "axiom" << "definition"
      << "notation" << "conjecture" << "remark" << "note"
      << "example" << "exercise" << "warning" << "convention"
      << "theorem*" << "proposition*" << "lemma*"
      << "corollary*" << "proof*" << "axiom*" << "definition*"
      << "notation*" << "conjecture*" << "remark*" << "note*"
      << "example*" << "exercise*" << "warning*" << "convention*"
      << "acknowledgments" << "quote" << "quotation" << "verse"
      << "indent" << "compact" << "jump-in"
      << "algorithm" << "body" << "render-code"
      << "center" << "left-aligned" << "right-aligned"
      << "small-table" << "big-table" << "small-figure" << "big-figure"
      << "tabular" << "tabular*" << "block" << "block*" << "descriptive-table"
      << "description" << "itemize" << "enumerate"
      << "bibliography" << "bib-list" << "thebibliography"
      << "bib-field" << "bib-entry" << "db-field" << "db-entry"
      << "itemize-minus" << "enumerate-roman" << "enumerate-alpha"
      << "strong" << "em" << "dfn" << "sample"
      << "name" << "person" << "cite*" << "abbr" << "acronym"
      << "really-tiny" << "tiny" << "very-small" << "small" << "flat-size"
      << "normal-size" << "large" << "very-large" << "huge" << "really-huge"
      << "underline" << "overline" << "pastel" << "greyed" << "light"
      << "british" << "bulgarian" << "chinese" << "croatian"
      << "czech" << "danish" << "dutch" << "english" << "finnish"
      << "french" << "german" << "greek" << "hungarian" << "italian"
      << "japanese" << "korean" << "polish" << "portuguese" << "romanian"
      << "russian" << "slovene" << "spanish" << "swedish"
      << "taiwanese" << "ukrainian"
      << "switch" << "screens" << "tiny-switch"
      << "shown" << "hidden" << "shown*" << "hidden*"
      << "unroll" << "unroll-compressed" << "unroll-phantoms" << "unroll-greyed"
      << "folded" << "unfolded";
    for (int i=0; i<N(a); i++) std_textual_envs->insert (a[i]);
  }
  return std_textual_envs->contains (s);
}

static string
upgrade_quotes (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; )
    if (s[i] == '<') {
      int start= i;
      tm_char_forwards (s, i);
      r << s (start, i);
    }
    else if (s[i] == '-' && i+1 < n && s[i+1] == '-') { r << "\25"; i += 2; }
    else if (s[i] == '\'' && i+1 < n && s[i+1] == '\'') { r << "\21"; i += 2; }
    else if (s[i] == '`' && i+1 < n && s[i+1] == '`') { r << "\20"; i += 2; }
    else { r << s[i]; i++; }
  return r;
}

tree
upgrade_quotes (tree t) {
  if (is_atomic (t))
    return upgrade_quotes (t->label);
  else if (is_concat (t)) {
    int i, n= N(t);
    tree r (t, n);
    bool adjust= false;
    for (i=0; i<n; i++) {
      adjust= adjust || is_compound (t[i], "emdash", 0);
      r[i]= upgrade_quotes (t[i]);
    }
    if (adjust) r= simplify_correct (r);
    return r;
  }
  else if (is_compound (t, "emdash", 0))
    return "\26";
  else if (is_compound (t, "body", 1))
    return compound ("body", upgrade_quotes (t[0]));
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      if (is_std_textual_env (as_string (L(t))))
        r[i]= upgrade_quotes (t[i]);
      else if (std_drd->get_type_child (t, i) != TYPE_REGULAR ||
               std_drd->get_env_child (t, i, MODE, "text") != "text")
        r[i]= t[i];
      else r[i]= upgrade_quotes (t[i]);
    }
    return r;
  }
}

/******************************************************************************
* Upgrade ancient
******************************************************************************/

static charp equation_tags[]= {
  "equation", "equation*", "eqnarray", "eqnarray*",
  "leqnarray", "leqnarray*", "align", "align*",
  "falign", "falign*", "aligned", "aligned*",
  "multline", "multline*", "gather", "gather*",
  "eqsplit", "eqsplit*",
  ""
};

bool
is_equation_env (tree t) {
  if (is_atomic (t) || N(t) != 1) return false;
  static hashset<tree_label> H;
  if (N(H) == 0)
    for (int i=0; equation_tags[i][0] != '\0'; i++)
      H->insert (as_tree_label (equation_tags[i]));
  return H->contains (L(t));
}

tree
upgrade_ancient (tree t) {
  // Miscellaneous upgradings for old documents
  if (is_atomic (t)) return t;
  else if (is_func (t, INACTIVE, 1) && is_func (t[0], RIGID))
    return upgrade_ancient (t[0]);
  else if (is_equation_env (t) && !is_func (t[0], DOCUMENT))
    return tree (L(t), tree (DOCUMENT, upgrade_ancient (t[0])));
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_ancient (t[i]);
    if (is_func (r, WITH)) {
      bool font_series= false;
      for (i=0; i+1<n; i+=2)
        if (r[i] == FONT_SERIES) font_series= true;
      for (i=0; i+1<n; i+=2)
        if (r[i] == MATH_FONT_SERIES && !font_series) {
          tree ins= tree (WITH, FONT_SERIES, copy (r[i+1]));
          return r (0, i) * ins * r (i, N(r));
        }
    }
    return r;
  }
}

/******************************************************************************
* Upgrade draw over/under
******************************************************************************/

tree
upgrade_draw_over_under (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "draw-over", 2))
    return compound ("draw-over",
                     upgrade_ancient (t[0]),
                     upgrade_ancient (t[1]), "0cm");
  else if (is_compound (t, "draw-under", 2))
    return compound ("draw-under",
                     upgrade_ancient (t[0]),
                     upgrade_ancient (t[1]), "0cm");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_draw_over_under (t[i]);
    return r;
  }
}

/******************************************************************************
* Upgrade qed
******************************************************************************/

tree
upgrade_qed (tree t) {
  if (is_atomic (t)) return t;
  else if (t == tree (VALUE, "qed"))
    return compound ("qed");
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= upgrade_qed (t[i]);
    return r;
  }
}

/******************************************************************************
* Preserve spacing of older documents
******************************************************************************/

tree
preserve_spacing (tree t) {
  if (!is_non_style_document (t)) return t;
  tree style= copy (extract (t, "style"));
  if (is_atomic (style)) style= tuple (style);
  if (style == tree (TUPLE)) style= tuple ("generic");
  for (int i=0; i<N(style); i++)
    if (style[i] == "source") return t;
    else if (style[i] == "old-spacing") return t;
    else if (style[i] == "default-spacing") return t;
    else if (style[i] == "wide-spacing") return t;
    else if (style[i] == "invisible-multiply") return t;
    else if (style[i] == "narrow-multiply") return t;
    else if (style[i] == "regular-multiply") return t;
  style << "old-spacing";
  return change_doc_attr (t, "style", style);
}

/******************************************************************************
* Rename styles
******************************************************************************/

tree
rename_style (tree t, string old_name, string new_name) {
  int i;
  if (is_atomic (t)) return t;
  else if (is_compound (t, "style") || is_func (t, TUPLE)) {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      if (t[i] == old_name) r[i]= new_name;
      else r[i]= rename_style (t[i], old_name, new_name);
    }
    return r;
  }
  else if (is_func (t, DOCUMENT)) {
    int n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= rename_style (t[i], old_name, new_name);
    return r;
  }
  else return t;
}

/******************************************************************************
* Upgrade from previous versions
******************************************************************************/

tree
upgrade_tex (tree t) {
  upgrade_tex_flag= true;
  t= upgrade_apply_expand_value (t);
  t= upgrade_new_environments (t);
  t= upgrade_items (t);
  t= upgrade_table (t);
  t= upgrade_split (t, false);
  t= simplify_correct (upgrade_mod_symbols (t));
  t= upgrade_menus_in_help (t);
  t= upgrade_capitalize_menus (t);
  t= upgrade_formatting (t);
  t= upgrade_expand (t, EXPAND);
  t= upgrade_expand (t, HIDE_EXPAND);
  t= upgrade_expand (t, VAR_EXPAND);
  t= upgrade_xexpand (t);
  t= upgrade_function (t);
  t= upgrade_apply (t);
  t= upgrade_env_vars (t);
  t= upgrade_style_rename (t);
  t= upgrade_item_punct (t);
  t= substitute (t, tree (VALUE, "hrule"), compound ("hrule"));
  t= upgrade_math (t);
  t= upgrade_resize_clipped (t);
  t= with_correct (t);
  t= superfluous_with_correct (t);
  t= upgrade_brackets (t);
  t= move_brackets (t);
  t= upgrade_image (t);
  t= upgrade_math_ops (t);
  t= clean_spaces (t);
  t= clean_header (t);
  t= upgrade_doc_language (t);
  t= upgrade_quotes (t);
  t= upgrade_ancient (t);
  upgrade_tex_flag= false;
  return t;
}

tree
upgrade_mathml (tree t) {
  t= upgrade_brackets (t, "math");
  t= downgrade_big (t);
  return t;
}

tree
upgrade (tree t, string version) {
  if (version_inf (version, "0.3.1.9")) {
    path p;
    t= upgrade_textual (t, p);
  }
  if (version_inf (version, "0.3.3.1"))
    t= upgrade_apply_expand_value (t);
  if (version_inf (version, "0.3.3.20"))
    t= upgrade_new_environments (t);
  if (version_inf (version, "0.3.3.24"))
    t= upgrade_items (t);
  if (version_inf (version, "0.3.4.4"))
    t= upgrade_resize (t);
  if (version_inf_eq (version, "0.3.4.7"))
    t= upgrade_table (t);
  if (version_inf_eq (version, "0.3.4.8"))
    t= upgrade_split (t, false);
  if (version_inf_eq (version, "0.3.5.6"))
    t= upgrade_project (t);
  if (version_inf_eq (version, "0.3.5.10"))
    t= upgrade_title (t);
  if (version_inf_eq (version, "1.0.0.1"))
    t= upgrade_cas (t);
  if (version_inf_eq (version, "1.0.0.8"))
    t= simplify_correct (upgrade_mod_symbols (t));
  if (version_inf_eq (version, "1.0.0.11"))
    t= upgrade_menus_in_help (t);
  if (version_inf_eq (version, "1.0.0.13"))
    t= upgrade_capitalize_menus (t);
  if (version_inf_eq (version, "1.0.0.19"))
    t= upgrade_traverse_branch (t);
  if (version_inf_eq (version, "1.0.1.20"))
    t= upgrade_session (t);
  if (version_inf_eq (version, "1.0.2.0"))
    t= upgrade_formatting (t);
  if (version_inf_eq (version, "1.0.2.3"))
    t= upgrade_expand (t, EXPAND);
  if (version_inf_eq (version, "1.0.2.4"))
    t= upgrade_expand (t, HIDE_EXPAND);
  if (version_inf_eq (version, "1.0.2.5")) {
    t= upgrade_expand (t, VAR_EXPAND);
    t= upgrade_xexpand (t);
  }
  if (version_inf_eq (version, "1.0.2.6")) {
    t= upgrade_function (t);
    t= upgrade_apply (t);
  }
  if (version_inf_eq (version, "1.0.2.8"))
    t= upgrade_env_vars (t);
  if (version_inf_eq (version, "1.0.3.3"))
    t= upgrade_use_package (t);
  if (version_inf_eq (version, "1.0.3.4"))
    t= upgrade_style_rename (t);
  if (version_inf_eq (version, "1.0.3.4"))
    t= upgrade_item_punct (t);
  if (version_inf_eq (version, "1.0.3.7"))
    t= upgrade_page_pars (t);
  if (version_inf_eq (version, "1.0.4")) {
    t= substitute (t, tree (VALUE, "hrule"), compound ("hrule"));
    t= upgrade_doc_info (t);
  }
  if (version_inf_eq (version, "1.0.4.6"))
    t= upgrade_bibliography (t);
  if (version_inf_eq (version, "1.0.5.4"))
    t= upgrade_switch (t);
  if (version_inf_eq (version, "1.0.5.7"))
    t= upgrade_fill (t);
  if (version_inf_eq (version, "1.0.5.8"))
    t= upgrade_graphics (t);
  if (version_inf_eq (version, "1.0.5.11"))
    t= upgrade_textat (t);
  if (version_inf_eq (version, "1.0.6.1"))
    t= upgrade_cell_alignment (t);
  if (version_inf_eq (version, "1.0.6.2"))
    t= rename_primitive (t, "hyper-link", "hlink");
  if (version_inf_eq (version, "1.0.6.2"))
    t= upgrade_label_assignment (t);
  if (version_inf_eq (version, "1.0.6.10"))
    t= upgrade_scheme_doc (t);
  if (version_inf_eq (version, "1.0.6.14"))
    t= upgrade_mmx (t);
  if (version_inf_eq (version, "1.0.7.1"))
    t= upgrade_session (t, "scheme", "default");
  if (version_inf_eq (version, "1.0.7.6"))
    t= upgrade_presentation (t);
  if (version_inf_eq (version, "1.0.7.6") && is_non_style_document (t))
    t= upgrade_math (t);
  if (version_inf_eq (version, "1.0.7.7"))
    t= upgrade_resize_clipped (t);
  if (version_inf_eq (version, "1.0.7.7"))
    t= upgrade_image (t);
  if (version_inf_eq (version, "1.0.7.7"))
    t= upgrade_root_switch (t);
  if (version_inf_eq (version, "1.0.7.8"))
    t= upgrade_hyphenation (t);
  if (DEBUG_CORRECT)
    if (is_non_style_document (t))
      math_status_cumul (t);
  if (version_inf_eq (version, "1.0.7.8") && is_non_style_document (t)) {
    t= with_correct (t);
    t= superfluous_with_correct (t);
    t= upgrade_brackets (t);
  }
  if (version_inf_eq (version, "1.0.7.9")) {
    t= move_brackets (t);
    if (is_non_style_document (t))
      t= upgrade_algorithm (t, false);
    t= upgrade_math_ops (t);
  }
  if (version_inf_eq (version, "1.0.7.10"))
    t= downgrade_big (t);
  if (version_inf_eq (version, "1.0.7.13"))
    t= upgrade_gr_attributes (t);
  if (version_inf_eq (version, "1.0.7.14"))
    t= upgrade_cursor (t);
  if (version_inf_eq (version, "1.0.7.15"))
    t= upgrade_cyrillic (t);
  if (version_inf_eq (version, "1.0.7.17")) {
    t= upgrade_metadata (t);
    t= upgrade_abstract_data (t);
    t= correct_metadata (t);
  }
  if (version_inf_eq (version, "1.0.7.20")) {
    t= upgrade_unroll (t);
    t= upgrade_style (t, false);
    t= upgrade_doc_language (t);
  }
  if (version_inf_eq (version, "1.0.7.21")) {
    t= upgrade_varsession (t);
    t= upgrade_subsession (t);
  }
  if (version_inf_eq (version, "1.99.2")) {
    t= upgrade_quotes (t);
    t= upgrade_ancient (t);
  }
  if (version_inf_eq (version, "1.99.4"))
    t= upgrade_draw_over_under (t);
  if (version_inf_eq (version, "1.99.6")) {
    t= upgrade_qed (t);
    if (is_non_style_document (t))
      t= preserve_spacing (t);
  }
  if (version_inf_eq (version, "1.99.8")) {
    if (is_non_style_document (t)) {
      t= rename_style (t, "exam", "old-exam");
      t= rename_style (t, "compact", "old-compact");
      t= rename_style (t, "beamer", "old2-beamer");
    }
  }
  if (version_inf_eq (version, "1.99.9")) {
    t= rename_primitive (t, "solution", "solution*");
    t= rename_primitive (t, "answer", "answer*");
    t= rename_primitive (t, "html-div", "html-div-class");
    t= rename_primitive (t, "html-style", "html-div-style");
  }
  
  if (is_non_style_document (t))
    t= automatic_correct (t, version);
  return t;
}
