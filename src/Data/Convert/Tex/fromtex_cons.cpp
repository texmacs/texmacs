
/******************************************************************************
 * MODULE     : fromtex_cons.cpp
 * DESCRIPTION: conservative conversion of tex strings into texmacs trees
 * COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
 ******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "base64.hpp"
#include "scheme.hpp"
#include "vars.hpp"
#include "tree_correct.hpp"
#include "merge_sort.hpp"

/******************************************************************************
 * Reimport of exported TeXmacs documents
 ******************************************************************************/

static array<tree>
remove_empty (array<tree> l) {
  array<tree> r;
  for (int i=0; i<N(l); i++)
    if (N(l[i] != document ("")) > 0)
      r << l[i];
  return r;
}

static array<string>
remove_empty (array<string> l) {
  array<string> r;
  for (int i=0; i<N(l); i++)
    if (l[i] != "")
      r << l[i];
  return r;
}

static tree
remove_preamble_marks (tree t) {
  if (!is_compound (t, "hide-preamble", 1) ||
      !is_document (t[0]) || N(t[0]) <= 2)
    return t;
  int n= N(t[0]);
  return compound ("hide-preamble", t[0](1, n-1));
}

static string
clean_preamble (string s) {
  return replace (s, "{\\tmtexmarkpreamble}\n", "");
}

static array<tree>
tokenize_document (tree body) {
  array<tree> r;
  tree tmp (DOCUMENT);
  for (int i=0; i<N(body); i++) {
    if (is_compound (body[i], "tmtex@mark")) {
      if (N(tmp) > 0) {
        r << tmp;
        tmp= tree (DOCUMENT);
      }
    }
    else
      tmp << body[i];
  }
  if (N(tmp) > 0) r << tmp;
  return r;
}

static array<string>
populates_lambda (hashmap<string,tree> &lambda, tree doc, tree src) {
  string s= as_string (src);
  array<string> l_src= tokenize (s, "\n\n{\\tmtexmark}\n\n");
  l_src= remove_empty (l_src);
  tree body= (N(doc) > 2 && N(doc[2]) > 0)? doc[2][0] : tree (DOCUMENT);
  array<tree> l_body= tokenize_document (body);
  l_body= remove_empty (l_body);
  bool has_preamble= N(l_body) > 0 && N(l_body[0]) > 0
    && is_compound (l_body[0][0], "hide-preamble", 1);
  if (has_preamble && N(l_body)+1 == N(l_src)) {
    // asymetry due to \\end{document} only present in src.
    l_src[0]= clean_preamble (l_src[0]);
    l_body[0][0]= remove_preamble_marks (l_body[0][0]);
    l_src= range (l_src, 0, N(l_src)-1);
  }
  else if (!has_preamble && N(l_body)+2 == N(l_src))
    // asymetry due to \\end{document} and preamble only present in src.
    l_src= range (l_src, 1, N(l_src)-1);
  else
  return array<string> ();
  for (int i=0; i<N(l_body); i++)
    lambda(l_src[i])= l_body[i];
  return l_src;
}

struct key_less_eq_operator {
  static bool leq (string s1, string s2) {
    return N(s1) > N(s2);
  }
};

static array<string>
sort_keys (array<string> l) {
  merge_sort_leq<string,key_less_eq_operator> (l);
  return l;
}

static array<string>
recover_document (string s, string p) {
  array<string> r, t= tokenize (s, "\n" * p * "\n");
  int i, n= N(t);
  if (n < 2) return t;
  for (i=0; i<n-1; i++)
    r << t[i] << p;
  r << t[n-1];
  return r;
}

static array<string>
recover_document (array<string> a, array<string> l, array<string> keys) {
  if (N(l) == 0) return a;
  string p= l[0];
  array<string> r;
  int i, n= N(a);
  for (i=0; i<n; i++)
    if (contains (a[i], keys))
      r << a[i];
    else
      r << recover_document (a[i], p);
  return recover_document (r, range (l, 1, N(l)), keys);
}

array<string>
remove_whitespace (array<string> l) {
  int i, n=N(l);
  array<string> r;
  for (i=0; i<n; i++)
    if (replace (replace (l[i], " ", ""), "\n", "") != "")
      r << l[i];
  return r;
}

int
latex_search_forwards (string s, string in);

static string
remove_end_of_document (string s) {
  int e= latex_search_forwards ("\\end{document}", s);
  if (e > 0)
    return s(0, e);
  else
    return s;
}

static array<string>
recover_document (string s, array<string> l) {
  array<string> a;
  a << remove_end_of_document (s);
  return remove_whitespace (recover_document (a, l, l));
}

bool
is_uptodate_tm_document (tree t) {
  return is_document (t) && N(t) > 1 && is_compound (t[0], "TeXmacs", 1)
    && as_string (t[0][0]) == TEXMACS_VERSION;
}

bool
is_preamble (string s) {
  return latex_search_forwards ("\\documentclass", s) >= 0;
}

tree concat_document_correct (tree t);

tree
latex_conservative_document_to_tree (string s, bool as_pic, bool keep_src,
    array<array<double> > range) {
  int b, e;
  b= search_forwards ("%\n% -----BEGIN TEXMACS DOCUMENT-----\n%", 0, s) + 38;
  e= search_forwards ("% \n% -----END TEXMACS DOCUMENT-----", b, s);
  if (b < e) {
    string code= replace (s(b,e), "% ", "");
    s= s(0, b-38);
    tree d= stree_to_tree (string_to_object (decode_base64 (code)));
    if (is_document (d) && N(d) == 2 && is_uptodate_tm_document (d[0])) {
      tree document= d[0](1, N(d[0]));
      tree uninit= tree (UNINIT);
      hashmap<string,tree> lambda (uninit);
      array<string> keys= populates_lambda (lambda, d[0], d[1]);
      if (N(keys) == 0) return "";
      keys= sort_keys (keys);
      array<string> l= recover_document (s, keys);
      tree body (DOCUMENT);
      int i, n= N(l);
      for (i=0; i<n; i++) {
        tree tmp= lambda[l[i]];
        if (tmp != uninit)
          body << tmp;
        else if (i == 0 && is_preamble (l[i])) {
          document= latex_to_tree (parse_latex_document (
                l[i] * "\n\\end{document}",
                true, as_pic, false, array<array<double> > ()));
          if (is_document (document)       && N(document)       > 1 &&
              is_compound (document[1], "body", 1)                  &&
              is_document (document[1][0]) && N(document[1][0]) > 0 &&
              is_compound (document[1][0][0], "hide-preamble", 1))
            body << document[1][0][0];
        }
        else
          body << latex_to_tree (parse_latex (l[i], true, false, as_pic,
                false, array<array<double> > ()));
      }
      document[1]= compound ("body", body);
      return concat_document_correct (document);
    }
  }
  return "";
}

/******************************************************************************
 * Import of marked LaTeX documents (paragraph breaking managment)
 ******************************************************************************/

tree merge_successive_withs (tree t, bool force_concat= false);
tree unnest_withs (tree t);
tree remove_empty_withs (tree t);
tree concat_sections_and_labels (tree t);
tree modernize_newlines (tree t, bool skip);

static tree
search_and_remove_breaks (tree t, tree &src) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r(L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (!is_atomic (t[i][0]))
        src << A(t[i][0]);
    }
    else {
      tree tmp= search_and_remove_breaks (t[i], src);
      if (is_concat (tmp) && N(tmp) == 0);
      else if (is_concat (tmp) && N(tmp) == 1) r << tmp[0];
      else r << tmp;
    }
  }
  return r;
}

static tree
merge_non_root_break_trees (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree start (as_string (0)), src (CONCAT), r (L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (!is_atomic (t[i][0])) {
        src << A(t[i][0]);
        t[i][0]= src;
        t[i][1]= start;
        r << t[i];
        src= tree (CONCAT);
        start= t[i][2];
      }
    }
    else {
      tree tmp= search_and_remove_breaks (t[i], src);
      if (is_concat (tmp) && N(tmp) == 0);
      else if (is_concat (tmp) && N(tmp) == 1) r << tmp[0];
      else r << tmp;
    }
  }
  return r;
}

static tree
merge_empty_break_trees (tree t) {
  int i, n=N(t);
  bool merge= false;
  tree src, r (L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (merge) {
        src= tree (CONCAT);
        src << A(r[N(r)-1][0]);
        if (!is_atomic (t[i][0]))
          src << A(t[i][0]);
        r[N(r)-1][0]= src;
      }
      else {
        r << t[i];
        merge= true;
      }
    }
    else {
      r << t[i];
      merge= false;
    }
  }
  return r;
}

static bool
contains_title_or_abstract (tree t) {
  if (is_atomic (t)) return false;
  if (is_compound (t, "doc-data") || is_compound (t, "abstract-data"))
    return true;
  int i, n=N(t);
  for (i=0; i<n; i++)
    if (contains_title_or_abstract (t[i]))
      return true;
  return false;
}

static tree
merge_non_ordered_break_trees (tree t) {
  if (!contains_title_or_abstract (t)) return t;
  int i, n=N(t);
  bool merge= false;
  tree src (CONCAT), r (L(t));
  for (i=n-1; i>=0; i--) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (merge)
        src= (t[i][0] << A(src));
      else
        r << t[i];
    }
    else {
      r << t[i];
      if (contains_title_or_abstract (t[i]))
        merge= true;
    }
  }
  n=N(r);
  for (i=0; i<n; i++) {
    if (is_compound (r[i], "textm@break", 3)) {
      if (!is_atomic (r[i][0]))
        src << A(r[i][0]);
      r[i][0]= src;
      break;
    }
  }
  return tree (DOCUMENT, reverse (A(r)));
}

tree
pick_paragraph_breaks (tree t, array<tree> &b) {
  if (is_atomic (t)) return t;
  t= merge_non_root_break_trees (t);
  t= merge_empty_break_trees (t);
  t= merge_non_ordered_break_trees (t);
  t= merge_successive_withs (t);
  t= unnest_withs (t);
  t= remove_empty_withs (t);
  t= concat_sections_and_labels (t);
  int i, n= N(t);
  tree r (L(t));
  tree u (DOCUMENT);
  unsigned int id_cnt= 0;
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      string uid= as_string (id_cnt++);
      tree src= modernize_newlines (t[i][0], false);
      tree from= t[i][1], to= t[i][2];
      src= concat_document_correct (src);
      src= simplify_correct (src);
      if (N(u) == 1 && is_compound (u[0], "hide-preamble"))
        u= u[0];
      b << compound ("associate", "latex-tree-src" * uid,
          compound ("latex-tree-src", u, src, from, to));
      if (u != document ())
        r << u;
      u= tree (DOCUMENT);
    }
    else
      u << t[i];
  }
  if (N(u) > 0 && r == document ())
    return t;
  if (N(u) > 0)
    r << u;
  return r;
}
