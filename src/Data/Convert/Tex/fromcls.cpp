
/******************************************************************************
* MODULE     : fromcls.cpp
* DESCRIPTION: conversion of LaTeX style files into texmacs trees
* COPYRIGHT  : (C) 2012  Francois Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "vars.hpp"

extern bool textm_class_flag;

/******************************************************************************
* Miscellanous
******************************************************************************/

tree
source_short_comment (string s) {
  return compound ("active*", compound ("src-short-comment", s));
}

tree
source_comment (string s) {
  return compound ("active*", compound ("src-comment", s));
}

tree
new_list (string name, int suffix, tree item) {
  name = name * "-" * as_string (suffix);
  return tree (ASSIGN, name, tree (MACRO, "body", compound ("list",
          tree (MACRO, "name", compound ("aligned-item", item)),
          tree (MACRO, "x", tree (ARG, "x")), tree (ARG, "body"))));
}

tree
extract_display (tree t, string s) {
  tree r = tree();
  if (is_func(t, NUMBER, 2) && as_string(t[0]) == s)
    return t;
  else if (!is_atomic(t)) {
    for (int i = 0 ; i < N(t) ; i++) {
      r = extract_display(t[i], s);
      if (r != tree()) return r;
    }
  }
  return r;
}

/******************************************************************************
* Filters
// TODO: factorize the extraction of the content of an assignment, 
//       given it's name.
******************************************************************************/

void
extract_class_body (tree& body, tree doc) {
  if (is_func (doc, DOCUMENT) || is_func (doc, CONCAT))
    for (int i=0; i<N(doc); i++)
      extract_class_body (body, doc[i]);
  else if (is_func (doc, WITH))
    extract_class_body (body, doc[N(doc)-1]);
  else if (is_compound (doc))
    body << doc;
}

tree
extract_class_body (tree doc) {
  tree body= extract (doc, "body");
  if (body != tree (DOCUMENT, "")) doc= body;
  body= tree (DOCUMENT);
  extract_class_body (body, doc);
  return body;
}

array<tree>
filter_theorem (tree doc) {
  array<tree> r = array<tree>();
  string s;
  hashmap<string,string> tm_std_theorems ("");
  hashmap<string,string> tm_std_remarks ("");
  hashmap<string,string> tm_std_exercises ("");


  tm_std_exercises("Exercise")        = "exercise";
  tm_std_exercises("Problem")         = "problem";
  tm_std_remarks  ("Acknowledgments") = "acknowledgments";
  tm_std_remarks  ("Convention")      = "convention";
  tm_std_remarks  ("Example")         = "example";
  tm_std_remarks  ("Note")            = "note";
  tm_std_remarks  ("Remark")          = "remark";
  tm_std_remarks  ("Warning")         = "warning";
  tm_std_theorems ("Axiom")           = "axiom";
  tm_std_theorems ("Conjecture")      = "conjecture";
  tm_std_theorems ("Corollary")       = "corollary";
  tm_std_theorems ("Definition")      = "definition";
  tm_std_theorems ("Lemma")           = "lemma";
  tm_std_theorems ("Notation")        = "notation";
  tm_std_theorems ("Proposition")     = "proposition";
  tm_std_theorems ("Question")        = "question";
  tm_std_theorems ("Theorem")         = "theorem";

  for (int i=0; i<N(doc); i++)
    if (as_string (L(doc[i])) == "new-theorem" && N(doc[i]) > 1) {
      s = as_string (doc[i][1]);
      if (s == "Algorithm");
      else if (tm_std_exercises[s] != "")
        r << compound("new-exercise", tm_std_exercises[s], s);
      else if (tm_std_remarks[s] != "")
        r << compound("new-remark", tm_std_remarks[s], s);
      else if (tm_std_theorems[s] != "")
        r << compound("new-theorem", tm_std_theorems[s], s);
      else
        r << doc[i];
    }
    else if (as_string (L(doc[i])) == "theoremstyle")
      r << source_short_comment ("With theoremstyle: " * as_string (doc[i][0]));
  return r;
}

array<tree>
filter_sectionstyle (tree doc) {
  array<tree> r = array<tree>();
  tree sectional_macros = tree();
  sectional_macros << "part" << "chapter" << "section" << "subsection"
    <<  "subsubsection"  << "paragraph" << "subparagraph";
  for (int j = 0 ; j < N(sectional_macros) ; j++)
    for (int i=0; i<N(doc); i++)
      if (is_func (doc[i], ASSIGN, 2))
        if (doc[i][0] == sectional_macros[j])
          r << doc[i][1][0];
  return r;
}

array<tree>
filter_setfontsize (tree doc) {
  array<tree> r = array<tree>();
  tree font_sizes = tree();
  font_sizes << "tiny" << "scriptsize" << "footnotesize" << "normalsize"
    << "large" << "Large" << "LARGE" << "huge" << "Huge";
  for (int j = 0 ; j < N(font_sizes) ; j++)
    for (int i=0; i<N(doc); i++)
      if (is_func (doc[i], ASSIGN, 2)) 
        if (doc[i][0] == font_sizes[j])
          if (is_func (doc[i][1], MACRO, 1)) 
            if (is_func (doc[i][1][0], WITH) && N(doc[i][1][0]) > 4) {
              tree t = tree (WITH);
              for (int k=0; k<4 ; k++)
                t << doc[i][1][0][k];
              t << tree (ARG, "x");
              r << tree (ASSIGN, doc[i][0], tree (MACRO, "x", t));
            }
  return r;
}

array<tree>
filter_setlength (tree doc) {
  tree global_layout_parameters = tree();
  array<tree> r = array<tree>();
  global_layout_parameters
    << "tex-odd-side-margin" << "tex-even-side-margin" << "tex-text-width"
    << "tex-top-margin" << "tex-head-height" << "tex-top-skip"
    << "tex-text-height" << "tex-foot-skip" << "tex-footnote-sep"
    << "tex-column-sep" << "tex-margin-par-width" << "tex-jot"
    << "tex-math-indent" << "tex-above-display-skip"
    << "tex-below-display-skip" << "tex-above-display-short-skip"
    << "tex-below-display-short-skip" << "par-first";
  for (int j = 0 ; j < N(global_layout_parameters) ; j++)
    for (int i = 0 ; i < N(doc) ; i++)
      if (is_func (doc[i], ASSIGN, 2))
        if (doc[i][0] == global_layout_parameters[j])
          r << doc[i];
  return r;
}

array<tree>
filter_custom_names (tree doc) {
  tree custom_names = tree();
  array<tree> r = array<tree>();
  custom_names
    << "abstract-text" << "appendix-text" << "table-of-contents-text"
    << "figure-text" << "index-text" << "list-of-figures-text"
    << "list-of-tables-text" << "part-text" << "bibliography-text";
  for (int j = 0 ; j < N(custom_names) ; j++)
    for (int i = 0 ; i < N(doc) ; i++)
      if (is_func (doc[i], ASSIGN, 2))
        if (doc[i][0] == custom_names[j])
          r << doc[i];
  return r;
}

array<tree>
filter_itemize (tree doc) {
  string s;
  tree item;
  array<tree> r = array<tree>();
  int n = 0;
  int maxlevel = 0;
  for (int i=0; i<N(doc); i++) {
    if (is_func (doc[i], ASSIGN, 2)) {
      s = as_string (doc[i][0]);
      if (read (s, "labelitem"))
        if (is_func (doc[i][1], MACRO, 1)) {
          s= (s(9, N(s)));
          item = doc[i][1][0];
          if (s == "i")        n = 1;
          else if (s == "ii")  n = 2;
          else if (s == "iii") n = 3;
          else if (s == "iv")  n = 4;
          else return r;
          r << new_list ("itemize", n, item);
          maxlevel = max (maxlevel, n);
        }
    }
  }
  if (maxlevel > 0) {
    maxlevel = min (maxlevel, 4);
    r << tree (ASSIGN, "itemize-reduce", tree (MACRO, "nr", 
          tree (MINIMUM, tree (ARG, "nr"), as_string (maxlevel))));
  }
  return r;
}

array<tree>
filter_enumerate (tree doc) {
  string s, f, t;
  tree item;
  array<tree> r = array<tree>();
  int n = 0;
  int maxlevel = 0;
  for (int i=0; i<N(doc); i++) {
    if (is_func (doc[i], ASSIGN, 2)) {
      s = as_string (doc[i][0]);
      if (read (s, "labelenum"))
        if (is_func (doc[i][1], MACRO, 1)) {
          s= (s(9, N(s)));
          item = doc[i][1][0];
          if (s == "i")        n = 1;
          else if (s == "ii")  n = 2;
          else if (s == "iii") n = 3;
          else if (s == "iv")  n = 4;
          else return r;
          item = substitute (item, compound ("theenum"*s), tree (ARG, "name"));
          item = substitute (item, "enum"*s, tree (ARG, "name"));
          r << new_list ("enumerate", n, item);
          maxlevel = max (maxlevel, n);
        }
    }
  }
  if (maxlevel > 0) {
    maxlevel = min (maxlevel, 4);
    r << tree (ASSIGN, "enumerate-reduce", tree (MACRO, "nr", 
          tree (MINIMUM, tree (ARG, "nr"), as_string (maxlevel))));
  }
  return r;
}

array<tree>
filter_counters (tree doc) {
  array<tree> r = array<tree>();
  tree prefix, display;
  string s;
  for (int i=0; i<N(doc); i++) {
    if (is_func (doc[i], ASSIGN, 2)) {
      s = as_string (doc[i][0]);
      if (read (s, "the-") && N(s) > 4) {
        s = s(4,N(s));
        if (is_func (doc[i][1], MACRO, 1)) {
          prefix = extract_display (doc[i][1][0], s);
          display = substitute (prefix, s, tree (ARG, "nr"));
          r << tree(ASSIGN, "display-"*s, tree(MACRO, "nr", display));
          r << substitute (doc[i], prefix, 
              compound("display-"*s, compound(s*"-nr")));
        }
      }
    }
  }
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
latex_class_filter (tree t) {
  t= extract_class_body (t);
  tree   incls = tree (USE_PACKAGE, "article", "std-latex", "section-base");
  tree   doc   = tree (DOCUMENT);
  string header= "This style file is the result of an automatic conversion";

  /*
  cout << "-------------------------  Global Tree  -------------------------\n";
  cout << t << "\n";
  cout << "-----------------------------------------------------------------\n";
  */

  doc << source_comment (header)
      << incls
      << source_comment ("Global layout parameters");
  doc << filter_setlength (t);

  doc << source_comment ("Font sizes");
  doc << filter_setfontsize (t);

  doc << source_comment ("Sectional macros");
  doc << filter_sectionstyle (t);

  doc << source_comment ("Theorems-like macros");
  doc << source_short_comment ("With theoremstyle: default");
  doc << filter_theorem (t);

  doc << source_comment ("Itemize lists");
  doc << filter_itemize (t);

  doc << source_comment ("Enumerate lists");
  doc << filter_enumerate (t);

  doc << source_comment ("Counter rendering");
  doc << filter_counters (t);

  doc << source_comment ("Customization of environments");
  doc << filter_custom_names (t);

  // Not implemented
  doc << source_comment ("Headers and footers");

  doc << source_comment ("Rendering of floating objects");

  doc << source_comment ("Title rendering");

  doc << source_comment ("Bibliography");

  doc << source_comment ("Miscellaneous macros provided by the style");

  doc << source_comment ("Tables of contents");

  doc << source_comment ("Indexes and glossaries");

  tree the_style= compound ("style", "source");
  tree preamble = tree (ASSOCIATE, PREAMBLE, "true");
  tree the_init = compound ("initial", tree (COLLECTION, preamble));
  tree the_body = compound ("body", doc);

  return tree (DOCUMENT, the_style, the_init, the_body);
}

tree
latex_class_document_to_tree (string s) {
  bool old= textm_class_flag;
  textm_class_flag= true;  
  tree t= latex_document_to_tree (s, false);
  tree r= latex_class_filter (t);
  textm_class_flag= old;
  return r;
}
