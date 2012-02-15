
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
* Interface
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
  for (int i=0; i<N(body); i++)
    cout << body[i] << "\n";
  return body;
}

tree
source_comment (string s) {
  return compound ("active*", compound ("src-comment", s));
}

array<tree>
filter_setlength (tree doc, string var) {
  for (int i=0; i<N(doc); i++)
    if (is_func (doc[i], ASSIGN, 2))
      if (doc[i][0] == var)
	return A (tuple (doc[i]));
  return array<tree> ();
}

tree
latex_class_filter (tree t) {
  t= extract_class_body (t);
  tree   incls = tree (USE_PACKAGE, "article", "std-latex");
  tree   doc   = tree (DOCUMENT);
  string header= "This style file is the result of an automatic conversion";
  doc << source_comment (header)
      << incls
      << source_comment ("Global layout parameters")
      << filter_setlength (t, "tex-odd-side-margin")
      << filter_setlength (t, "tex-even-side-margin")
      << filter_setlength (t, "tex-text-width")
      << filter_setlength (t, "tex-top-margin")
      << filter_setlength (t, "tex-head-height")
      << filter_setlength (t, "tex-top-skip")
      << filter_setlength (t, "tex-text-height")
      << filter_setlength (t, "tex-foot-skip")
      << filter_setlength (t, "tex-footnote-sep")
      << filter_setlength (t, "tex-column-sep")
      << filter_setlength (t, "tex-margin-par-width")
    //<< filter_setlength (t, "par-first")
      << filter_setlength (t, "tex-jot")
      << filter_setlength (t, "tex-math-indent")
      << filter_setlength (t, "tex-above-display-skip")
      << filter_setlength (t, "tex-below-display-skip")
      << filter_setlength (t, "tex-above-display-short-skip")
      << filter_setlength (t, "tex-below-display-short-skip")
      << source_comment ("Headers and footers")
      << source_comment ("Font sizes");
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
  tree t= latex_document_to_tree (s);
  tree r= latex_class_filter (t);
  textm_class_flag= old;
  return r;
}
