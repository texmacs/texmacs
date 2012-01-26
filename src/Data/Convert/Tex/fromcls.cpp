
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

/******************************************************************************
* Interface
******************************************************************************/

tree
source_comment (string s) {
  return compound ("active*", compound ("src-comment", s));
}

tree
latex_class_to_tree (tree t1) {
  (void) t1;
  tree   incls = tree (USE_PACKAGE, "article", "std-latex");
  tree   doc   = tree (DOCUMENT);
  string header= "This style file is the result of an automatic conversion";
  doc << source_comment (header)
      << incls
      << source_comment ("Global layout parameters")
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
  return latex_class_to_tree (tree (DOCUMENT, ""));
  /*
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();
  tree t= parse_latex_document (s, true);
  tree r= latex_class_to_tree (t);
  command_type ->shorten ();
  command_arity->shorten ();
  command_def  ->shorten ();
  return r;
  */
}
