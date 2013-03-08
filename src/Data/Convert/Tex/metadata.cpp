
/******************************************************************************
* MODULE     : metadata.cpp
* DESCRIPTION: conversion of tex metadata into texmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"

bool
is_metadata_env (tree u) {
  return is_tuple (u, "\\begin-frontmatter") ||
         is_tuple (u, "\\begin-abstract");
}

bool
is_metadata (tree u) {
  return is_tuple (u, "\\address")           || 
         is_tuple (u, "\\affiliation")       || 
         is_tuple (u, "\\additionalauthors") || 
         is_tuple (u, "\\author")            || 
         is_tuple (u, "\\author*")           || 
         is_tuple (u, "\\category")          || 
         is_tuple (u, "\\category*")         || 
         is_tuple (u, "\\classification")    || 
         is_tuple (u, "\\conferenceinfo")    || 
         is_tuple (u, "\\CopyrightYear")     || 
         is_tuple (u, "\\crdata")            || 
         is_tuple (u, "\\date")              || 
         is_tuple (u, "\\doc-acm")           || 
         is_tuple (u, "\\doc-terms")         || 
         is_tuple (u, "\\email")             || 
         is_tuple (u, "\\footnotetext")      || 
         is_tuple (u, "\\footnotetext*")     || 
         is_tuple (u, "\\keywords")          || 
         is_tuple (u, "\\numberofauthors")   || 
         is_tuple (u, "\\pagenumbering")     || 
         is_tuple (u, "\\subjclass")         || 
         is_tuple (u, "\\subjclass*")        || 
         is_tuple (u, "\\subtitle")          || 
         is_tuple (u, "\\terms")             || 
         is_tuple (u, "\\title")             || 
         is_tuple (u, "\\title*")            || 
         is_tuple (u, "\\urladdr");
}

static tree
collect_metadata_latex (tree t) {
  int i, n=N(t);
  tree r (CONCAT);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_tuple (u, "\\title", 1)  || is_tuple (u, "\\title*", 2)  ||
        is_tuple (u, "\\author", 1) || is_tuple (u, "\\author*", 2) ||
        is_tuple (u, "\\date", 1)) {
      tree v= concat(), w=u[N(u)-1];
      array<tree> l;
      if (is_atomic (w))
          r << tuple (u[0], w);
      else {
        for (int j=0; j<N(w); j++) {
          if (is_tuple (w[j], "\\thanks", 1)) {
            if (u[0] == "\\title" || u[0] == "\\title*")
              l << tuple ("\\title-thanks", w[j][1]);
            else if (u[0] == "\\author" || u[0] == "\\author*")
              l << tuple ("\\author-note", w[j][1]);
          }
          else if (is_tuple (w[j], "\\and", 0)) {
            if (u[0] == "\\author" || u[0] == "\\author*") {
              r << tuple ("\\author", v);
              v= concat ();
            }
          }
          else
            v << w[j];
        }
        if (is_tuple (u, "\\title", 1)  || is_tuple (u, "\\title*", 2)) {
          r << tuple ("\\title", v);
        }
        else if (is_tuple (u, "\\author", 1) || is_tuple (u, "\\author*", 2)) {
          r << tuple ("\\author", v);
        }
        else {
          r << tuple (u[0], v);
        }
        for (int j=0; j<N(l); j++)
          r << l[j];
      }
    }
  }
  return r;
}

tree
collect_metadata (tree t, tree latex_classe) {
  string s = "article";
  if (is_tuple (latex_classe, "\\documentclass", 1) ||
      is_tuple (latex_classe, "\\documentstyle", 1))
    s = latex_verbarg_to_string (latex_classe[1]);
  else if (is_tuple (latex_classe, "\\documentclass*", 2) ||
           is_tuple (latex_classe, "\\documentstyle*", 2))
    s = latex_verbarg_to_string (latex_classe[2]);

  if (s == "acm_proc_article-sp" ||
      s == "sig-alternate" || s == "sig-alt-full")
    return collect_metadata_acm (t);
  else if (s == "elsarticle" || s == "elsart")
    return collect_metadata_elsevier (t);
  return collect_metadata_latex (t);
}
