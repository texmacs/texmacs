
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
         is_tuple (u, "\\maketitle")         ||
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

#define cltm clean_latex_title_markup

static bool
is_latex_titlenote (tree t) {
  return is_tuple (t, "\\thanks", 1);
}

static tree
clean_latex_title_markup (tree t) {
  if (is_atomic (t)) return t;
  if (is_latex_titlenote (t)) return concat();
  tree r (L(t));
  int i, n=N(t);
  for (i=0; i<n; i++)
      r << clean_latex_title_markup (t[i]);
  return r;
}

static void
get_latex_title_notes (tree t, array<tree> &r) {
  if (is_atomic (t)) return;
  if (is_latex_titlenote (t)) {
    tree u= copy(t);
    u[0]= "\\doc-note";
    r << u;
    return;
  }
  int i, n=N(t);
  for (i=0; i<n; i++)
    get_latex_title_notes (t[i], r);
}

static array<tree>
get_latex_author_datas (tree t) {
  int i, n=N(t);
  array<tree> r;
  tree u;
  tree author_data (APPLY, "\\author-data");
  tree author_name (CONCAT);
  for (i=0; i<=n; i++) {
    if (i<n) u= t[i];
    else u= concat();

    if (i==n || is_tuple (u, "\\and")) {
      if (N(author_name) > 1) {
        author_data << tree (APPLY, "\\author-name", author_name);
        author_name= tree (CONCAT);
      }
      if (N(author_data) > 1) {
        r << author_data;
        author_data= tree (APPLY, "\\author-data");
      }
    }
    else if (is_tuple (u, "\\thanks", 1)) {
      author_data << tree (APPLY, "\\author-misc", u[1]);
    }
    else {
      author_name << u;
    }
  }
  return r;
}

static tree
collect_metadata_latex (tree t) {
  tree r (CONCAT);
  tree doc_data (TUPLE, "\\doc-data");
  tree abstract_data (TUPLE, "\\abstract-data");
  array<tree> doc_notes;
  bool dated= false;
  int i, n=N(t);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_tuple (u, "\\title", 1)  || is_tuple (u, "\\title*", 2)) {
      get_latex_title_notes (u[N(u)-1], doc_notes);
      doc_data << tuple ("\\doc-title", cltm (u[N(u)-1]));
    }
    else if (is_tuple (u, "\\author", 1)  || is_tuple (u, "\\author*", 2)) {
      array<tree> author_datas= get_latex_author_datas (u[1]);
      for (int j=0; j<N(author_datas); j++)
        doc_data << tree (APPLY, "\\doc-author", author_datas[j]);
    }
    else if (is_tuple (u, "\\date", 1)) {
      dated= true;
      get_latex_title_notes (u[1], doc_notes);
      doc_data << tuple ("\\doc-date", cltm (u[1]));
    }
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract"))
        abstract_text << t[i++];
      abstract_data << tree (APPLY, "\\abstract", abstract_text);
    }
  }
  if (!dated)
    doc_data << tuple ("\\doc-date", tree (APPLY, "\\date", ""));
  if (N(doc_notes) > 0)
    for (int j=0; j<N(doc_notes); j++)
      doc_data << doc_notes[j];
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
  return r;
}

#undef cltm

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
