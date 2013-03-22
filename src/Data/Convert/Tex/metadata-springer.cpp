
/******************************************************************************
* MODULE     : metadata-springer.cpp
* DESCRIPTION: conversion of (Springer) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "metadata.hpp"

static bool
is_springer_titlenote (tree t) {
  return is_tuple (t, "\\thanks", 1);
}

static void
get_springer_title_notes (tree t, array<tree> &r) {
  if (is_atomic (t)) return;
  if (is_springer_titlenote (t)) {
    r << tree (APPLY, "\\doc-note", t[1]);
    return;
  }
  if (is_tuple (t, "\\tmmisc", 1)) {
    r << tree (APPLY, "\\doc-misc", t[1]);
    return;
  }
  int i, n=N(t);
  for (i=0; i<n; i++)
    get_springer_title_notes (t[i], r);
}

#define cstm clean_springer_title_markup

static tree
clean_springer_title_markup (tree t) {
  if (is_atomic (t)) return t;
  if (is_springer_titlenote (t) ||
      is_tuple (t, "\\tmisc", 1))
    return concat();
  if (is_tuple (t, "\\ttlit")) {
    t[0]= "\\it";
    return t;
  }
  tree r (L(t));
  int i, n=N(t);
  for (i=0; i<n; i++)
      r << clean_springer_title_markup (t[i]);
  return r;
}

tree
collect_metadata_svmono (tree t) {
  int i, n=N(t);
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes;
  array<tree> tmp= collect_metadata_latex (t);
  for (i=0; i<N(tmp); i++) {
    if (is_apply (tmp[i], "\\doc-data")) doc_data= tmp[i];
    if (is_apply (tmp[i], "\\abstract-data")) abstract_data= tmp[i];
  }
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\subtitle", 1)) {
      get_springer_title_notes (u[1], doc_notes);
      doc_data << tree (APPLY, "\\doc-subtitle", cstm (u[1]));
    }
  }
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
  return r;
}

#undef cstm
