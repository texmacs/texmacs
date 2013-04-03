
/******************************************************************************
* MODULE     : metadata-ieee.cpp
* DESCRIPTION: conversion of (IEEE) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "metadata.hpp"

array<tree>
get_ieee_conf_author_datas (tree t) {
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
    else if (is_tuple (u, "\\begin-affiliation")) {
      tree tmp(CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-affiliation")) tmp << t[i++];
      author_data << tree (APPLY, "\\author-affiliation", tmp);
    }
    else if (is_tuple (u, "\\tmmisc", 1)) {
      author_data << tree (APPLY, "\\author-misc", u[1]);
    }
    else if (is_tuple (u, "\\tmnote", 1)) {
      author_data << tree (APPLY, "\\author-note", u[1]);
    }
    else if (is_tuple (u, "\\email", 1)) {
      author_data << tree (APPLY, "\\author-email", u[1]);
    }
    else if (is_tuple (u, "\\tmhomepage", 1)) {
      author_data << tree (APPLY, "\\author-homepage", u[1]);
    }
    else
      author_name << u;
  }
  return r;
}

tree
collect_metadata_ieee_conf (tree t) {
  int i;
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes;
  array<tree> tmp= collect_metadata_latex (t, &get_ieee_conf_author_datas);
  for (i=0; i<N(tmp); i++) {
    if (is_apply (tmp[i], "\\doc-data")) doc_data= tmp[i];
    if (is_apply (tmp[i], "\\abstract-data")) abstract_data= tmp[i];
  }
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
  return r;
}
