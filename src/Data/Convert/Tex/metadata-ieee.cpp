
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

static bool
is_and (tree t, int &i) {
  bool b = N(t) > i+4 && t[i] == "a" && t[i+1] == "n" && t[i+2] == "d"
                      && (t[i+3] == "~" || t[i+3] == tuple ("\\nbsp"));
  if (b) i+=3;
  return b;
}

static tree
add_author_datas (tree author, array<tree> author_datas) {
  if (!is_apply (author) || N(author) < 2) return concat ();
  for (int i=0; i<N(author_datas); i++) {
    for (int j=1; j<N(author_datas[i]); j++)
      author << author_datas[i][j];
  }
  return author;
}

static array<tree>
select_affs_containing (array<tree> affs, tree t) {
  array<tree> r;
  for (int i=0; i<N(affs); i++)
    if (contains (t, A(affs[i])))
      r << affs[i];
  return r;
}

static array<tree>
merge_author_datas (array<tree> authors, array<tree> affs) {
  array<tree> r, author_affs;
  for (int i=0; i<N(authors); i++) {
    author_affs= array<tree>();
    for (int j=1; j<N(authors[i]); j++)
      if (is_apply (authors[i][j], "\\author-inst", 1))
        author_affs << select_affs_containing (affs, authors[i][j]);
    r << add_author_datas (authors[i], author_affs);
  }
  return r;
}

static array<tree>
get_ieee_author_datas (tree t, string s) {
  bool root= false;
  if (s == "") {
    root= true;
    s= "\\author-name";
  }
  int i, n=N(t);
  array<tree> r, tmp_a, tmp_n;
  tree u;
  tree author_data (APPLY, "\\author-data");
  tree author_name (CONCAT);
  for (i=0; i<=n; i++) {
    if (i<n) u= t[i];
    else u= concat();
    if (i==n
        || (root && (is_tuple (u, "\\and") || is_and (t, i)))
        || (!root && (is_tuple (u, "\\tmsep")
            || (s == "author-name" && t[i] == string (","))))) {
      if (N(author_name) > 1) {
        author_data << tree (APPLY, s, author_name);
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
    else if (is_tuple (u, "\\IEEEauthorblockN", 1)) {
      tmp_n= get_ieee_author_datas (u[1], "\\author-name");
      if (N(tmp_n) == 1) author_data= tmp_n[0];
      author_name= tree (CONCAT);
    }
    else if (is_tuple (u, "\\IEEEauthorblockA", 1)) {
      tmp_a= get_ieee_author_datas (u[1], "\\author-affiliation");
      if (N(tmp_n) <= 1) {
        for (int j=0; j<N(tmp_a); j++)
          for (int k=0; k<N(tmp_a[j]); k++)
            author_data << tmp_a[j][k];
        tmp_n= array<tree>();
      }
      else {
        tmp_n= merge_author_datas (tmp_n, tmp_a);
      }
      author_name= tree (CONCAT);
    }
    else if (is_tuple (u, "\\tmaffiliation", 1)) {
      author_data << tree (APPLY, "\\author-affiliation", u[1]);
    }
    else if (is_tuple (u, "\\tmmisc", 1)) {
      author_data << tree (APPLY, "\\author-misc", u[1]);
    }
    else if (is_tuple (u, "\\tmnote", 1)) {
      author_data << tree (APPLY, "\\author-note", u[1]);
    }
    else if (is_tuple (u, "\\tmhomepage", 1)) {
      author_data << tree (APPLY, "\\author-homepage", u[1]);
    }
    else if (is_tuple (u, "\\IEEEauthorrefmark", 1)) {
      author_data << tree (APPLY, "\\author-inst", u[1]);
    }
    else if (is_tuple (u, "\\tmieeeemail", 1)) {
      author_data << tree (APPLY, "\\author-email", u[1]);
    }
    else if (is_tuple (u, "\\begin-affiliation")) {
      tree tmp(CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-affiliation")) tmp << t[i++];
      author_data << tree (APPLY, "\\author-affiliation", tmp);
    }
    else if (is_tuple (u, "\\email", 1)) {
      author_data << tree (APPLY, "\\author-email", u[1]);
    }
    else
      author_name << u;
  }
  if (N(tmp_n) > 0) r << tmp_n;
  return r;
}

static array<tree>
get_ieee_author_datas (tree t) {
  return get_ieee_author_datas (t, "");
}

tree
collect_metadata_ieee (tree t) {
  int i;
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes;
  array<tree> tmp= collect_metadata_latex (t, &get_ieee_author_datas);
  for (i=0; i<N(tmp); i++) {
    if (is_apply (tmp[i], "\\doc-data")) doc_data= tmp[i];
    if (is_apply (tmp[i], "\\abstract-data")) abstract_data= tmp[i];
  }
  for (i=0; i<N(t); i++) {
    u= t[i];
    if (is_tuple (u, "\\begin-IEEEkeywords")) {
      i++;
      tree kw (CONCAT);
      while (i<N(t) && !is_tuple (t[i], "\\end-IEEEkeywords"))
        kw << t[i++];
      array<tree> sep= A(concat (",", ";",
                                 tuple ("\\tmsep"), tuple ("\\tmSep")));
      array<tree> tmp= tokenize_concat (kw, sep);
      if (N(tmp) > 0) {
        kw= tree (APPLY, "\\abstract-keywords");
        kw << tmp;
        abstract_data << kw;
      }
    }
  }
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
  return r;
}
