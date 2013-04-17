
/******************************************************************************
* MODULE     : metadata-revtex.cpp
* DESCRIPTION: conversion of (RevTeX) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "metadata.hpp"

static bool
revtex_contains_noaffiliation (tree t) {
  for (int i=0; i<N(t); i++)
    if (is_apply (t[i], "\\noaffiliation"))
      return true;
  return false;
}

static bool
revtex_contains_affiliation (tree t) {
  for (int i=0; i<N(t); i++)
    if (is_apply (t[i], "\\author-affiliation"))
      return true;
  return false;
}

static array<tree>
revtex_get_affiliations (tree t) {
  array<tree> r;
  for (int i=0; i<N(t); i++)
    if (is_apply (t[i], "\\author-affiliation"))
      r << t[i];
  return r;
}

static array<tree>
revtex_add_affiliations (array<tree> a, array<tree> affs) {
  for (int i=0; i<N(a); i++)
    a[i] << affs;
  return a;
}

static tree
revtex_translate_affiliations (tree t) {
  tree r(L(t));
  array<tree> tmp;
  for (int i=0; i<N(t); i++) {
    if (is_apply (t[i], "\\noaffiliation"));
    else if (is_apply (t[i], "\\altaffiliation", 1))
      tmp << tree (APPLY, "\\author-affiliation", t[i][1]);
    else r << t[i];
  }
  r << tmp;
  return r;
}

static array<tree>
revtex_translate_affiliations (array<tree> t) {
  for (int i=0; i<N(t); i++)
    t[i]= revtex_translate_affiliations (t[i]);
  return t;
}

static array<tree>
revtex_uncluster_authors (array<tree> a, bool &clustered) {
  array<tree> r, tmp;
  for (int i=0; i<N(a); i++) {
    if (revtex_contains_noaffiliation (a[i]) ||
        revtex_contains_affiliation (a[i])) {
      array<tree> affs= revtex_get_affiliations (a[i]);
      r << revtex_translate_affiliations (revtex_add_affiliations (tmp, affs));
      tmp= array<tree> ();
    }
    if (revtex_contains_affiliation (a[i])) {
      r << revtex_translate_affiliations (a[i]);
    }
    else if (revtex_contains_noaffiliation (a[i]))
      r << revtex_translate_affiliations (a[i]);
    else {
      clustered= true;
      tmp << a[i];
    }
  }
  r << tmp;
  r= revtex_translate_affiliations (r);
  return r;
}

tree
collect_metadata_revtex (tree t) {
  int i, n=N(t);
  bool author= false, clustered= false;
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  tree author_data (APPLY, "\\author-data");
  array<tree> authors;
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\title", 1) || is_tuple (u, "\\title*", 2)) {
      doc_data << tree (APPLY, "\\doc-title", u[N(u)-1]);
      author= false;
    }
    // doc datas
    else if (is_tuple (u, "\\tmsubtitle", 1))
      doc_data << tree (APPLY, "\\doc-subtitle", u[1]);
    else if (is_tuple (u, "\\received", 1) || is_tuple (u, "\\revised", 1) ||
             is_tuple (u, "\\accepted", 1)) {
      string s= as_string (u[0]);
      s= upcase_first (s(1, N(s))) * ": ";
      doc_data << tree (APPLY, "\\doc-date", concat (s, u[1]));
    }
    else if (is_tuple (u, "\\date", 1))
      doc_data << tree (APPLY, "\\doc-date", u[1]);
    else if ((is_tuple (u, "\\thanks", 1) ||
              is_tuple (u, "\\tmnote", 1)) && !author)
      doc_data << tree (APPLY, "\\doc-note", u[1]);
    else if (is_tuple (u, "\\preprint", 1))
      doc_data << tree (APPLY, "\\doc-note", concat ("Preprint: ", u[1]));
    else if (is_tuple (u, "\\copyrightholder", 1))
      doc_data << tree (APPLY, "\\doc-note",
                               concat ("Copyright holder: ", u[1]));
    else if (is_tuple (u, "\\copyrightyear", 1))
      doc_data << tree (APPLY, "\\doc-note",
                               concat ("Copyright year: ", u[1]));
    else if (is_tuple (u, "\\tmmisc", 1) && !author)
      doc_data << tree (APPLY, "\\doc-misc", u[1]);
    // authors datas
    else if ((is_tuple (u, "\\thanks", 1) ||
              is_tuple (u, "\\tmnote", 1)) && author)
      author_data << tree (APPLY, "\\author-note", u[1]);
    else if (is_tuple (u, "\\tmmisc", 1) && author)
      author_data << tree (APPLY, "\\author-misc", u[1]);
    else if (is_tuple (u, "\\affiliation", 1))
      author_data << tree (APPLY, "\\author-affiliation", u[1]);
    else if (is_tuple (u, "\\noaffiliation", 0))
      author_data << tree (APPLY, "\\noaffiliation");
    else if (is_tuple (u, "\\altaffiliation", 1) ||
             is_tuple (u, "\\altaffiliation*", 2))
      author_data << tree (APPLY, "\\altaffiliation", u[N(u)-1]);
    else if (is_tuple (u, "\\email", 1) || is_tuple (u, "\\email*", 2))
      author_data << tree (APPLY, "\\author-email", u[N(u)-1]);
    else if (is_tuple (u, "\\homepage", 1) || is_tuple (u, "\\homepage*", 2))
      author_data << tree (APPLY, "\\author-homepage", u[N(u)-1]);
    else if (is_tuple (u, "\\author*", 2)  || is_tuple (u, "\\author", 1)) {
      author= true;
      if (N(author_data) > 1)
        authors << author_data;
      author_data= tree (APPLY, "\\author-data",
                         tree (APPLY, "\\author-name", u[N(u)-1]));
    }
    // abstract datas
    else if (is_tuple (u, "\\keywords", 1)) {
      array<tree> tmp= tokenize_concat (u[N(u)-1], A(concat (",", ";",
              tree (TUPLE, "\\tmsep"), tree (TUPLE, "\\tmSep"))));
      if (N(tmp) > 0) {
        tree kw= tree (APPLY, "\\abstract-keywords");
        kw << tmp;
        abstract_data << kw;
      }
    }
    else if (is_tuple (u, "\\pacs", 1)) {
      array<tree> tmp= tokenize_concat (u[N(u)-1], A(concat (",", ";",
              tree (TUPLE, "\\tmsep"), tree (TUPLE, "\\tmSep"))));
      if (N(tmp) > 0) {
        tree msc= tree (APPLY, "\\abstract-pacs");
        msc << tmp;
        abstract_data << msc;
      }
    }
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract")) {
        if (is_tuple (t[i], "\\tmmsc") || is_tuple (t[i], "\\tmacm") ||
            is_tuple (t[i], "\\tmarxiv"))
          abstract_data << collect_abstract_data (t[i]);
        else
          abstract_text << t[i];
        i++;
      }
      abstract_data << tree (APPLY, "\\abstract", abstract_text);
    }
  }
  if (N(author_data) > 1)
    authors << author_data;
  if (N(authors) > 0) {
    authors= revtex_uncluster_authors (authors, clustered);
    for (int j=0; j<N(authors); j++)
      doc_data << tree (APPLY, "\\doc-author", authors[j]);
    if (clustered)
      doc_data << tree (APPLY, "\\doc-title-options", "cluster-by-affiliation");

  }
  if (N(doc_data) > 1)
    r << doc_data << "\n";
  if (N(abstract_data) > 1)
    r << abstract_data << "\n";
  return r;
}
