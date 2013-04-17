
/******************************************************************************
* MODULE     : metadata-elsevier.cpp
* DESCRIPTION: conversion of (Elsevier) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "metadata.hpp"

static bool
is_elsevier_note_ref (tree t) {
  return is_tuple (t, "\\tnoteref")  || is_tuple (t, "\\fnref") ||
         is_tuple (t, "\\thanksref");
}

#define cenr clean_elsevier_notes_refs

static tree
clean_elsevier_notes_refs (tree t) {
  if (is_atomic (t)) return t;
  tree r (L(t));
  int i, n=N(t);
  for (i=0; i<n; i++) {
    if (!is_elsevier_note_ref (t[i]))
      r << clean_elsevier_notes_refs (t[i]);
  }
  return r;
}

static array<tree>
find_all_tuples (tree t, string s) {
  array<tree> r;
  if (is_atomic (t)) return r;
  int i, n=N(t);
  for (i=0; i<n; i++) {
    if (is_tuple (t[i], s)) r << t[i];
    else r << find_all_tuples (t[i], s);
  }
  return r;
}

static array<string>
get_elsevier_author_refs (tree u) {
  array<string> r;
  array<tree> t;
  if (is_tuple (u, "\\author*", 2))
    r << tokenize (string_arg (u[1]), ",");
  u= u[N(u)-1];
  t = find_all_tuples (u, "\\thanksref");
  for (int i=0; i<N(t); i++) r << string_arg (t[i][1]);
  t = find_all_tuples (u, "\\fnref");
  for (int i=0; i<N(t); i++) r << tokenize (string_arg (t[i][1]), ",");
  return r;
}

static array<tree>
get_elsevier_author_attributes (tree t, array<string> refs) {
  int i, n=N(t);
  array<tree> r;
  for (i=0; i<n; i++)
    if (is_tuple (t[i]) && !is_tuple (t[i], "\\author*") && N(t[i]) > 1 &&
        ends (as_string (t[i][0]), "*") &&
        contains (string_arg (t[i][1]), refs))
       r << t[i];
  return r;
}

static tree
translate_author_metadata_elsevier (tree u) {
  if (!is_tuple (u) || N(u)<2)
    return concat ();
  if (is_tuple (u, "\\address*", 2))
    return tree (APPLY, "\\author-affiliation", cenr (u[2]));
  if (is_tuple (u, "\\thanks*", 2) || is_tuple (u, "\\fntext*", 2))
    return tree (APPLY, "\\author-note", cenr (u[2]));
  if (is_tuple (u, "\\thanksamisc*", 2) || is_tuple (u, "\\fmtext*", 2))
    return tree (APPLY, "\\author-misc", cenr (u[2]));
  if (is_tuple (u, "\\thanksemail*", 2))
    return tree (APPLY, "\\author-email", cenr (u[2]));
  if (is_tuple (u, "\\thankshomepage*", 2))
    return tree (APPLY, "\\author-homepage", cenr (u[2]));
  return concat ();
}

static array<tree>
translate_abstract_data_elsevier (tree t) {
  array<tree> r;
  int i=0, n=N(t);
  tree comp (APPLY, "\\abstract-keywords");
  tree word (CONCAT);
  for (i=0; i<n; i++) {
    if (is_tuple (t[i], "\\PACS")) {
      comp << word;
      if (N(comp) > 1) r << comp;
      word= concat ();
      comp= tree (APPLY, "\\abstract-pacs");
    }
    else if (is_tuple (t[i], "\\MSC")) {
      comp << word;
      if (N(comp) > 1) r << comp;
      word= concat ();
      comp= tree (APPLY, "\\abstract-msc");
    }
    else if (is_tuple (t[i], "\\tmacm") || is_tuple (t[i], "\\tmarxiv"))
          r << collect_abstract_data (t[i]);
    else if (is_tuple (t[i], "\\sep")) {
      comp << word;
      word= concat ();
    }
    else
      word << t[i];
  }
  if (N(word) > 1) comp << word;
  if (N(comp) > 1) r << comp;
  return r;
}

static tree
translate_metadata_elsevier (tree t) {
  int i, n=N(t);
  bool clustered= false;
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract (APPLY, "\\abstract-data");
  tree author (APPLY, "\\author-data");
  array<string> authors_stuff;
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\title", 1))
      doc_data << tree (APPLY, "\\doc-title", cenr (u[1]));
    else if (is_tuple (u, "\\thankssubtitle*", 2) ||
             is_tuple (u, "\\tsubtitletext*", 2))
      doc_data << tree (APPLY, "\\doc-subtitle", cenr (u[2]));
    else if (is_tuple (u, "\\thanksmisc*", 2) ||
             is_tuple (u, "\\tmisctext*", 2))
      doc_data << tree (APPLY, "\\doc-misc", cenr (u[2]));
    else if (is_tuple (u, "\\thanksdate*", 2) ||
             is_tuple (u, "\\tdatetext*", 2))
      doc_data << tree (APPLY, "\\doc-date", cenr (u[2]));
    else if (is_tuple (u, "\\thanks*", 2)
        && !contains (string_arg (u[1]), authors_stuff))
      doc_data << tree (APPLY, "\\doc-note", cenr (u[2]));
    else if (is_tuple (u, "\\tnotetext*", 2))
      doc_data << tree (APPLY, "\\doc-note", cenr (u[2]));
    else if (is_tuple (u, "\\author*", 2) || is_tuple (u, "\\author", 1)) {
      if (N(author) > 1) doc_data << tree (APPLY, "\\doc-author", author);
      if (is_tuple (u, "\\author*", 2)) clustered= true;
      array<string> refs= get_elsevier_author_refs (u);
      array<tree> attrs= get_elsevier_author_attributes (t, refs);
      author= tree (APPLY, "\\author-data",
                    tree (APPLY, "\\author-name", cenr (u[N(u)-1])));
      authors_stuff << refs;
      for (int j=0; j<N(attrs); j++)
        author << translate_author_metadata_elsevier (attrs[j]);
    }
    else if (is_tuple (u, "\\address", 1))
        author << tree (APPLY, "\\author-affiliation", cenr (u[1]));
    else if (is_tuple (u, "\\ead", 1))
        author << tree (APPLY, "\\author-email", cenr (u[1]));
    else if (is_tuple (u, "\\ead*", 2) && string_arg (u[1]) == "url")
        author << tree (APPLY, "\\author-homepage", cenr (u[2]));
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract"))
        abstract_text << t[i++];
      abstract << tree (APPLY, "\\abstract", abstract_text);
    }
    else if (is_tuple (u, "\\begin-keyword")) {
      tree keywords (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-keyword"))
        keywords << t[i++];
      array<tree> abstract_data= translate_abstract_data_elsevier (keywords);
      for (int j=0; j<N(abstract_data); j++)
        abstract << abstract_data[j];
    }
  }
  if (N(author) > 1) doc_data << tree (APPLY, "\\doc-author", author);
  if (clustered) doc_data << tree (APPLY, "\\doc-title-options", "cluster-all");
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract) > 1) r << abstract << "\n";
  return r;
}

tree
collect_metadata_elsevier (tree t) {
  tree r (CONCAT);
  if (is_atomic (t)) return r;
  else {
    int i, n=N(t);
    for (i=0; i<n; i++) {
      if (is_tuple (t[i], "\\begin-frontmatter")) {
        r = concat ();
        for (i++; i<n && !is_tuple (t[i], "\\end-frontmatter"); i++)
          r << t[i];
        return translate_metadata_elsevier (r);
      }
      else {
        r= collect_metadata_elsevier (t[i]);
        if (N(r) > 0) return r;
      }
    }
    return r;
  }
}

#undef cenr
