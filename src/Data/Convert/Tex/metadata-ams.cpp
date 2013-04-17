
/******************************************************************************
* MODULE     : metadata-ams.cpp
* DESCRIPTION: conversion of (AMS) TeX metadata into TeXmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "metadata.hpp"

tree
collect_metadata_ams (tree t) {
  int i, n=N(t);
  bool author= false;
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  tree author_data (APPLY, "\\author-data");
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\title", 1) || is_tuple (u, "\\title*", 2)) {
      doc_data << tree (APPLY, "\\doc-title", u[N(u)-1]);
      author= false;
    }
    // doc datas
    else if (is_tuple (u, "\\tmsubtitle", 1))
      doc_data << tree (APPLY, "\\doc-subtitle", u[1]);
    else if (is_tuple (u, "\\date", 1))
      doc_data << tree (APPLY, "\\doc-date", u[1]);
    else if ((is_tuple (u, "\\thanks", 1) ||
              is_tuple (u, "\\tmnote", 1)) && !author)
      doc_data << tree (APPLY, "\\doc-note", u[1]);
    else if (is_tuple (u, "\\dedicatory", 1))
      doc_data << tree (APPLY, "\\doc-note", concat ("Dedicatory: ", u[1]));
    else if (is_tuple (u, "\\tmmisc", 1) && !author)
      doc_data << tree (APPLY, "\\doc-misc", u[1]);
    // authors datas
    else if ((is_tuple (u, "\\thanks", 1) ||
              is_tuple (u, "\\tmnote", 1)) && author)
      author_data << tree (APPLY, "\\author-note", u[1]);
    else if (is_tuple (u, "\\tmmisc", 1) && author)
      author_data << tree (APPLY, "\\author-misc", u[1]);
    else if (is_tuple (u, "\\address", 1))
      author_data << tree (APPLY, "\\author-affiliation", u[1]);
    else if (is_tuple (u, "\\curaddr", 1))
      author_data << tree (APPLY, "\\author-affiliation",
          concat ("Current address:", u[1]));
    else if (is_tuple (u, "\\email", 1))
      author_data << tree (APPLY, "\\author-email", u[1]);
    else if (is_tuple (u, "\\urladdr", 1))
      author_data << tree (APPLY, "\\author-homepage", u[1]);
    else if (is_tuple (u, "\\author*", 2)  || is_tuple (u, "\\author", 1)  ||
             is_tuple (u, "\\contrib*", 2) || is_tuple (u, "\\contrib", 1)) {
      author= true;
      if (N(author_data) > 1)
        doc_data << tree (APPLY, "\\doc-author", author_data);
      author_data= tree (APPLY, "\\author-data",
                         tree (APPLY, "\\author-name", u[N(u)-1]));
      if (is_tuple (u, "\\contrib*") || is_tuple (u, "\\contrib"))
        author_data << tree (APPLY, "\\author-note", "(Contributor)");
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
    else if (is_tuple (u, "\\subjclass", 1)   ||
             is_tuple (u, "\\subjclass*", 2)) {
      array<tree> tmp= tokenize_concat (u[N(u)-1], A(concat (",", ";",
              tree (TUPLE, "\\tmsep"), tree (TUPLE, "\\tmSep"))));
      if (N(tmp) > 0) {
        tree msc= tree (APPLY, "\\abstract-msc");
        msc << tmp;
        abstract_data << msc;
      }
    }
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract")) {
        if (is_tuple (t[i], "\\tmacm") || is_tuple (t[i], "\\tmpacs") ||
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
    doc_data << tree (APPLY, "\\doc-author", author_data);
  if (N(doc_data) > 1)
    r << doc_data << "\n";
  if (N(abstract_data) > 1)
    r << abstract_data << "\n";
  return r;
}
