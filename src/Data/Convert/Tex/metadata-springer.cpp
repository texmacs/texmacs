
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
  return is_tuple (t, "\\thanks", 1) || is_tuple (t, "\\tmnote", 1);
}

static bool
is_line_break (tree t) {
  return is_tuple (t, "\\\\") || is_tuple (t, "\\\\*");
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

static array<tree>
set_minus (array<tree> a, array<tree> b) {
  array<tree> r;
  if (N(b) == 0) return a;
  for (int i=0; i<N(a); i++)
    if (!contains (a[i], b))
      r << a[i];
  return r;
}

static tree
get_author_name (tree t) {
  for (int i=0; i<N(t); i++)
    if (is_apply (t[i], "\\author-name"))
      return t[i];
  return concat();
}

static array<tree>
get_authors_by_name (array<tree> a, tree t) {
  array<tree> r;
  for (int i=0; i<N(a); i++)
  {
    if (contains (t, A(a[i])))
      r << a[i];
  }
  return r;
}

static tree
add_llncs_author_datas (tree author, array<tree> author_affs) {
  if (!is_apply (author) || N(author) < 2) return concat ();
  for (int i=0; i<N(author_affs); i++) {
    for (int j=1; j<N(author_affs[i]); j++)
      author << author_affs[i][j];
  }
  return author;
}

static array<tree>
merge_llncs_author_datas (array<tree> authors, array<tree> affs) {
  bool simple= true;
  for (int i=0; i<N(authors); i++)
    for (int j=1; j<N(authors[i]); j++)
      if (is_apply (authors[i][j], "\\author-inst", 1))
        simple= false;

  array<tree> r;
  for (int i=0; i<N(authors); i++) {
    if (simple)
      r << add_llncs_author_datas (authors[i], affs);
    else {
      array<tree> author_affs;
      for (int j=1; j<N(authors[i]); j++) {
        if (is_apply (authors[i][j], "\\author-inst", 1)) {
          int n= as_int (as_string (authors[i][j][1]));
          if (n>0 && n<=N(affs))
            author_affs << affs[n-1];
        }
      }
      r << add_llncs_author_datas (authors[i], author_affs);
    }
  }
  return r;
}

static array<tree>
merge_springer_author_datas (array<tree> a) {
  if (N(a) < 2) return a;
  tree name(CONCAT);
  int i=0;
  while (i<N(a) && name == concat())
    name= get_author_name (a[i++]);
  if (i == N(a)) return a;
  array<tree> same=  get_authors_by_name (a, name);
  array<tree> others= set_minus (a, same);
  tree datas= same[0];
  for (int i=1; i<N(same); i++)
    for (int j=0; j<N(same[i]); j++)
      if (!contains (same[i][j], A(datas)))
        datas << same[i][j];
  array<tree> r;
  r << datas;
  if (N(others) == 1) r << others;
  if (N(others) > 1) r << merge_springer_author_datas (others);
  return r;
}

#define cstm clean_springer_title_markup

static tree
clean_springer_title_markup (tree t) {
  if (is_atomic (t)) return t;
  if (is_springer_titlenote (t) ||
      is_tuple (t, "\\tmmisc", 1))
    return concat();
  tree r (L(t));
  int i, n=N(t);
  for (i=0; i<n; i++)
      r << clean_springer_title_markup (t[i]);
  return r;
}

static array<tree>
get_springer_author_datas (tree t, string s, bool llncs=false) {
  s= "\\author-" * s;
  int i, n=N(t);
  bool line_break= true;
  array<tree> r;
  tree u;
  tree author_data (APPLY, "\\author-data");
  tree author_name (CONCAT);
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\and")) {
      line_break= false;
      if (N(author_name) > 1) {
        tree tmp= concat();
        for (int j=0; j<N(author_name); j++)
          if (j+1 < N(author_name) || !is_line_break (author_name[j]))
            tmp << author_name[j];
        if (N(tmp) > 1)
          author_data << tree (APPLY, s, tmp);
        author_name= tree (CONCAT);
      }
      if (N(author_data) > 1) {
        r << author_data;
        author_data= tree (APPLY, "\\author-data");
      }
      line_break= true;
    }
    else if (is_springer_titlenote (u))
      author_data << tree (APPLY, "\\author-note", u[1]);
    else if (llncs && is_tuple (u, "\\inst", 1))
      author_data << tree (APPLY, "\\author-inst", string_arg (u[1]));
    else if (is_tuple (u, "\\email", 1))
      author_data << tree (APPLY, "\\author-email", u[1]);
    else if (is_tuple (u, "\\tmhomepage", 1))
      author_data << tree (APPLY, "\\author-homepage", u[1]);
    else if (is_tuple (u, "\\tmmisc", 1))
      author_data << tree (APPLY, "\\author-misc", u[1]);
    else if (!line_break || !is_line_break (u)) {
      if (!line_break || (u != " " && u != concat (" ") && u != concat ()))
        author_name << u;
      if (is_line_break (u)) line_break= true;
      else if (u != " " && u != concat (" ") && u != concat ())
        line_break= false;
    }
  }

  if (N(author_name) > 1) {
    tree tmp= concat();
    for (int j=0; j<N(author_name); j++)
      if (j+1 < N(author_name) || !is_line_break (author_name[j]))
        tmp << author_name[j];
    if (N(tmp) > 1)
      author_data << tree (APPLY, s, tmp);
    author_name= tree (CONCAT);
  }
  if (N(author_data) > 1) {
    r << author_data;
    author_data= tree (APPLY, "\\author-data");
  }
  return r;
}

static array<tree>
get_llncs_affiliation_datas (tree t, bool llncs=false) {
  (void) llncs;
  return get_springer_author_datas (t, "affiliation");
}

static array<tree>
get_springer_affiliation_datas (tree t, bool llncs=false) {
  (void) llncs;
  int i, n=N(t);
  array<tree> r, author_datas, author_affiliation;
  tree tmp (CONCAT);
  for (i=0; i<n; i++) {
    while (i<n && !is_tuple (t[i], "\\at")) tmp << t[i++];
    author_datas= get_springer_author_datas (tmp, "name");
    tmp= concat ();
    if (is_tuple (t[i], "\\at")) {
      i++;
      while (i<n && !is_tuple (t[i], "\\and")) tmp << t[i++];
      author_affiliation= get_springer_author_datas (tmp, "affiliation");
      tmp= concat ();
      for (int j=0; j<N(author_datas); j++)
        for (int k=0; k<N(author_affiliation); k++)
          for (int l=1; l<N(author_affiliation[k]); l++)
            author_datas[j]= (author_datas[j] << author_affiliation[k][l]);
      r << author_datas;
      author_datas= array<tree>();
      author_affiliation= array<tree>();
    }
  }
  return r;
}

static void
translate_springer_abstract_data (tree u, string s, tree &abstract_data) {
  if (N(u) < 2) return;
  s= "\\abstract-" * s;
  array<tree> tmp= tokenize_concat (u[N(u)-1], 
      A(concat (tree (TUPLE, "\\and"), tree (TUPLE, "\\tmsep"))));
  if (N(tmp) > 0) {
    tree t= tree (APPLY, s);
    t << tmp;
    abstract_data << t;
  }
}

tree
collect_metadata_springer (tree t, bool llncs) {
  int i, n=N(t);
  tree u, r (CONCAT);
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes, author_datas, author_affs;
  for (i=0; i<n; i++) {
    u= t[i];
    if (is_tuple (u, "\\dedication", 1))
      doc_notes << tree (APPLY, "\\doc-note", concat ("Dedication: ", u[1]));
    else if (is_tuple (u, "\\date", 1))
      doc_data << tree (APPLY, "\\doc-date", u[1]);
    else if (is_tuple (u, "\\title", 1)) {
      get_springer_title_notes (u[1], doc_notes);
      doc_data << tree (APPLY, "\\doc-title", cstm (u[1]));
    }
    else if (is_tuple (u, "\\subtitle", 1)) {
      get_springer_title_notes (u[1], doc_notes);
      doc_data << tree (APPLY, "\\doc-subtitle", cstm (u[1]));
    }
    else if (is_tuple (u, "\\author", 1))
      author_datas << get_springer_author_datas (u[1], "name", llncs);
    else if (!llncs && is_tuple (u, "\\institute", 1))
      author_affs << get_springer_affiliation_datas (u[1]);
    else if (llncs && is_tuple (u, "\\institute", 1))
      author_affs << get_llncs_affiliation_datas (u[1]);
    else if (is_tuple (u, "\\begin-abstract")) {
      tree abstract_text (CONCAT);
      i++;
      while (i<n && !is_tuple (t[i], "\\end-abstract")) {
        u= t[i];
        if (is_tuple (u, "\\keywords", 1))
          translate_springer_abstract_data (u, "keywords", abstract_data);
        else if (is_tuple (u, "\\tmmsc") || is_tuple (u, "\\tmarxiv") ||
                 is_tuple (u, "\\tmacm") || is_tuple (u, "\\tmpacs"))
          abstract_data << collect_abstract_data (u);
        else
          abstract_text << u;
        i++;
      }
      abstract_data << tree (APPLY, "\\abstract", abstract_text);
    }
    else if (is_tuple (u, "\\keywords", 1))
      translate_springer_abstract_data (u, "keywords", abstract_data);
    else if (is_tuple (u, "\\subclass", 1))
      translate_springer_abstract_data (u, "msc", abstract_data);
    else if (is_tuple (u, "\\PACS", 1))
      translate_springer_abstract_data (u, "pacs", abstract_data);
    else if (is_tuple (u, "\\CRclass", 1))
      translate_springer_abstract_data (u, "acm", abstract_data);
    else if (is_tuple (u, "\\tmarxiv"))
      abstract_data << collect_abstract_data (u);
  }
  bool spaced= false;
  if (llncs) {
    author_datas= merge_llncs_author_datas (author_datas, author_affs);
  }
  else {
    author_datas << author_affs;
    author_datas= filter_spaces (author_datas, spaced);
    author_datas= merge_springer_author_datas (author_datas);
  }
  for (int j=0; j<N(author_datas); j++)
    author_datas[j]= tree (APPLY, "\\doc-author", author_datas[j]);
  if (N(author_datas) > 0) doc_data << author_datas;
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) r << doc_data << "\n";
  if (N(abstract_data) > 1) r << abstract_data << "\n";
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
