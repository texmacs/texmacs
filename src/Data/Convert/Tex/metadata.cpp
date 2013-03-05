
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
  return is_tuple (u, "\\begin-frontmatter");
}

bool
is_metadata (tree u) {
  return is_tuple (u, "\\address")         ||
         is_tuple (u, "\\affiliation")     ||
         is_tuple (u, "\\author")          ||
         is_tuple (u, "\\author*")         ||
         is_tuple (u, "\\category")        ||
         is_tuple (u, "\\category*")       ||
         is_tuple (u, "\\classification")  ||
         is_tuple (u, "\\conferenceinfo")  ||
         is_tuple (u, "\\CopyrightYear")   ||
         is_tuple (u, "\\date")            ||
         is_tuple (u, "\\doc-acm")         ||
         is_tuple (u, "\\doc-terms")       ||
         is_tuple (u, "\\email")           ||
         is_tuple (u, "\\footnotetext")    ||
         is_tuple (u, "\\footnotetext*")   ||
         is_tuple (u, "\\keywords")        ||
         is_tuple (u, "\\numberofauthors") ||
         is_tuple (u, "\\pagenumbering")   ||
         is_tuple (u, "\\subjclass")       ||
         is_tuple (u, "\\subjclass*")      ||
         is_tuple (u, "\\subtitle")        ||
         is_tuple (u, "\\terms")           ||
         is_tuple (u, "\\title")           ||
         is_tuple (u, "\\title*")          ||
         is_tuple (u, "\\urladdr");
}

static tree
collect_metadata_acm (tree t) {
  int i, n=N(t);
  tree r (CONCAT);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_tuple (u, "\\title", 1) || is_tuple (u, "\\subtitle", 1)) {
      tree v (CONCAT), w= u[1], titlenote;
      for (int j=0; j<N(w); j++) {
        if (is_tuple (w[j], "\\titlenote", 1)     ||
            is_tuple (w[j], "\\thanks", 1)) {
          titlenote= copy (w[j]);
          if (u[0] == "\\title")
            titlenote[0]= "\\title-thanks";
          else if (u[0] == "\\subtitle")
            titlenote[0]= "\\doc-subtitle-note";
        }
        else
          v << w[j];
      }
      if (u[0] == "\\title")
        r << tuple ("\\title", v);
      else if (u[0] == "\\subtitle")
        r << tuple ("\\subtitle", v);
      if (is_tuple (titlenote, "\\title-thanks") ||
          is_tuple (titlenote, "\\doc-subtitle-note"))
        r << titlenote;
    }
    else if (is_tuple (u, "\\author", 1)) {
      tree v (CONCAT), w= u[1], a= tuple ("\\author");
      array<tree> l;
      for (int j=0; j<N(w); j++) {
        if (is_tuple (w[j], "\\titlenote", 1) ||
            is_tuple (w[j], "\\thanks", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\title-thanks";
          l << x;
        }
        else if (is_tuple (w[j], "\\affaddr", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\address";
          l << x;
        }
        else if (is_tuple (w[j], "\\email", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\title-email";
          l << x;
        }
        else if (is_tuple (w[j], "\\alignauthor") ||
                 is_tuple (w[j], "\\and")) {
          if (N(v) > 0)
            a << v;
          v= concat ();
          if (is_tuple (a, "\\author", 1))
            r << a;
          a= a= tuple ("\\author");
          for (int j=0; j<N(l); j++)
            r << l[j];
          l= array<tree> ();
        }
        else if (is_tuple (w[j], "\\\\"));
        else
          v << w[j];
      }
      if (N(v) > 0)
        a << v;
      if (is_tuple (a, "\\author", 1))
        r << a;
      for (int j=0; j<N(l); j++)
        r << l[j];
    }
    else if (is_tuple (u, "\\footnotetext", 1) ||
             is_tuple (u, "\\footnotetext*", 2)) {
      tree v= tuple (u[0], u[N(u)-1]);
      v[0] = "\\title-thanks";
      r << v;
    }
    else if (is_tuple (u, "\\keywords"))
      r << u;
    else if (is_tuple (u, "\\category") || is_tuple (u, "\\category*")) {
      tree v= copy (u);
      v[0]= "\\doc-acm";
      r << v;
    }
    else if (is_tuple (u, "\\conferenceinfo")) {
      tree v= copy (u);
      v[0]= "\\doc-conference";
      r << v;
    }
    else if (is_tuple (u, "\\terms")) {
      tree v= copy (u);
      v[0]= "\\doc-terms";
      r << v;
    }
  }
  return r;
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
  return concat ();
}

static array<tree>
translate_abstract_data_elsevier (tree t) {
  array<tree> r;
  int i=0, n=N(t);
  tree kw  (APPLY, "\\abstract-keywords");
  tree msc (APPLY, "\\abstract-msc");
  tree tmp (CONCAT);
  while (i<n && !is_tuple (t[i], "\\PACS")) {
    while (i<n && !is_tuple (t[i], "\\sep") && !is_tuple (t[i], "\\PACS"))
      tmp << t[i++];
    kw << tmp;
    tmp= concat ();
    if (!is_tuple (t[i], "\\PACS")) i++;
  }
  if (is_tuple (t[i], "\\PACS")) {
    i++;
    while (i<n) {
      while (i<n && !is_tuple (t[i], "\\sep")) tmp << t[i++];
      msc << tmp;
      tmp= concat ();
      i++;
    }
  }
  if (N(kw)>1)  r << kw;
  if (N(msc)>1) r << msc;
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

static tree
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
