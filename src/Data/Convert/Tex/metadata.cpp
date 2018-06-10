
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
#include "metadata.hpp"

array<tree>
tokenize_concat (tree t, array<tree> a, bool keep) {
  if (!is_concat (t)) return array<tree> ();
  tree tmp (CONCAT);
  array<tree> r;
  for (int i=0; i<N(t); i++) {
    if (!contains (t[i], a))
      tmp << t[i];
    else {
      if (keep || tmp != concat()) r << tmp;
      tmp= concat();
    }
  }
  if (keep || tmp != concat()) r << tmp;
  return r;
}

bool
is_metadata_env (tree u) {
  return is_tuple (u, "\\begin-frontmatter")  ||
         is_tuple (u, "\\begin-IEEEkeywords") ||
         is_tuple (u, "\\begin-keywords")     ||
         is_tuple (u, "\\begin-abstract");
}

bool
is_metadata (tree u) {
  return is_tuple (u, "\\accepted")          ||
         is_tuple (u, "\\address")           ||
         is_tuple (u, "\\additionalauthors") ||
         is_tuple (u, "\\addtocmark")        ||
         is_tuple (u, "\\addtocmark*")       ||
         is_tuple (u, "\\affiliation")       ||
         is_tuple (u, "\\altaffiliation")    ||
         is_tuple (u, "\\altaffiliation*")   ||
         is_tuple (u, "\\author")            ||
         is_tuple (u, "\\authornote")        ||
         is_tuple (u, "\\authorrunning")     ||
         is_tuple (u, "\\author*")           ||
         is_tuple (u, "\\category")          ||
         is_tuple (u, "\\category*")         ||
         is_tuple (u, "\\ccsdesc")           ||
         is_tuple (u, "\\ccsdesc*")          ||
         is_tuple (u, "\\CCSXML")            ||
         is_tuple (u, "\\classification")    ||
         is_tuple (u, "\\contrib")           ||
         is_tuple (u, "\\contrib*")          ||
         is_tuple (u, "\\copyrightholder")   ||
         is_tuple (u, "\\copyrightyear")     ||
         is_tuple (u, "\\curaddr")           ||
         is_tuple (u, "\\date")              ||
         is_tuple (u, "\\dedicatory")        ||
         is_tuple (u, "\\doc-acm")           ||
         is_tuple (u, "\\doc-terms")         ||
         is_tuple (u, "\\email")             ||
         is_tuple (u, "\\email*")            ||
         is_tuple (u, "\\homepage")          ||
         is_tuple (u, "\\homepage*")         ||
         is_tuple (u, "\\institute")         ||
         is_tuple (u, "\\footnotetext")      ||
         is_tuple (u, "\\footnotetext*")     ||
         is_tuple (u, "\\keywords")          ||
         is_tuple (u, "\\maketitle")         ||
         is_tuple (u, "\\noaffiliation")     ||
         is_tuple (u, "\\numberofauthors")   ||
         is_tuple (u, "\\orcid")             ||
         is_tuple (u, "\\pacs")              ||
         is_tuple (u, "\\preprint")          ||
         is_tuple (u, "\\pagenumbering")     ||
         is_tuple (u, "\\received")          ||
         is_tuple (u, "\\revised")           ||
         is_tuple (u, "\\subclass")          ||
         is_tuple (u, "\\subjclass")         ||
         is_tuple (u, "\\subjclass*")        ||
         is_tuple (u, "\\subtitle")          ||
         is_tuple (u, "\\subtitlenote")      ||
         is_tuple (u, "\\terms")             ||
         is_tuple (u, "\\title")             ||
         is_tuple (u, "\\titlenote")         ||
         is_tuple (u, "\\titlerunning")      ||
         is_tuple (u, "\\title*")            ||
         is_tuple (u, "\\thanks")            ||
         is_tuple (u, "\\thanks*")           ||
         is_tuple (u, "\\tmacm")             ||
         is_tuple (u, "\\tmaffiliation")     ||
         is_tuple (u, "\\tmarxiv")           ||
         is_tuple (u, "\\tmemail")           ||
         is_tuple (u, "\\tmfnaffiliation")   ||
         is_tuple (u, "\\tmfnemail")         ||
         is_tuple (u, "\\tmfnhomepage")      ||
         is_tuple (u, "\\tmhomepage")        ||
         is_tuple (u, "\\tmkeywords")        ||
         is_tuple (u, "\\tmmisc")            ||
         is_tuple (u, "\\tmmsc")             ||
         is_tuple (u, "\\tmnote")            ||
         is_tuple (u, "\\tmpacs")            ||
         is_tuple (u, "\\tmsep")             ||
         is_tuple (u, "\\tmSep")             ||
         is_tuple (u, "\\tmsubtitle")        ||
         is_tuple (u, "\\toctitle")          ||
         is_tuple (u, "\\tocauthor")         ||
         is_tuple (u, "\\translator")        ||
         is_tuple (u, "\\urladdr");
}

#define cltm clean_latex_title_markup

static bool
is_latex_titlenote (tree t) {
  return is_tuple (t, "\\thanks", 1) || is_tuple (t, "\\tmnote", 1);
}

static tree
clean_latex_title_markup (tree t) {
  if (is_atomic (t)) return t;
  if (is_latex_titlenote (t)      ||
      is_tuple (t, "\\tmmisc", 1) || is_tuple (t, "\\tmsubtitle", 1))
    return concat();
  if (is_tuple (t, "\\tmSep")) return concat ("\n");
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
    r << tree (APPLY, "\\doc-note", t[1]);
    return;
  }
  if (is_tuple (t, "\\tmmisc", 1)) {
    r << tree (APPLY, "\\doc-misc", t[1]);
    return;
  }
  if (is_tuple (t, "\\tmsubtitle", 1)) {
    r << tree (APPLY, "\\doc-subtitle", t[1]);
    return;
  }
  int i, n=N(t);
  for (i=0; i<n; i++)
    get_latex_title_notes (t[i], r);
}

tree
collect_abstract_data (tree u) {
  if (!is_tuple (u) || N(u) < 1) return u;
  string s= as_string (u[0]);
  s= "\\abstract-" * s(3, N(s));
  tree r (APPLY, s);
  for (int i=1; i<N(u); i++)
    if (!is_tuple (u[i], "\\tmsep"))
      r << u[i];
  return r;
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
    else if (is_tuple (u, "\\thanks", 1))
      author_data << tree (APPLY, "\\author-misc", u[1]);
    else if (is_tuple (u, "\\tmaffiliation", 1) ||
             is_tuple (u, "\\tmfnaffiliation", 1))
      author_data << tree (APPLY, "\\author-affiliation", cltm (u[1]));
    else if (is_tuple (u, "\\tmmisc", 1))
      author_data << tree (APPLY, "\\author-misc", cltm (u[1]));
    else if (is_tuple (u, "\\tmnote", 1))
      author_data << tree (APPLY, "\\author-note", cltm (u[1]));
    else if (is_tuple (u, "\\tmemail", 1) ||
             is_tuple (u, "\\tmfnemail", 1))
      author_data << tree (APPLY, "\\author-email", cltm (u[1]));
    else if (is_tuple (u, "\\tmhomepage", 1) ||
             is_tuple (u, "\\tmfnhomepage", 1))
      author_data << tree (APPLY, "\\author-homepage", cltm (u[1]));
    else
      author_name << u;
  }
  return r;
}

array<tree>
collect_metadata_latex (tree t, array<tree>(*get_author_datas)(tree)) {
  array<tree> r;
  tree doc_data (APPLY, "\\doc-data");
  tree abstract_data (APPLY, "\\abstract-data");
  array<tree> doc_notes;
  bool dated= false, maketitle= false;
  int i, n=N(t);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_tuple (u, "\\maketitle")) maketitle= true;
    else if (is_tuple (u, "\\title", 1)  || is_tuple (u, "\\title*", 2)) {
      get_latex_title_notes (u[N(u)-1], doc_notes);
      doc_data << tuple ("\\doc-title", cltm (u[N(u)-1]));
    }
    else if (is_tuple (u, "\\author", 1)  || is_tuple (u, "\\author*", 2)) {
      array<tree> author_datas= (*get_author_datas) (u[1]);
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
    else if (is_tuple (u, "\\tmkeywords") || is_tuple (u, "\\tmmsc")  ||
             is_tuple (u, "\\tmacm")      || is_tuple (u, "\\tmpacs") ||
             is_tuple (u, "\\tmarxiv"))
      abstract_data << collect_abstract_data (u);
  }
  if (!dated && maketitle)
    doc_data << tuple ("\\doc-date", tree (APPLY, "\\date", ""));
  if (N(doc_notes) > 0) doc_data << doc_notes;
  if (N(doc_data) > 1) {
    r << doc_data;
    r << concat ("\n");
  }
  if (N(abstract_data) > 1) {
    r << abstract_data;
    r << concat ("\n");
  }
  return r;
}

array<tree>
collect_metadata_latex (tree t) {
  return collect_metadata_latex (t, &get_latex_author_datas);
}

#undef cltm

static bool
is_space (tree t) {
  return t == " " || t == concat (" ");
}

tree
filter_spaces (tree t, bool &spaced) {
  if (is_space (t) && spaced)  return concat();
  if (is_space (t) && !spaced) {
    spaced= true;
    return t;
  }
  spaced= false;
  if (is_atomic (t)) return t;
  tree r (L(t));
  int i, n=N(t);
  if (is_apply (t) || is_tuple (t)) {
    // then arity shouldn't vary
    for (i=0; i<n; i++)
      r << filter_spaces (t[i], spaced);
    return r;
  }
  for (i=0; i<n; i++) {
    if (t[i] == concat() || t[i] == "") continue;
    if (!is_space (t[i]) || !spaced) {
      r << filter_spaces (t[i], spaced);
      if (is_space (t[i])) spaced= true;
      else spaced= false;
    }
  }
  n= N(r);
  if (n>0 && is_space (r[n-1])) r[n-1]= concat();
  return r;
}

array<tree>
filter_spaces (array<tree> a, bool &spaced) {
  for (int i=0; i<N(a); i++)
    a[i]= filter_spaces (a[i], spaced);
  return a;
}

static bool
need_tokenize (tree t, array<tree> sep) {
  if (!(is_apply (t) && N(t) == 2 && is_concat (t[1]))) return false;
  for (int i=0; i<N(t[1]); i++)
    if (contains (t[1][i], sep)) return true;
  return false;
}

static array<tree>
unconcat_tmseps (tree t) {
  if (is_atomic (t)) return A(concat (t));
  array<tree> sep= A(concat (tuple ("\\tmsep"), tuple ("\\tmSep")));
  if (need_tokenize (t, sep)) {
    array<tree> tmp= tokenize_concat (t[1], sep);
    for (int i=0; i<N(tmp); i++)
      tmp[i]= tree (L(t), t[0], tmp[i]);
    return tmp;
  }
  tree r(L(t));
  for (int i=0; i<N(t); i++) {
    r << unconcat_tmseps (t[i]);
  }
  return A(concat (r));
}

tree
collect_metadata (tree t, tree latex_class) {
  tree r(CONCAT);
  bool spaced;
  string s = "article";
  if (is_tuple (latex_class, "\\documentclass", 1) ||
      is_tuple (latex_class, "\\documentstyle", 1))
    s = latex_verbarg_to_string (latex_class[1]);
  else if (is_tuple (latex_class, "\\documentclass*", 2) ||
           is_tuple (latex_class, "\\documentstyle*", 2))
    s = latex_verbarg_to_string (latex_class[2]);

  if (s == "acm_proc_article-sp" ||
      s == "sig-alternate" || s == "sig-alt-full")
    r= collect_metadata_acm_old (t);
  else if (s == "acmart" || s == "acmsmall" || s == "acmlarge" ||
           s == "acmtog" || s == "sigconf" || s == "sigchi" || s == "sigplan")
    r= collect_metadata_acm (t);
  else if (s == "elsarticle" || s == "elsart" || s == "ifacconf")
    r= collect_metadata_elsevier (t);
  else if (s == "amsart" || s == "amsbook" || s == "amsproc")
    r= collect_metadata_ams (t);
  else if (s == "revtex4-1")
    r= collect_metadata_revtex (t);
  else if (s == "svmono")
    r= collect_metadata_svmono (t);
  else if (s == "svjour3")
    r= collect_metadata_springer (t, false);
  else if (s == "llncs")
    r= collect_metadata_springer (t, true);
  else if (s == "IEEEconf" || s == "IEEEtran")
    r= collect_metadata_ieee (t);
  else
    r << collect_metadata_latex (t);
  r=  unconcat_tmseps (r);
  r= filter_spaces (r, spaced);
  return r;
}

string
get_latex_style (tree t) {
  if (N(t) != 3 && N(t) != 2) return "";
  string s= trim_spaces (string_arg (t[N(t)-1]));
  string opt= N(t)==3? trim_spaces (string_arg (t[1])): string ("");
  array<string> opts= trim_spaces (tokenize (opt, ","));
  if (N(t) == 3 && occurs ("acmart", s)) {
    if (occurs ("acmsmall", opt)) return "acmsmall";
    if (occurs ("acmlarge", opt)) return "acmlarge";
    if (occurs ("acmtog", opt)) return "acmtog";
    if (occurs ("sigconf", opt)) return "sigconf";
    if (occurs ("sigchi", opt)) return "sigchi";
    if (occurs ("sigplan", opt)) return "sigplan";
  }
  if (N(t) == 3 && occurs ("revtex", s)) {
    if (contains (string ("aip"), opts)) return "aip";
    if (contains (string ("aps"), opts)) return "aps";
  }
  if (occurs ("llncs", s))
    return "llncs";
  if (occurs ("svjour", s))
    return "svjour";
  if (occurs ("ifacconf", s))
    return "ifac";
  if (occurs ("IEEEconf", s))
    return "ieeeconf";
  if (occurs ("IEEEtran", s))
    return "ieeetran";
  if (occurs ("acm_proc", s))
    return "acmconf";
  if (occurs ("sig-alt", s))
    return "sig-alternate";
  return s;
}
