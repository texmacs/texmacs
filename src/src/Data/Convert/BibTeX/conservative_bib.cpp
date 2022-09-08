
/******************************************************************************
* MODULE     : conservative_bib.cpp
* DESCRIPTION: conservative conversions with bibtex
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "wencoding.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "iterator.hpp"

/******************************************************************************
* Break bibtex files into individual chunks, alternating comments and entries
******************************************************************************/

static bool
bib_skip (string s, int& i) {
  int n= N(s);
  while (i < n) {
    while (i < n && s[i] != '\n') i++;
    if (i < n) i++;
    int j= i;
    while (j < n && (s[j] == ' ' || s[j] == '\t')) j++;
    if (j >= n || s[j] != '@') continue;
    j++;
    int k= j;
    while (j < n && is_alpha (s[j])) j++;
    string tp= locase_all (s (k, j));
    bool ok=
      (tp == "article") ||
      (tp == "book") ||
      (tp == "booklet") ||
      (tp == "inbook") ||
      (tp == "incollection") ||
      (tp == "inproceedings") ||
      (tp == "conference") ||
      (tp == "manual") ||
      (tp == "mastersthesis") ||
      (tp == "misc") ||
      (tp == "phdthesis") ||
      (tp == "proceedings") ||
      (tp == "techreport") ||
      (tp == "unpublished");
    if (ok) return true;
    bool other=
      (tp == "comment") ||
      (tp == "string") ||
      (tp == "preamble");
    if (other) return false;
  }
  return false;
}

static void
bib_rewind (string s, int& i) {
  while (true) {
    int save= i;
    if (i > 0) i--;
    while (i > 0 && s[i-1] != '\n') i--;
    for (int j=i; j<save; j++)
      if (s[j] == '%' || s[j] == '\n') break;
      else if (s[j] == ' ' || s[j] == '\t');
      else { i= save; return; }
  }
}

static bool
immediate_start (string s) {
  int i=0, n=N(s);
  while (i<n && (s[i] == ' ' || s[i] == '\t')) i++;
  return i<n && s[i] == '@';
}

static array<string>
bib_break (string s) {
  int i=0, n=N(s), start=0;
  array<string> a;
  while (i<n) {
    start= i;
    if (i>0 || !immediate_start (s)) {
      while (true) {
        bool ok= bib_skip (s, i);
        if (ok || i >= n) break;
      }
      if (i >= n) break;
    }
    a << s (start, i);
    //cout << "---------------------------------------------------------\n";
    //cout << s (start, i);
    start= i;
    (void) bib_skip (s, i);
    bib_rewind (s, i);
    ASSERT (i > start, "strange entry in bib_break");
    a << s (start, i);
    //cout << "---------------------------------------------------------\n";
    //cout << s (start, i);
    start= i;
  }
  a << s (start, n);
  //cout << "---------------------------------------------------------\n";
  //cout << s (start, n);
  //cout << "---------------------------------------------------------\n";
  return a;
}

static string
bib_get_type (string s) {
  int i=0, n=N(s);
  while (i<n && s[i] != '@') i++;
  if (i<n) i++;
  while (i<n && is_space (s[i])) i++;
  int start= i;
  while (i<n && is_alpha (s[i])) i++;
  return locase_all (s (start, i));
}

static string
bib_get_key (string s) {
  int i=0, n=N(s);
  while (i<n && s[i] != '{') i++;
  if (i<n) i++;
  while (i<n && is_space (s[i])) i++;
  int start= i;
  while (i<n && s[i] != ',' && !is_space (s[i])) i++;
  return s (start, i);
}

static hashmap<string,int>
bib_indices (array<string> a) {
  int i, n= N(a);
  hashmap<string,int> h (-1);
  for (i=1; i<n; i+=2) {
    string key= bib_get_key (a[i]);
    h (key)= i;
  }
  return h;
}

/******************************************************************************
* Managing special bib primitives and preambles
******************************************************************************/

static bool
bib_is_special (string s) {
  string type= bib_get_type (s);
  return type == "" || type == "string" || type == "preamble";
}

static bool
bib_check_standard_preamble (array<string> a) {
  int i;
  for (i=1; i<N(a); i+=2)
    if (!bib_is_special (a[i]) && bib_get_type (a[i]) != "comment") break;
  for (; i<N(a); i+=2)
    if (bib_is_special (a[i])) return false;
  return true;
}

/******************************************************************************
* Find bibliographic entries in a TeXmacs file
******************************************************************************/

static bool
is_db_entry (tree t) {
  if (N(t) != 5 || !is_atomic (t[2])) return false;
  return
    is_compound (t, "db-entry") ||
    is_compound (t, "db-folded-entry") ||
    is_compound (t, "db-pretty-entry");
}

static void
bib_collect_entries (hashmap<string,tree>& h, tree t) {
  if (is_atomic (t));
  else if (is_db_entry (t)) h (t[2]->label)= t;
  else {
    int i, n=N(t);
    for (i=0; i<n; i++)
      bib_collect_entries (h, t[i]);
  }
}

static hashmap<string,tree>
bib_collect_entries (tree t) {
  hashmap<string,tree> h (UNINIT);
  bib_collect_entries (h, t);
  return h;
}

/******************************************************************************
* Equivalence of bibliographic entries
******************************************************************************/

static bool
children_included (tree t1, tree t2) {
  for (int i=0; i<N(t1); i++) {
    bool found= false;
    for (int j=0; j<N(t2); j++)
      found= found || t1[i] == t2[j];
    if (!found) return false;
  }
  return true;
}

static bool
bib_equivalent (tree t1, tree t2) {
  if (t1 == t2) return true;
  if (is_atomic (t1) || is_atomic (t2)) return false;
  if (N(t1) != 5 || N(t2) != 5) return false;
  if (t1[1] != t2[1] || t1[2] != t2[2]) return false;
  if (!is_document (t1[4]) || !is_document (t2[4])) return false;
  if (N(t1[4]) != N(t2[4])) return false;
  return children_included (t1[4], t2[4]) && children_included (t2[4], t1[4]);
}

/******************************************************************************
* Conservative BibTeX -> TeXmacs importation
******************************************************************************/

static tree
bib_import (string s) {
  return as_tree (call ("zealous-bib-import", s));
}

tree
conservative_bib_import (string old_s, tree old_t, string new_s) {
  if (get_preference ("bibtex->texmacs:conservative", "off") != "on" ||
      !is_func (old_t, DOCUMENT))
    return bib_import (new_s);
  if (new_s == old_s) return old_t;

  array<string> old_a= bib_break (old_s);
  array<string> new_a= bib_break (new_s);
  hashmap<string,int> old_i= bib_indices (old_a);
  hashmap<string,int> new_i= bib_indices (new_a);
  hashmap<string,tree> old_e= bib_collect_entries (old_t);

  if (!bib_check_standard_preamble (old_a) ||
      !bib_check_standard_preamble (new_a))
    return bib_import (new_s);
  int d;
  for (d=1; d<N(old_a) && d<N(new_a); d+=2)
    if (new_a[d] != old_a[d]) break;
  if (d < N(old_a) && d < N(new_a))
    if (bib_is_special (old_a[d]) || bib_is_special (new_a[d]))
      return bib_import (new_s);

  string delta_s;
  for (int i=1; i<N(new_a); i+=2) {
    if (bib_is_special (new_a[i]))
      delta_s << new_a[i-1] << new_a[i];
    else {
      string key= bib_get_key (new_a[i]);
      int j= old_i[key];
      if (j < 0 || new_a[i] != old_a[j])
        delta_s << new_a[i-1] << new_a[i];
    }
  }
  tree delta_t= bib_import (delta_s);
  hashmap<string,tree> delta_e= bib_collect_entries (delta_t);
  //cout << "================================\n";
  //cout << "delta_t= " << delta_t << "\n";
  //cout << "================================\n";

  tree doc (DOCUMENT);
  for (int i=0; i<N(old_t); i++)
    if (is_db_entry (old_t[i])) break;
    else doc << old_t[i];
  for (int i=1; i<N(new_a); i+=2)
    if (!bib_is_special (new_a[i]) && bib_get_type (new_a[i]) != "comment") {
      string key= bib_get_key (new_a[i]);
      tree val;
      if (delta_e->contains (key)) val= delta_e[key];
      else val= old_e[key];
      doc << val;
    }
  return doc;
}

/******************************************************************************
* Conservative TeXmacs -> BibTeX exportation
******************************************************************************/

string
conservative_bib_export (string src_s, tree src_t, string obj_s, tree obj_t) {
  if (get_preference ("texmacs->bibtex:conservative", "off") != "on")
    return obj_s;
  //cout << "Conservative export\n";
  if (src_t == obj_t) return src_s;
  array<string> src_a= bib_break (src_s);
  array<string> obj_a= bib_break (obj_s);
  hashmap<string,int> src_i= bib_indices (src_a);
  hashmap<string,tree> src_e= bib_collect_entries (src_t);
  hashmap<string,tree> obj_e= bib_collect_entries (obj_t);
  int i, n= N (obj_a);
  string r= copy (src_a[0]);
  int prev_match= -3;
  for (i=1; i<n; i+=2) {
    string key= bib_get_key (obj_a[i]);
    if (src_e->contains (key) &&
        obj_e->contains (key) &&
        bib_equivalent (src_e[key], obj_e[key])) {
      int this_match= src_i[key];
      if (i == 1);
      else if (this_match == prev_match + 2) r << copy (src_a[this_match-1]);
      else r << "\n";
      if (this_match >= 0) r << copy (src_a[this_match]);
      else r << copy (obj_a[i]);
      prev_match= this_match;
      //cout << "Matched " << i << ", " << this_match << "\n";
    }
    else {
      if (i != 1) r << "\n";
      r << copy (obj_a[i]);
      prev_match= -1;
    }
  }
  //cout << "Lengths " << N(src_a) << ", " << N(obj_a) << "\n";
  if (N(src_a) > 1) r << copy (src_a[N(src_a)-1]);
  //if (r == src_s) cout << "Unaltered\n";
  return r;
}

static string
bib_export (tree t) {
  return as_string (call ("zealous-bib-export", t));
}

string
conservative_bib_export (tree old_t, string old_s, tree new_t) {
  if (get_preference ("texmacs->bibtex:conservative", "off") != "on" ||
      !is_func (old_t, DOCUMENT) ||
      !is_func (new_t, DOCUMENT))
    return bib_export (new_t);
  if (new_t == old_t) return old_s;

  array<string> old_a= bib_break (old_s);
  hashmap<string,int> old_i= bib_indices (old_a);
  hashmap<string,tree> old_e= bib_collect_entries (old_t);
  hashmap<string,tree> new_e= bib_collect_entries (new_t);
  if (!bib_check_standard_preamble (old_a))
    return bib_export (new_t);

  tree delta_t (DOCUMENT);
  iterator<string> it= iterate (new_e);
  while (it->busy ()) {
    string key= it->next ();
    tree val= new_e[key];
    if (!old_e->contains (key) || old_e[key] != val || !old_i->contains (key))
      delta_t << val;
  }
  string delta_s= bib_export (delta_t);
  array<string> delta_a= bib_break (delta_s);
  hashmap<string,int> delta_i= bib_indices (delta_a);
  //cout << "============== delta_s ================\n";
  //cout << delta_s;
  //cout << "=======================================\n";

  string new_s;
  for (int i=0; i<N(new_t); i++)
    if (is_db_entry (new_t[i])) {
      string key= cork_to_utf8 (new_t[i][2]->label);
      if (delta_i->contains (key))
        new_s << "\n" << delta_a [delta_i [key]];
      else if (old_i->contains (key))
        new_s << "\n" << old_a [old_i [key]];
    }
  return conservative_bib_export (old_s, old_t, new_s, new_t);
}
