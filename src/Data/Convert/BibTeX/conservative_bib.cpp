
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

static array<string>
bib_break (string s) {
  int i=0, n=N(s), start=0;
  array<string> a;
  while (i<n) {
    start= i;
    while (true) {
      bool ok= bib_skip (s, i);
      if (ok || i >= n) break;
    }
    if (i >= n) break;
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

static void
bib_collect_entries (hashmap<string,tree>& h, tree t) {
  if (is_atomic (t)) return;
  else if (is_compound (t, "db-entry") ||
           is_compound (t, "db-folded-entry") ||
           is_compound (t, "db-pretty-entry")) {
    if (N(t) == 5 && is_atomic (t[2]))
      h (t[2]->label)= t;
  }
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
