
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
    start= i;
    (void) bib_skip (s, i);
    bib_rewind (s, i);
    ASSERT (i > start, "strange entry in bib_break");
    a << s (start, i);
    cout << "---------------------------------------------------------\n";
    cout << s (start, i);
  }
  a << s (start, n);
  return a;
}

string
conservative_bib_export (string src_s, tree src_t, string obj_s, tree obj_t) {
  if (get_preference ("texmacs->bibtex:conservative", "off") != "on")
    return obj_s;
  //cout << "Conservative export\n";
  //(void) bib_break (src_s);
  if (src_t == obj_t) return src_s;
  return obj_s;
}
