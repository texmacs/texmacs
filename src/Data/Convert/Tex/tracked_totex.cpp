
/******************************************************************************
* MODULE     : tracked_totex.cpp
* DESCRIPTION: Conversion from TeXmacs to LaTeX with source tracking
* COPYRIGHT  : (C) 2014 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "hashset.hpp"
#include "scheme.hpp"
#include "base64.hpp"
#include "iterator.hpp"
#include "fast_search.hpp"
#include "file.hpp"

/******************************************************************************
* Add markers to TeXmacs document
******************************************************************************/

string
encode_as_string (path p) {
  string r;
  while (!is_nil (p)) {
    if (N(r) != 0) r << ",";
    r << as_string (p->item);
    p= p->next;
  }
  return r;
}

tree
texmacs_mark (tree t, path p, hashset<path>& l) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      path q= p * i;
      r[i]= texmacs_mark (t[i], q, l);
      if (is_document (t) &&
          !l->contains (q) &&
          !is_compound (r[i], "!ilx", 1)) {
        l->insert (q);
        r[i]= compound ("mtm", encode_as_string (q), r[i]);
      }
    }
    return r;
  }
}

/******************************************************************************
* Remove markers from LaTeX document
******************************************************************************/

path
decode_as_path (string s) {
  path p;
  array<string> a= tokenize (s, ",");
  for (int i=0; i<N(a); i++)
    if (!is_int (a[i])) return path ();
    else p= p * as_int (a[i]);
  return p;
}

string
latex_unmark (string s, hashset<path> l, hashmap<int,array<path> >& corr) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; ) {
    if (test (s, i, "{\\btm{") || test (s, i, "{\\etm{")) {
      int suffix= (s[i+2] == 'b'? 0: 1);
      int start= i;
      i += 5;
      int b= i+1;
      if (skip_curly (s, i)) {
        int e= i-1;
        path p= decode_as_path (s (b, e));
        if (!is_nil (p) && (l->contains (p) || N(l) == 0))
          if (i<n && s[i] == '}') {
            if (!corr->contains (N(r)))
              corr (N(r))= array<path> ();
            corr (N(r)) << (p * suffix);
            i++;
            continue;
          }
      }
      i= start;
    }
    r << s[i++];
  }
  return r;
}

/******************************************************************************
* Check transparency of marking process
******************************************************************************/

void
latex_declare_transparent (string ms, hashset<path>& l) {
  hashmap<path,path> corr;
  (void) latex_correspondence (ms, l, corr);
  iterator<path> it= iterate (corr);
  while (it->busy ()) {
    path p= it->next ();
    l->remove (p);
  }
}

static void
get_invalid_regions (string s1, int b1, int e1, string s2, int b2, int e2,
                     hashset<int>& regions) {
  while (b1 < e1 && b2 < e2 && s1[b1] == s2[b2]) { b1++; b2++; }
  while (b1 < e1 && b2 < e2 && s1[e1-1] == s2[e2-1]) { e1--; e2--; }
  int sb1, se1, sb2, se2;
  get_longest_common (s1 (b1, e1), s2 (b2, e2), sb1, se1, sb2, se2);
  //cout << HRULE << "Problematic" << LF << HRULE << s1 (b1, e1) << LF;
  //cout << HRULE << "Separator" << LF << HRULE << s1 (b1+sb1, b1+se1) << LF;
  if (se1 <= sb1) {
    //cout << HRULE << "Bad region" << LF << HRULE << s1 (b1, e1) << LF;
    for (int i=b1; i<=e1; i++)
      regions->insert (i);
  }
  else {
    get_invalid_regions (s1, b1, b1+sb1, s2, b2, b2+sb2, regions);
    get_invalid_regions (s1, b1+se1, e1, s2, b2+se2, e2, regions);
  }
}

void
latex_check_transparency (string ums, string s,
                          hashmap<int,array<path> > corr,
                          hashset<path>& invalid) {
  hashset<int> regions;
  get_invalid_regions (ums, 0, N(ums), s, 0, N(s), regions);

  for (int i=0; i<=N(s); i++)
    if (regions->contains (i) && corr->contains (i)) {
      array<path> a= corr[i];
      for (int j=0; j<N(a); j++)
        invalid->insert (path_up (a[j]));
    }
}

/******************************************************************************
* TeXmacs -> LaTeX conversion with source tracking
******************************************************************************/

string
tree_to_latex_document (tree d, object opts) {
  eval ("(use-modules (convert latex init-latex))");
  return as_string (call ("texmacs->latex-document", object (d), opts));
}

static tree
purify (tree d) {
  tree v= extract (d, "TeXmacs");
  tree s= extract (d, "style");
  tree p= extract (d, "project");
  tree b= extract (d, "body");
  tree i= extract (d, "initial");
  tree r (DOCUMENT);
  r << compound ("TeXmacs", v)
    << compound ("style", s);
  if (p != "") r << compound ("project", "");
  r << compound ("body", b);
  if (N(i) > 0) r << compound ("initial", i);
  return r;
}

bool
tracked_tree_to_latex_document (tree d, object opts, string& s, string& ms) {
  tree   t      = extract (d, "body");
  string tt_opt = "texmacs->latex:transparent-source-tracking";
  bool   tt_flag= get_preference (tt_opt, "off") == "on";
  if (tt_flag) s= tree_to_latex_document (d, opts);

  hashset<path> invalid;
  hashmap<int,array<path> > corr;
  int attempt= 0;
  while (true) {
    attempt++;
    //cout << HRULE << "Invalid markers" << LF << HRULE << invalid << LF;
    hashset<path> l= copy (invalid);
    tree mt= texmacs_mark (t, path (), l);
    //cout << HRULE << "Marked texmacs" << LF << HRULE << mt << LF;
    tree md= change_doc_attr (d, "body", mt);
    ms= tree_to_latex_document (md, opts);
    if (false) {
      cout << "Attempt " << attempt << LF;
      string mname= "marked-" * as_string (attempt) * ".tex";
      save_string ("$TEXMACS_HOME_PATH/system/tmp/" * mname, ms, false);
    }
    //cout << HRULE << "Marked latex" << LF << HRULE << ms << LF;
    l->insert (path (-1)); // force checking
    string ums= latex_unmark (ms, l, corr);
    l->remove (path (-1));
    //cout << HRULE << "Unmarked latex" << LF << HRULE << ums << LF;
    if (!tt_flag || ums == s) { s= ums; break; }

    //cout << HRULE << "Expected" << LF << HRULE << s << LF;
    int old_nr= N(invalid);
    hashset<path> new_invalid= copy (l);
    latex_declare_transparent (ms, new_invalid);
    if (N(new_invalid) > N(invalid)) { invalid= new_invalid; continue; }
    latex_check_transparency (ums, s, corr, invalid);
    if (N(invalid) <= old_nr) return true;
  }
  //cout << HRULE << "Marked latex" << LF << HRULE << ms << LF;
  return false;
}

string
tracked_texmacs_to_latex (tree doc, object opts) {
  if (get_preference ("texmacs->latex:source-tracking", "off") != "on")
    return tree_to_latex_document (doc, opts);

  string ms, s;
  if (tracked_tree_to_latex_document (doc, opts, s, ms)) return s;

  string post;
  post << tree_to_scheme (purify (doc));
  post << "\n% Separate attachments\n";
  post << ms;
  // TODO: add integrity checksum
  post= encode_base64 (post);
  s << "\n%%%%%%%%%% Begin TeXmacs source\n";
  s << "% " << replace (post, "\n", "\n% ");
  s << "\n%%%%%%%%%% End TeXmacs source\n";
  return s;
}
