
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

bool skip_curly (string s, int& i);

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
latex_unmark (string s, hashset<path> l, hashmap<int,path>& corr) {
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
            corr (N(r))= p * suffix;
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

string
latex_unmark (string s, hashset<path> l) {
  bool flag= (N(l) == 0);
  if (flag) l->insert (path (-1)); // force checking
  hashmap<int,path> corr;
  string r= latex_unmark (s, l, corr);
  if (flag) l->remove (path (-1));
  return r;
}

/******************************************************************************
* LaTeX -> TeXmacs conversion with source tracking
******************************************************************************/

string
tree_to_latex_document (tree d, object opts) {
  eval ("(use-modules (convert latex init-latex))");
  return as_string (call ("texmacs->latex-document", object (d), opts));
}

string
tracked_texmacs_to_latex (tree d, object opts) {
  if (get_preference ("texmacs->latex:source-tracking", "off") != "on")
    return tree_to_latex_document (d, opts);
  tree t= extract (d, "body");

  string ms, s;
  string tt_opt = "texmacs->latex:transparent-source-tracking";
  bool   tt_flag= get_preference (tt_opt, "off") == "on";
  if (tt_flag) s= tree_to_latex_document (d, opts);

  hashset<path> invalid;
  while (true) {
    hashset<path> l= copy (invalid);
    tree mt= texmacs_mark (t, path (), l);
    cout << HRULE << "Marked texmacs" << LF << HRULE << mt << LF;
    tree md= change_doc_attr (d, "body", mt);
    string ms= tree_to_latex_document (md, opts);
    cout << HRULE << "Marked latex" << LF << HRULE << ms << LF;
    string ums= latex_unmark (ms, l);
    cout << HRULE << "Unmarked latex" << LF << HRULE << ums << LF;
    if (!tt_flag || ums == s) { s= ums; break; }

    //int old_nr= N(invalid);
    //latex_declare_transparent (ms, l);
    //if (N(l) > N(invalid)) { invalid= l; continue; }
    //latex_check_transparency (ms, s, invalid);
    //if (N(invalid) <= old_nr) break;
    break;
  }

  string post;
  post << tree_to_scheme (t);
  post << "\n% Separate attachments\n";
  post << ms;
  // TODO: add integrity checksum
  post= encode_base64 (post);
  s << "\n%%%%%%%%%% Begin TeXmacs source\n";
  s << "% " << replace (post, "\n", "\n% ");
  s << "\n%%%%%%%%%% End TeXmacs source\n";
  return s;
}
