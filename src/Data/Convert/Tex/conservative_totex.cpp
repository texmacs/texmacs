
/******************************************************************************
* MODULE     : conservative_totex.cpp
* DESCRIPTION: Conservative conversion of TeXmacs to LaTeX
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

bool skip_latex_spaces (string s, int& i);

/******************************************************************************
* Extract tables for source/target correspondences
******************************************************************************/

tree
texmacs_correspondence (tree t, hashmap<tree,tree>& h) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "mlx", 2)) {
    tree r= texmacs_correspondence (t[1], h);
    if (!h->contains (r)) h(r)= tree (TUPLE);
    h(r) << t[0];
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_correspondence (t[i], h);
    if (is_concat (r)) return simplify_concat (r);
    if (is_document (r)) return simplify_document (r);
    return r;
  }
}

void
texmacs_predecessors (tree t, hashmap<tree,tree>& h) {
  if (is_atomic (t)) return;
  else {
    int i, n= N(t);
    for (i=1; i<n; i++)
      if (is_compound (t[i-1], "mlx", 2) &&
          is_compound (t[i], "mlx", 2))
        h(t[i][0])= t[i-1][0];
  }
}

/******************************************************************************
* Construct invarianted TeXmacs document
******************************************************************************/

static bool
get_range (tree id, int& b, int& e, string src) {
  array<string> a= tokenize (as_string (id), ":");
  if (N(a) != 2) return false;
  b= as_int (a[0]);
  e= as_int (a[1]);
  return b >= 0 && b <= e && e <= N(src);
}

tree
texmacs_invarianted (tree t, tree p, int c, string src,
                     hashmap<tree,tree> corr,
                     hashmap<tree,tree> pred,
                     hashmap<tree,tree> succ) {
  if (corr->contains (t)) {
    tree oids= corr[t], ids (TUPLE);
    for (int i=0; i<N(oids); i++) {
      int b, e;
      if (get_range (oids[i], b, e, src)) ids << oids[i];
    }
    if (N(ids) == 1)
      return compound ("ilx", ids[0]);
    // TODO: handle ambiguities
  }
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= texmacs_invarianted (t[i], t, i, src, corr, pred, succ);
      // TODO: fusion of comments and consecutive invarianted fragments
    }
    return r;
  }
}

static void
rewind_spaces (string s, int& i) {
  while (i>0 && (s[i-1] == ' ' || s[i-1] == '\t')) i--;
}

static void
rewind_line (string s, int& i) {
  while (i>0 && (s[i-1] != '\n')) i--;
}

static void
forward_line (string s, int& i) {
  while (i<N(s) && (s[i] != '\n')) i++;
}

static bool
is_comment_line (string s) {
  int i=0;
  skip_spaces (s, i);
  return i<N(s) && s[i] == '%';
}

tree
texmacs_invarianted_extend (tree id, string src) {
  int b, e, n= N(src);
  get_range (id, b, e, src);
  //cout << "Extending" << LF << HRULE << src (b, e) << LF << HRULE;
  rewind_spaces (src, b);
  while (b>0 && src[b-1] == '\n') {
    int prev= b-1;
    rewind_line (src, prev);
    if (is_comment_line (src (prev, b-1))) b= prev;
    else break;
  }
  skip_spaces (src, e);
  if (e<n && src[e] == '%') skip_line (src, e);
  while (e<n && src[e] == '\n') {
    int next= e+1;
    forward_line (src, next);
    if (is_comment_line (src (e+1, next))) e= next;
    else break;
  }
  //cout << "Extended to" << LF << HRULE << src (b, e) << LF << HRULE;
  return as_string (b) * ":" * as_string (e);
}

tree
texmacs_invarianted_merge (tree t, string src) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_invarianted_merge (t[i], src);
    if (is_concat (r) || is_document (r)) {
      tree m (L(r));
      for (i=0; i<n; i++) {
        if (is_document (r) && is_compound (r[i], "ilx", 1))
          r[i]= compound ("ilx", texmacs_invarianted_extend (r[i][0], src));
        if (N(m) > 0 &&
            is_compound (m[N(m)-1], "ilx", 1) &&
            is_compound (r[i], "ilx", 1)) {
          int b1, e1, b2, e2;
          get_range (m[N(m)-1][0], b1, e1, src);
          get_range (r[i][0], b2, e2, src);
          if (e1 <= b2) {
            skip_latex_spaces (src, e1);
            if (e1 >= b2) {
              string id= as_string (b1) * ":" * as_string (e2);
              m[N(m)-1][0]= id;
              continue;
            }
          }
        }
        m << r[i];
      }
      if (is_concat (m) && N(m) == 1) m= m[0];
      return m;
    }
    else return r;
  }
}

tree
texmacs_invarianted_replace (tree t, string src) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "ilx", 1)) {
    int b, e;
    get_range (t[0], b, e, src);
    return compound ("!ilx", src (b, e));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_invarianted_replace (t[i], src);
    return r;
  }
}

tree
texmacs_invarianted (tree t, tree oldt, string src) {
  // TODO: style, preamble, environment variables, etc.
  tree body= extract (t, "body");
  tree oldbody= extract (oldt, "body");
  hashmap<tree,tree> corr (UNINIT);
  hashmap<tree,tree> pred (UNINIT);
  hashmap<tree,tree> succ (UNINIT);
  (void) texmacs_correspondence (oldbody, corr);
  texmacs_predecessors (oldbody, pred);
  // TODO: succ= reverse (pred);
  body= texmacs_invarianted (body, UNINIT, -1, src, corr, pred, succ);
  body= texmacs_invarianted_merge (body, src);
  body= texmacs_invarianted_replace (body, src);
  return change_doc_attr (t, "body", body);
}

/******************************************************************************
* Conservative TeXmacs -> LaTeX conversion
******************************************************************************/

string
var_conservative_texmacs_to_latex (tree doc, object opts) {
  if (get_preference ("texmacs->latex:conservative", "off") != "on")
    return tracked_texmacs_to_latex (doc, opts);
  tree atts= extract (doc, "attachments");
  hashmap<string,tree> atts_map (UNINIT, atts);
  if (atts_map->contains ("latex-source")) {
    string lsource= as_string (atts_map["latex-source"]);
    tree ltarget= atts_map["latex-target"];
    doc= texmacs_invarianted (doc, ltarget, lsource);
  }
  return tracked_texmacs_to_latex (doc, opts);
}
