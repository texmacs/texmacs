
/******************************************************************************
* MODULE     : tracked_fromtex.cpp
* DESCRIPTION: Conversion from LaTeX to TeXmacs with source tracking
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
#include "convert.hpp"

/******************************************************************************
* Protect against adding markers to a LaTeX document
******************************************************************************/

static void
latex_protect (string s, hashset<int>& l, int& i, string env) {
  string b= "\\begin{" * env * "}";
  string e= "\\end{" * env * "}";
  if (test (s, i, b)) {
    i += N(b);
    int j= i;
    while (j < N(s) && !test (s, j, e)) j++;
    for (int k=i; k<min(N(s),j+1); k++)
      l->insert (k);
    i= j + N(e);
  }
  else i++;
}

static void
latex_protect (string s, hashset<int>& l) {
  int i, n= N(s);
  for (i=0; i<n; )
    if (s[i] == '\\') {
      if (test (s, i, "\\begin{verbatim}"))
        latex_protect (s, l, i, "verbatim");
      else if (test (s, i, "\\begin{alltt}"))
        latex_protect (s, l, i, "alltt");
      else i++;
    }
    else i++;
}

/******************************************************************************
* Add markers to LaTeX document
******************************************************************************/

bool
skip_curly (string s, int& i) {
  int n= N(s);
  skip_spaces (s, i);
  if (i >= n || s[i] != '{') return false;
  i++;
  while (true) {
    if (i >= n) return false;
    if (s[i] == '{') {
      if (!skip_curly (s, i)) return false;
      continue;
    }
    if (s[i] == '}') {
      i++;
      return true;
    }
    i++;
  }
}

static bool
skip_square (string s, int& i) {
  int n= N(s);
  skip_spaces (s, i);
  if (i >= n || s[i] != '[') return false;
  i++;
  while (true) {
    if (i >= n) return false;
    if (s[i] == '[') {
      if (!skip_square (s, i)) return false;
      continue;
    }
    if (s[i] == ']') {
      i++;
      return true;
    }
    i++;
  }
}

static bool
parse_begin_end (string s, int& i) {
  int n= N(s);
  if (i == n || s[i] != '\\') return false;
  if (test (s, i, "\\begin") || test (s, i, "\\end")) {
    int b= i;
    if (s[b+1] == 'b') i += 6;
    else i += 4;
    skip_spaces (s, i);
    int c= i;
    if (skip_curly (s, i)) {
      string cmd  = "begin-" * s (c+1, i-1);
      string type = latex_type (cmd);
      int    arity= latex_arity (cmd);
      if (type != "undefined") {
        if (s[b+1] == 'e') return true;
        bool opt= (arity < 0);
        if (opt) arity= -1 - arity;
        for (int j=0; j<arity; j++)
          if (!skip_curly (s, i)) { i=b; return false; }
        if (opt) {
          int j= i;
          skip_spaces (s, j);
          if (j<n && s[j] == '[') {
            i=j;
            if (!skip_square (s, i)) { i=b; return false; }
          }
        }
        return true;
      }
    }
    i= b;
  }
  return false;
}

static void
mark_begin (string& r, int i, hashset<int>& l) {
  if (l->contains (i)) return;
  l->insert (i);
  r << "{\\blx{" << as_string (i) << "}}";
}

static void
mark_end (string& r, int i, hashset<int>& l) {
  if (l->contains (i)) return;
  l->insert (i);
  r << "{\\elx{" << as_string (i) << "}}";
}

bool
skip_latex_spaces (string s, int& i) {
  int ln= 0, n= N(s);
  while (i<n) {
    skip_spaces (s, i);
    if (i<n && s[i] == '%') {
      skip_line (s, i);
      if (i<n && s[i] == '\n') i++;
    }
    else if (i<n && s[i] == '\n') {
      i++;
      ln++;
    }
    else break;
  }
  return ln >= 2;
}

string
latex_mark (string s, hashset<int>& l) {
  // FIXME: attention to Windows line breaks
  string r;
  int i= 0, n= N(s);
  skip_latex_spaces (s, i);
  mark_begin (r, i, l);
  while (true) {
    int b= i;
    if (skip_latex_spaces (s, i)) {
      if (i < n) {
        mark_end (r, b, l);
        r << s (b, i);
        mark_begin (r, i, l);
        continue;
      }
    }
    if (i >= n) {
      mark_end (r, b, l);
      r << s (b, i);
      break;
    }
    int bb= i;
    if (parse_begin_end (s, i)) {
      if (s[bb+1] == 'b') {
        r << s (b, i);
        b= i;
        if (skip_latex_spaces (s, i)) i= b;
        r << s (b, i);
        mark_begin (r, i, l);
      }
      else {
        mark_end (r, b, l);
        r << s (b, i);
      }
    }
    else {
      i++;
      r << s (b, i);
    }
  }
  return r;
}

/******************************************************************************
* Grouping markers in TeXmacs document
******************************************************************************/

static bool
contains_itm (tree t) {
  if (is_atomic (t)) return false;
  else if (is_compound (t, "itm", 1)) return true;
  else {
    for (int i=0; i<N(t); i++)
      if (contains_itm (t[i])) return true;
    return false;
  }
}

static tree
marked_group (string b, string e, tree body) {
  if (contains_itm (body)) return body;
  return compound ("mlx", b * ":" * e, body);
}

tree
texmacs_group_markers (tree t) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_group_markers (t[i]);
    if (is_concat (r) && N(r) >=2 &&
        is_compound (r[0], "blx", 1) &&
        is_compound (r[N(r)-1], "elx", 1))
      {
        string b= as_string (r[0][0]);
        string e= as_string (r[N(r)-1][0]);
        r= r (1, N(r)-1);
        if (N(r) == 0) r= "";
        else if (N(r) == 1) r= r[0];
        return marked_group (b, e, r);
      }
    if (is_document (r)) {
      tree d (DOCUMENT);
      for (int i=0; i<n; i++) {
        if (is_compound (r[i], "blx", 1) ||
            (is_concat (r[i]) && N(r[i]) > 1 &&
             is_compound (r[i][0], "blx", 1) &&
             !is_compound (r[i][N(r[i])-1], "elx", 1))) {
          int j= i+1;
          bool ok= false;
          while (j<n) {
            if (is_compound (r[j], "blx", 1)) break;
            if (is_compound (r[j], "elx", 1)) { ok= true; break; }
            if (is_concat (r[j])) {
              bool br= false;
              for (int k=0; k<N(r[j]); k++)
                if (is_compound (r[j][k], "blx", 1)) {
                  br= true; break; }
                else if (is_compound (r[j][k], "elx", 1)) {
                  br= true; ok= (k == (N(r[j]) - 1)); break; }
              if (br) break;
            }
            j++;
          }
          if (ok) {
            tree body (DOCUMENT);
            tree bt= r[i];
            tree et= r[j];
            if (is_concat (bt)) {
              tree rem= bt (1, N(bt));
              if (N(rem) == 1) rem= rem[0];
              body << rem;
              bt= bt[0];
            }
            body << A (r (i+1, j));
            if (is_concat (et)) {
              tree rem= et (0, N(et)-1);
              if (N(rem) == 1) rem= rem[0];
              body << rem;
              et= et[N(et)-1];
            }
            string b= as_string (bt[0]);
            string e= as_string (et[0]);
            i= j;
            if (N(body) == 0) continue;
            if (N(body) == 1) body= body[0];
            d << marked_group (b, e, body);
            continue;
          }
        }
        d << r[i];
      }
      r= d;
    }
    if (is_document (r) && N(r) >= 2 &&
        is_compound (r[0], "blx", 1) &&
        is_compound (r[N(r)-1], "elx", 1))
      {
        string b= as_string (r[0][0]);
        string e= as_string (r[N(r)-1][0]);
        r= r (1, N(r)-1);
        marked_group (b, e, r);
      }
    return r;
  }
}

static void
get_range (tree id, int& b, int& e) {
  array<string> a= tokenize (as_string (id), ":");
  if (N(a) != 2) b= e= -1;
  else {
    b= as_int (a[0]);
    e= as_int (a[1]);
  }
}

tree
texmacs_correct_markers (tree t, int b, int e) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "mlx", 2)) {
    int sb, se;
    get_range (t[0], sb, se);
    if (sb <= b || se >= e)
      return texmacs_correct_markers (t[1], b, e);
    return compound ("mlx", t[0], texmacs_correct_markers (t[1], sb, se));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_correct_markers (t[i], b, e);
    return r;
  }
}

/******************************************************************************
* Remove markers from TeXmacs document
******************************************************************************/

static tree
texmacs_unmark (tree t, bool all) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "blx", 1)) return "";
  else if (is_compound (t, "elx", 1)) return "";
  else if (all && is_compound (t, "mlx", 2)) return texmacs_unmark (t[1], all);
  else if (is_document (t)) {
    int i, n= N(t);
    tree r (DOCUMENT);
    for (i=0; i<n; i++)
      if (is_compound (t[i], "blx", 1));
      else if (is_compound (t[i], "elx", 1));
      else r << texmacs_unmark (t[i], all);
    return simplify_document (r);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_unmark (t[i], all);
    if (is_concat (r)) return simplify_concat (r);
    return r;
  }
}

tree texmacs_unmark (tree t) { return texmacs_unmark (t, true); }
tree texmacs_clean_markers (tree t) { return texmacs_unmark (t, false); }

/******************************************************************************
* Check transparency of marking process
******************************************************************************/

void texmacs_check_transparency (tree mt, tree t, hashset<int>& invalid);

static void
texmacs_declare_transparent (tree mt, hashset<int>& invalid) {
  if (is_compound (mt, "mlx", 2)) {
    int b, e;
    get_range (mt[0], b, e);
    invalid->remove (b);
    invalid->remove (e);
  }
  if (is_compound (mt))
    for (int i=0; i<N(mt); i++)
      texmacs_declare_transparent (mt[i], invalid);
}

static void
texmacs_declare_opaque (tree mt, hashset<int>& invalid) {
  if (is_compound (mt, "mlx", 2)) {
    int b, e;
    get_range (mt[0], b, e);
    invalid->insert (b);
    invalid->insert (e);
  }
  if (is_compound (mt))
    for (int i=0; i<N(mt); i++)
      texmacs_declare_opaque (mt[i], invalid);
}

static bool
texmacs_relative_transparency (tree mt, tree t) {
  if (is_compound (mt, "mlx", 2)) return true;
  if (!is_compound (mt) || !is_compound (t)) return mt == t;
  if (N(mt) != N(t)) return false;
  for (int i=0; i<N(mt); i++)
    if (!texmacs_relative_transparency (mt[i], t[i]))
      return false;
  return true;
}

static void
texmacs_document_transparency (tree mt, int& mpos, tree t, int& pos,
                               hashset<int>& invalid) {
  while (mpos < N(mt) && pos < N(t)) {
    if (is_compound (mt[mpos], "mlx", 2) && is_document (mt[mpos][1])) {
      int spos= 0, bpos= pos;
      texmacs_document_transparency (mt[mpos][1], spos, t, pos, invalid);
      if (spos != N(mt[mpos][1])) break;
      if (!texmacs_relative_transparency (mt[mpos][1], t (bpos, pos)))
        texmacs_declare_opaque (mt[mpos], invalid);
      mpos++;
    }
    else {
      texmacs_check_transparency (mt[mpos], t[pos], invalid);
      mpos++; pos++;
    }
  }
}

void
texmacs_check_transparency (tree mt, tree t, hashset<int>& invalid) {
  if (texmacs_unmark (mt) == t || !is_compound (mt)) return;
  //cout << "Problematic " << mt << LF
  //<< "........... " << t << LF;
  if (is_compound (mt, "mlx", 2)) {
    if (texmacs_relative_transparency (mt[1], t))
      texmacs_check_transparency (mt[1], t[1], invalid);
    else
      texmacs_declare_opaque (mt, invalid);
  }
  else if (is_document (mt) && is_document (t)) {
    int mpos= 0, pos= 0;
    texmacs_document_transparency (mt, mpos, t, pos, invalid);
    if (mpos < N(mt) || pos < N(t))
      texmacs_declare_opaque (mt, invalid);
  }
  else if (!is_compound (t) || N(mt) != N(t))
    texmacs_declare_opaque (mt, invalid);
  else
    for (int i=0; i<N(mt); i++)
      texmacs_check_transparency (mt[i], t[i], invalid);
}

/******************************************************************************
* LaTeX -> TeXmacs conversion with source tracking
******************************************************************************/

tree
tracked_latex_to_texmacs (string s, bool as_pic) {
  if (get_preference ("latex->texmacs:source-tracking", "off") != "on")
    return latex_document_to_tree (s, as_pic);
  
  tree   t, body, mt, mbody;
  string tt_opt = "latex->texmacs:transparent-source-tracking";
  bool   tt_flag= get_preference (tt_opt, "off") == "on";
  if (tt_flag) {
    t= latex_document_to_tree (s, as_pic);
    body= extract (t, "body");
  }

  hashset<int> invalid;
  latex_protect (s, invalid);
  while (true) {
    //cout << HRULE << "Invalid markers" << LF << HRULE << invalid << LF;
    hashset<int> l= copy (invalid);
    string ms= latex_mark (s, l);
    //cout << HRULE << "Marked latex" << LF << HRULE << ms << LF;
    mt= latex_document_to_tree (ms, as_pic);
    mbody= extract (mt, "body");
    //cout << HRULE << "Marked texmacs" << LF << HRULE << mbody << LF;
    mbody= texmacs_group_markers (mbody);
    //cout << HRULE << "Grouped texmacs" << LF << HRULE << mbody << LF;
    mbody= texmacs_correct_markers (mbody, -1000000000, 1000000000);
    //cout << HRULE << "Corrected texmacs" << LF << HRULE << mbody << LF;
    mbody= texmacs_clean_markers (mbody);
    //cout << HRULE << "Cleaned texmacs" << LF << HRULE << mbody << LF;
    if (!tt_flag || texmacs_unmark (mbody) == body) break;

    int old_nr= N(invalid);
    texmacs_declare_transparent (mbody, l);
    if (N(l) > N(invalid)) { invalid= l; continue; }
    texmacs_check_transparency (mbody, body, invalid);
    if (N(invalid) <= old_nr) { mt= t; mbody= body; break; }
  }

  tree lsrc (ASSOCIATE, "latex-source", s);
  tree ltar (ASSOCIATE, "latex-target", change_doc_attr (mt, "body", mbody));
  tree atts (COLLECTION, lsrc, ltar);
  mbody= texmacs_unmark (mbody);
  mt= change_doc_attr (mt, "body", mbody);
  mt= change_doc_attr (mt, "attachments", atts);
  return mt;
}
