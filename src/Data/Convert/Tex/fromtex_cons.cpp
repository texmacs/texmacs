
/******************************************************************************
 * MODULE     : fromtex_cons.cpp
 * DESCRIPTION: conservative conversion of tex strings into texmacs trees
 * COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
 ******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "base64.hpp"
#include "scheme.hpp"
#include "vars.hpp"
#include "tree_correct.hpp"
#include "merge_sort.hpp"

/******************************************************************************
 * Export of native TeXmacs documents
 ******************************************************************************/

tree
texmacs_to_latex_mark_document (tree t) {
  if (is_atomic (t)) return t;
  tree r (DOCUMENT);
  int i=0, j, n= N(t);
  while (i<n && !is_compound (t[i], "body", 1) && !is_document (t[i][0]))
    r << t[i++];
  if (i == n) return t;
  tree mbody (DOCUMENT), body= t[i][0];
  n= N(body);
  for (j=0; j<n-1; j++) {
    if (is_compound (body[j], "hide-preamble", 1) && is_document (body[j][0]))
    {
      tree tmp (DOCUMENT);
      tmp << compound ("tmtex@mark@preamble");
      tmp << A(body[j][0]);
      tmp << compound ("tmtex@mark@preamble");
      mbody << compound ("hide-preamble", tmp) << compound ("tmtex@mark");
    }
    else
      mbody << body[j] << compound ("tmtex@mark");
  }
  mbody << body[j];
  r << compound ("body", mbody);
  i++, n= N(t);
  while (i<n)
    r << t[i++];
  return r;
}

/******************************************************************************
 * Reimport of exported TeXmacs documents
 ******************************************************************************/

string
latex_normalize (string s) {
  // Normalize a bit LaTeX spaces and clean comments
  char status= 'N';
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    if (s[i] == '\n') {                     // New line
      if (status != 'N')
        r << s[i];
      status= 'N';
    }
    else if (s[i] == ' ' || s[i] == '\t') { // Space
      if (status == 'O') {
        status= 'S';
        r << s[i];
      }
      else if (status == 'T')
        status= 'S';
    }
    else if (s[i] == '%') {                 // Comment
      if (status != 'C')
        r << s[i];
      status= 'C';
    }
    else if (s[i] == '\\') {                // Token
      if (status != 'C') {
        r << s[i];
        status= 'T';
      }
    }
    else {                                  // Other
      if (status == 'T')
        r << s[i];
      else if (status != 'C') {
        r << s[i];
        status= 'O';
      }
    }
  }
  return trim_spaces (r);
}

struct latex_hash {
  hashmap<string,tree> h;
  inline string latex_normalize (string s) { return ::latex_normalize (s); };
  inline latex_hash (tree init, int n=1, int max=1): h (init, n, max) { };
  inline tree  operator [] (string x) { return h[latex_normalize (x)]; }
  inline tree& operator () (string x) { return h(latex_normalize (x)); }
};

static array<tree>
remove_empty (array<tree> l) {
  array<tree> r;
  for (int i=0; i<N(l); i++)
    if (N(l[i]) > 0 && l[i] != document (""))
      r << l[i];
  return r;
}

static array<string>
remove_empty (array<string> l) {
  array<string> r;
  for (int i=0; i<N(l); i++)
    if (l[i] != "")
      r << l[i];
  return r;
}

static tree
remove_preamble_marks (tree t) {
  if (!is_compound (t, "hide-preamble", 1) ||
      !is_document (t[0]) || N(t[0]) <= 2)
    return t;
  int n= N(t[0]);
  return compound ("hide-preamble", t[0](1, n-1));
}

static string
clean_preamble (string s) {
  return replace (s, "{\\tmtexmarkpreamble}\n", "");
}

static array<tree>
tokenize_document (tree body) {
  array<tree> r;
  tree tmp (DOCUMENT);
  for (int i=0; i<N(body); i++) {
    if (is_compound (body[i], "tmtex@mark")) {
      if (N(tmp) > 0) {
        r << tmp;
        tmp= tree (DOCUMENT);
      }
    }
    else
      tmp << body[i];
  }
  if (N(tmp) > 0) r << tmp;
  return r;
}

static array<string>
populates_lambda (latex_hash &lambda, tree doc, tree src) {
  string s= as_string (src);
  array<string> l_src= tokenize (s, "\n\n{\\tmtexmark}\n\n");
  l_src= remove_empty (l_src);
  tree body= (N(doc) > 2 && N(doc[2]) > 0)? doc[2][0] : tree (DOCUMENT);
  array<tree> l_body= tokenize_document (body);
  l_body= remove_empty (l_body);
  bool has_preamble= N(l_body) > 0 && N(l_body[0]) > 0
    && is_compound (l_body[0][0], "hide-preamble", 1);
  if (has_preamble && N(l_body)+1 == N(l_src)) {
    // asymetry due to \\end{document} only present in src.
    l_src[0]= clean_preamble (l_src[0]);
    l_body[0][0]= remove_preamble_marks (l_body[0][0]);
    l_src= range (l_src, 0, N(l_src)-1);
  }
  else if (!has_preamble && N(l_body)+2 == N(l_src))
    // asymetry due to \\end{document} and preamble only present in src.
    l_src= range (l_src, 1, N(l_src)-1);
  else
    return array<string> ();
  for (int i=0; i<N(l_body); i++)
    lambda(l_src[i])= l_body[i];
  return l_src;
}

struct key_less_eq_operator {
  static bool leq (string s1, string s2) {
    return N(s1) > N(s2);
  }
};

static array<string>
sort_keys (array<string> l) {
  merge_sort_leq<string,key_less_eq_operator> (l);
  return l;
}

static bool
latex_paragraph_end (string s, int i) {
  int n= N(s), count= 0;
  while (i<n && count < 2) {
    if (s[i] == '\n') count++;
    else if (s[i] == ' ' || s[i] == '\t');
    else if (s[i] == '%' && (i > 0 && s[i-1] != '\\'))
      while (i < n && s[i] != '\n') i++;
    else return false;
    i++;
  }
  return true;
}

static array<string>
tokenize_at_line_feed (string s, string sep) {
  int start=0;
  array<string> a;
  for (int i=0; i<N(s); )
    if (test (s, i, sep)
        && (N(s) == N(sep) + i || latex_paragraph_end (s, i+N(sep)))) {
      a << s (start, i);
      i += N(sep);
      start= i;
    }
    else i++;
  a << s(start, N(s));
  return a;
}

static array<string>
recover_document (string s, string p) {
  array<string> r, t= tokenize_at_line_feed (s, p);
  int i, n= N(t);
  if (n < 2) return t;
  for (i=0; i<n-1; i++)
    r << t[i] << p;
  r << t[n-1];
  return r;
}

static array<string>
recover_document (array<string> a, array<string> l, array<string> keys) {
  if (N(l) == 0) return a;
  string p= l[0];
  array<string> r;
  int i, n= N(a);
  for (i=0; i<n; i++)
    if (contains (a[i], keys))
      r << a[i];
    else
      r << recover_document (a[i], p);
  return recover_document (r, range (l, 1, N(l)), keys);
}

array<string>
remove_whitespace (array<string> l) {
  int i, n=N(l);
  array<string> r;
  for (i=0; i<n; i++)
    if (replace (replace (l[i], " ", ""), "\n", "") != "")
      r << l[i];
  return r;
}

int
latex_search_forwards (string s, string in);

static string
remove_end_of_document (string s) {
  int e= latex_search_forwards ("\\end{document}", s);
  if (e > 0)
    return s(0, e);
  else
    return s;
}

static array<string>
recover_document (string s, array<string> l) {
  array<string> a;
  a << remove_end_of_document (s);
  return remove_whitespace (recover_document (a, l, l));
}

bool
is_uptodate_tm_document (tree t) {
  return is_document (t) && N(t) > 1 && is_compound (t[0], "TeXmacs", 1)
    && as_string (t[0][0]) == TEXMACS_VERSION;
}

bool
is_preamble (string s) {
  return latex_search_forwards ("\\documentclass", s) >= 0;
}

tree concat_document_correct (tree t);

tree
latex_conservative_document_to_tree (string s, bool as_pic, bool keep_src,
    array<array<double> > range) {
  int b, e;
  b= search_forwards ("\n% -----BEGIN TEXMACS DOCUMENT-----\n%", 0, s) + 37;
  e= search_forwards ("% \n% -----END TEXMACS DOCUMENT-----", b, s);
  if (b < e) {
    string code= replace (s(b,e), "% ", "");
    s= s(0, b-37);
    tree d= stree_to_tree (string_to_object (decode_base64 (code)));
    if (is_document (d) && N(d) == 2 && is_uptodate_tm_document (d[0])) {
      tree document= d[0](1, N(d[0]));
      tree uninit= tree (UNINIT);
      latex_hash lambda (uninit);
      array<string> keys= populates_lambda (lambda, d[0], d[1]);
      if (N(keys) == 0) return "";
      keys= sort_keys (keys);
      array<string> l= recover_document (s, keys);
      tree body (DOCUMENT);
      int i, n= N(l);
      for (i=0; i<n; i++) {
        tree tmp= lambda[l[i]];
        if (tmp != uninit)
          body << tmp;
        else if (i == 0 && is_preamble (l[i])) {
          document= latex_to_tree (parse_latex_document (
                l[i] * "\n\\end{document}",
                true, as_pic, false, array<array<double> > ()));
          if (is_document (document)       && N(document)       > 1 &&
              is_compound (document[1], "body", 1)                  &&
              is_document (document[1][0]) && N(document[1][0]) > 0 &&
              is_compound (document[1][0][0], "hide-preamble", 1))
            body << document[1][0][0];
        }
        else
          body << latex_to_tree (parse_latex (l[i], true, false, as_pic,
                false, array<array<double> > ()));
      }
      document[1]= compound ("body", body);
      return concat_document_correct (document);
    }
  }
  return "";
}

/******************************************************************************
 * Import LaTeX documents (paragraph marking)
 ******************************************************************************/

static bool
paragraph_break_here (string s, int i) {
  int n= N(s);
  if (i >= n || s[i] != '\n') return false;
  i++;
  while (i < n && s[i] == ' ') i++;
  if (i >= n || s[i] != '\n') return false;
  return true;
}

static bool
in_range (double i, array<array<double> > ranges) {
  int j, n= N(ranges);
  for (j=0; j<n; j++)
    if (N(ranges[j]) == 2 && ranges[j][0] <= i && i <= ranges[j][1])
      return true;
  return false;
}

string
latex_to_texmacs_mark_document_pre (string s, array<array<double> > ranges) {
  string r;
  int i= 0, start= 0, n= N(s), count= 0;
  while (i < n) {
    int j= i+1;
    if (count == 0 && s[i] == '\n'
        && !in_range (i, ranges)
        && (paragraph_break_here (s, i) || (j < n && s[j] == '\\' &&
        // strategic places to cut
        (test_env   (s, j, "abstract")          ||
         test_env   (s, j, "abstract", false)   ||
         test_env   (s, j, "document")          ||
         test_env   (s, j, "document", false)   ||
         test_env   (s, j, "titlepage")         ||
         test_env   (s, j, "titlepage", false)  ||
         test_macro (s, j, "\\chapter")         ||
         test_macro (s, j, "\\date")            ||
         test_macro (s, j, "\\maketitle")       ||
         test_macro (s, j, "\\part")            ||
         test_macro (s, j, "\\section")         ||
         test_macro (s, j, "\\subsection")      ||
         test_macro (s, j, "\\subsubsection")   ||
         test_macro (s, j, "\\tableofcontents") ||
         test_macro (s, j, "\\title"))))) {
      r << s(start, i+1)
        << "\n\n\\textm@break{" << as_string (i) << "}\n";
      start= i+1;
      if (!(s[i] == '\n' && paragraph_break_here (s, i))) {
        i+= 4;
      }
      else {
        while (i < n) {
          if (s[i] == ' ' || s[i] == '\n') i++;
          else if (s[i] == '%' && (i == 0 || s[i-1] != '\\'))
            while (i < n && s[i] != '\n') i++;
          else break;
        }
      }
    }
    else if ((i == 0 || s[i-1] != '\\') && s[i] == '{')
      i++, count++;
    else if ((i == 0 || s[i-1] != '\\') && s[i] == '}')
      i++, count--;
    else
      i++;
  }
  if (i!= start) r << s(start, n);
  return r;
}

static tree
clean_paragraph_markup (tree t, int d) {
  if (is_atomic (t)) return t;
  tree r(L(t));
  int i, n= N(t);
  if (!is_concat (t)) {
    for (i=0; i<n; i++)
      r << clean_paragraph_markup (t[i], d + 1);
    return r;
  }
  bool merge= false;
  for (i=0; i<n; i++) {
    if (i+2 < n && t[i] == concat ("\n") && t[i+2] == concat ("\n")
        && is_tuple (t[i+1], "\\textm@break", 1)) {
      if (d == 0) {
        if (merge)
          r[N(r)-1]= t[i];
        else
          r << t[i+1];
        merge= true;
      }
      else i++;
    }
    else if (is_tuple (t[i], "\\textm@break", 1)) {
      if (d == 0) {
        if (merge)
          r[N(r)-1]= t[i];
        else
          r << t[i];
        merge= true;
      }
    }
    else {
      r << clean_paragraph_markup (t[i], d);
      merge= false;
    }

    if (is_tuple (t[i]) && N(t[i]) > 0
        && starts (as_string (t[i][0]), "\\begin-")
        && t[i][0] != "\\begin-document")
      d++;
    else if (is_tuple (t[i]) && N(t[i]) > 0
        && starts (as_string (t[i][0]), "\\end-")
        && t[i][0] != "\\end-document")
      d--;
    else if (is_tuple (t[i]) && N(t[i]) > 0 && t[i][0] == "\\begin-document")
      d--;
    else if (is_tuple (t[i]) && N(t[i]) > 0 && t[i][0] == "\\end-document")
      d++;
  }
  return r;
}

static tree
clean_paragraph_markup (tree t) {
  return clean_paragraph_markup (t, 1);
}

static tree
fill_paragraph_markup (tree t, string s) {
  if (is_atomic (t)) return t;
  tree r(L(t));
  int start= 0, stop= 0;
  for (int i=0; i<N(t); i++) {
    if (is_tuple (t[i], "\\textm@break", 1)) {
      stop= as_int (simplify_concat (t[i][1])) + 1;
      r << tuple ("\\textm@break", verbatim_escape (s(start, stop)),
            as_string (start),  as_string (stop));
      start= stop+1;
    }
    else
      r << t[i];
  }
  if (!is_tuple (t[N(t)-1], "\\textm@break", 1))
    r << tuple ("\\textm@break", verbatim_escape (s(start, N(s))),
          as_string (start),  as_string (N(s)));
  return r;
}

tree
latex_to_texmacs_mark_document_post (tree t, string s) {
    tree r= clean_paragraph_markup (t);
    r= fill_paragraph_markup (r, s);
    return r;
}

/******************************************************************************
 * Import of marked LaTeX documents (paragraph breaking managment)
 ******************************************************************************/

tree merge_successive_withs (tree t, bool force_concat= false);
tree unnest_withs (tree t);
tree remove_empty_withs (tree t);
tree concat_sections_and_labels (tree t);
tree modernize_newlines (tree t, bool skip);

static tree
search_and_remove_breaks (tree t, tree &src) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r(L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (!is_atomic (t[i][0]))
        src << A(t[i][0]);
    }
    else {
      tree tmp= search_and_remove_breaks (t[i], src);
      if (is_concat (tmp) && N(tmp) == 0);
      else if (is_concat (tmp) && N(tmp) == 1) r << tmp[0];
      else r << tmp;
    }
  }
  return r;
}

static tree
merge_non_root_break_trees (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree start (as_string (0)), src (CONCAT), r (L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (!is_atomic (t[i][0])) {
        src << A(t[i][0]);
        t[i][0]= src;
        t[i][1]= start;
        r << t[i];
        src= tree (CONCAT);
        start= t[i][2];
      }
    }
    else {
      tree tmp= search_and_remove_breaks (t[i], src);
      if (is_concat (tmp) && N(tmp) == 0);
      else if (is_concat (tmp) && N(tmp) == 1) r << tmp[0];
      else r << tmp;
    }
  }
  return r;
}

static tree
merge_empty_break_trees (tree t) {
  int i, n=N(t);
  bool merge= false;
  tree src, r (L(t));
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (merge) {
        src= tree (CONCAT);
        src << A(r[N(r)-1][0]);
        if (!is_atomic (t[i][0]))
          src << A(t[i][0]);
        r[N(r)-1][0]= src;
      }
      else {
        r << t[i];
        merge= true;
      }
    }
    else {
      r << t[i];
      merge= false;
    }
  }
  return r;
}

static bool
contains_title_or_abstract (tree t) {
  if (is_atomic (t)) return false;
  if (is_compound (t, "doc-data") || is_compound (t, "abstract-data"))
    return true;
  int i, n=N(t);
  for (i=0; i<n; i++)
    if (contains_title_or_abstract (t[i]))
      return true;
  return false;
}

static tree
merge_non_ordered_break_trees (tree t) {
  if (!contains_title_or_abstract (t)) return t;
  int i, n=N(t);
  bool merge= false;
  tree src (CONCAT), r (L(t));
  for (i=n-1; i>=0; i--) {
    if (is_compound (t[i], "textm@break", 3)) {
      if (merge)
        src= (t[i][0] << A(src));
      else
        r << t[i];
    }
    else {
      r << t[i];
      if (contains_title_or_abstract (t[i]))
        merge= true;
    }
  }
  n=N(r);
  for (i=0; i<n; i++) {
    if (is_compound (r[i], "textm@break", 3)) {
      if (!is_atomic (r[i][0]))
        src << A(r[i][0]);
      r[i][0]= src;
      break;
    }
  }
  return tree (DOCUMENT, reverse (A(r)));
}

static void
separate_documents (tree t, tree &the_good, tree &the_bad, tree &the_ugly) {
  if (is_atomic (t)) {
    the_good= t;
    return;
  }
  t= merge_non_root_break_trees (t);
  t= merge_empty_break_trees (t);
  t= merge_non_ordered_break_trees (t);
  t= merge_successive_withs (t);
  t= unnest_withs (t);
  t= remove_empty_withs (t);
  t= concat_sections_and_labels (t);
  int i, n= N(t);
  tree u (DOCUMENT);
  unsigned int id_cnt= 0;
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "textm@break", 3)) {
      string uid= as_string (id_cnt++);
      tree src= modernize_newlines (t[i][0], false);
      tree from= t[i][1], to= t[i][2];
      src= concat_document_correct (src);
      src= simplify_correct (src);
      if (N(u) == 1 && is_compound (u[0], "hide-preamble"))
        u= u[0];
      the_ugly << compound ("latex-src", from, to, src);
      if (u != document ()) {
        the_good << u;
        the_bad << u << compound ("textm@break");
      }
      u= tree (DOCUMENT);
    }
    else
      u << t[i];
  }
  if (N(u) > 0 && the_good == document ())
    the_good= t;
  else if (N(u) > 0) {
    the_good << u;
    the_bad << u;
  }
  else if (N(the_bad) > 0 && the_bad[N(the_bad)-1] == compound ("textm@break"))
    the_bad= the_bad(0, N(the_bad)-1);
}

tree
latex_add_conservative_attachments (tree doc) {
  if (!(is_document (doc) && N(doc) > 1 && is_compound (doc[1], "body", 1)))
    return doc;
  tree mdoc (L(doc), N(doc));
  mdoc[0]= doc[0];
  for (int i=2; i<N(doc); i++)
    mdoc[i]= doc[i];

  tree unmarked (DOCUMENT), marked (DOCUMENT), msrc (DOCUMENT), att;
  separate_documents (doc[1][0], unmarked, marked, msrc);
  mdoc[1]= compound ("body", marked);
  doc[1][0]= unmarked;
  msrc= compound ("associate", "latex-src", msrc);
  mdoc= compound ("associate", "latex-doc", mdoc);
  att= compound ("attachments", compound ("collection", mdoc, msrc));
  doc << att;
  return doc;
}

/******************************************************************************
 * Reexport of imported LaTeX documents
 ******************************************************************************/

static tree
extract_from_doc (tree t, string label) {
  if (!is_document (t))
    return "";
  int i= 0, n= N(t);
  while (i<n) {
    if (as_string (L(t[i])) == label)
      return t[i];
    i++;
  }
  return "";
}

static tree
extract_from_attachements (tree t, string what) {
  if (!(is_compound (t, "attachments", 1) &&
        is_compound (t[0], "collection") && N(t[0]) > 0))
    return "";
  tree u, col= t[0];
  int i= 0, n= N(col);
  while (i<n) {
    u= col[i];
    if (as_string (L(u)) == "associate" && N(u) == 2 && u[0] == what)
      return u[1];
    i++;
  }
  return "";
}

static array<tree>
populate_texmacs_latex_hash (tree t, hashmap<tree,tree> &h) {
  tree msrc= extract_from_attachements (t, "latex-src");
  // recovering list of doc fragments from marked document
  tree mdoc= extract_from_attachements (t, "latex-doc");
  if (extract_from_doc (mdoc, "TeXmacs")
      != compound ("TeXmacs", TEXMACS_VERSION) || msrc == "")
    return array<tree> ();
  mdoc= extract_from_doc (mdoc, "body");
  if (!(is_compound (mdoc, "body", 1) && is_document (mdoc[0])))
    return array<tree> ();
  mdoc= mdoc[0];
  array<tree> ldoc;
  tree tmp (DOCUMENT);
  int i, n= N(mdoc);
  for (i=0; i<n; i++) {
    if (is_compound (mdoc[i], "textm@break")) {
      if (N(tmp) > 0)
        ldoc << tmp;
      tmp= tree (DOCUMENT);
    }
    else
      tmp << mdoc[i];
  }
  if (N(tmp) > 0)
    ldoc << tmp;

  // recovering list of src fragments from source snippet
  array<tree> lsrc (N(msrc));
  n= N(msrc);
  for (i=0; i<n; i++) {
    if (is_compound (msrc[i], "latex-src", 3))
      lsrc[i]= msrc[i][2];
    else
      lsrc[i]= "";
  }

  // associating fragments
  if (N(lsrc) != N(ldoc))
    debug_convert << "Warning: marked doc and marked source"
      << "are not associable\n";
  n= N(ldoc);
  for (i=0; i<n; i++)
    h(ldoc[i])= lsrc[i];

  // we return the list of keys
  return ldoc;
}

static bool
test_subdocument (tree t, int i, tree test) {
  int n= N(t), m= N(test), j=0;
  while (j<m) {
    if (i>=n) return false;
    if (t[i]!=test[j]) return false;
    i++; j++;
  }
  return true;
}

static array<tree>
split_document_childs (tree t, tree sep) {
  int start=0;
  array<tree> a;
  if (!is_document (t)) {
    a << t;
    return a;
  }
  for (int i=0; i<N(t); )
    if (test_subdocument (t, i, sep)) {
      a << t (start, i);
      i += N(sep);
      start= i;
    }
    else i++;
  a << t(start, N(t));
  return a;
}

static array<tree>
recover_latex_texmacs (tree u, tree p) {
  array<tree> r, t= split_document_childs (u, p);
  int i, n= N(t);
  if (n < 2) return t;
  for (i=0; i<n-1; i++)
    r << t[i] << p;
  r << t[n-1];
  return r;
}

static array<tree>
recover_latex_texmacs (array<tree> a, array<tree> l, array<tree> keys) {
  if (N(l) == 0) return a;
  tree p= l[0];
  array<tree> r;
  int i, n= N(a);
  for (i=0; i<n; i++)
    if (contains (a[i], keys))
      r << a[i];
    else
      r << recover_latex_texmacs (a[i], p);
  return recover_latex_texmacs (r, range (l, 1, N(l)), keys);
}

static array<tree>
recover_latex_texmacs (tree body, array<tree> keys) {
  return remove_empty (recover_latex_texmacs (A(body), keys, keys));
}

static bool
same_containers (tree u, tree v) {
  return is_document (u) && is_document (v) && N(u) > 2 && N(v) > 2    &&
    extract_from_doc (u, "TeXmacs") == extract_from_doc (v, "TeXmacs")   &&
    extract_from_doc (u, "style")   == extract_from_doc (v, "style")   &&
    extract_from_doc (u, "initial") == extract_from_doc (v, "initial");
}

struct tkey_less_eq_operator {
  static bool leq (tree s1, tree s2) {
    return N(s1) > N(s2);
  }
};

static array<tree>
sort_keys (array<tree> l) {
  merge_sort_leq<tree,tkey_less_eq_operator> (l);
  return l;
}

tree
conservative_texmacs_to_latex (tree t) {
  tree uninit= tree (UNINIT);
  hashmap<tree,tree> h (uninit);
  tree body= extract_from_doc (t, "body");
  tree atts= extract_from_doc (t, "attachments");
  tree mdoc= extract_from_attachements (atts, "latex-doc");
  array<tree> keys= populate_texmacs_latex_hash (atts, h);
  if (N(keys) == 0)
    return "";
  keys= sort_keys (keys);
  array<tree> ldoc= recover_latex_texmacs (body, keys);

  /*
    Since we recover using only the document's body, we have to compare
    the "container" of t and the original "container" in att (the style
    and the initial, actually) and invalid the ldoc preamble related stuff.

    If the container is valid: use a simple '(document ...) as container.

    If preamble has changed OR if containers differs : use t as container.
  */

  tree rbody (DOCUMENT);
  int i, n= N (ldoc);
  for (i=0; i<n; i++) {
    tree tmp= h[ldoc[i]];
    if (tmp != uninit)
      rbody << compound ("!invariant", tmp);
    else
      rbody << ldoc[i];
  }
  tree r (DOCUMENT);
  if (!same_containers (t, mdoc)
      || (N(rbody) > 0 && !is_compound (rbody[0], "!invariant", 1))) {
    if (N(ldoc) > 0 && N(rbody) > 0 && is_compound (rbody[0], "!invariant"))
      rbody[0]= ldoc[0];
    r << extract_from_doc (t, "TeXmacs")
      << extract_from_doc (t, "style")
      << compound ("body", rbody)
      << extract_from_doc (t, "initial");
  }
  else
    r= rbody;
  return r;
}
