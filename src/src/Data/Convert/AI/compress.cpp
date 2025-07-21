
/******************************************************************************
* MODULE     : compress.cpp
* DESCRIPTION: compression routines for AI purposes
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "converter.hpp"
#include "locale.hpp"
#include "wencoding.hpp"
#include "vars.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"

static hashmap<string,tree> compressed;
static hashmap<tree,string> compressed_code;

tree html_as_compressed (string s, int& pos, string close, int mode);

/******************************************************************************
* Compressing non-textual markup as trees
******************************************************************************/

tree
compress_tree (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  if (is_format (t)) {
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= compress_tree (t[i]);
    return r;
  }
  tree r= compound ("compressed", "");
  tree val (L(t));
  for (i=0; i<n; i++)
    if (the_drd->is_accessible_child (t, i) &&
        the_drd->get_env_child (t, i, MODE, "text") == "text") {
      val << compound ("decompressed");
      r << compress_tree (t[i]);
    }
    else val << t[i];
  if (compressed_code->contains (val)) {
    //cout << val << " <- " << compressed_code[val] << "\n";
    r[0]= compressed_code[val];
  }
  else {
    int nr= N(compressed);
    string code= "x" * as_string (nr);
    r[0]= code;
    //cout << code << ", " << N(compressed) << " -> " << val << "\n";
    compressed (code)= val;
    compressed_code (val)= code;
  }
  return r;
}

/******************************************************************************
* Decompressing non-textual markup from trees
******************************************************************************/

tree
decompress_tree (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  if (is_format (t)) {
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= decompress_tree (t[i]);
    return r;
  }
  if (is_compound (t, "compressed") && N(t) >= 1 && is_atomic (t[0])) {
    string code= t[0]->label;
    if (compressed->contains (code)) {
      tree val= copy (compressed[code]);
      int j=0, k=N(val);
      for (i=1; i<n; i++) {
        while (j<k && !is_compound (val[j], "decompressed", 0)) j++;
        if (j<k) val[j]= decompress_tree (t[i]);
        else val[j]= "";
      }
      return val;
    }
  }
  return "";
}

/******************************************************************************
* Compressing non-textual TeXmacs markup as a subset of HTML
******************************************************************************/

void
compressed_as_html (string& s, tree t, int mode) {
  if (is_atomic (t)) {
    string r= cork_to_utf8 (t->label);
    r= replace (r, "&", "&amp;");
    r= replace (r, "<", "&lt;");
    r= replace (r, ">", "&gt;");
    s << r;
    return;
  }
  int i, n= N(t);
  if (is_document (t)) {
    for (i=0; i<n; i++) {
      s << "<p>";
      compressed_as_html (s, t[i], mode);
      s << "</p>";
      if (i < (n-1) && mode != 0) s << "\n";
    }
  }
  else if (is_concat (t)) {
    for (i=0; i<n; i++)
      compressed_as_html (s, t[i], mode);
  }
  else if (is_compound (t, "compressed", 1) && is_atomic (t[0]))
    s << "<a id=\"" << t[0]->label << "\">";
  else if (is_compound (t, "compressed") && N(t) > 1 && is_atomic (t[0])) {
    for (i=1; i<n; i++) {
      s << "<div id=\"";
      if (i > 1) s << "cont-";
      s << t[0]->label << "\">";
      compressed_as_html (s, t[i], mode);
      s << "</div>";
    }
  }
}

string
compress_html (tree t, int mode) {
  string r;
  tree c= compress_tree (t);
  compressed_as_html (r, c, mode);
  return r;
}

/******************************************************************************
* Decompressing non-textual TeXmacs markup from a subset of HTML
******************************************************************************/

string
parse_html_tag (string s, int& pos, string& var, string& val) {
  var= ""; val= "";
  //cout << "s[pos] = " << s[pos] << "\n";
  if (s[pos] != '<') return "";
  int start= pos, n= N(s);
  pos++;
  while (pos < n && s[pos-1] != '>') pos++;
  //cout << "s[pos-1] = " << s[pos-1] << "\n";
  if (s[pos-1] != '>') return "";
  string tag= s (start + 1, pos - 1);
  //cout << "tag= " << tag << ", " << N(s) << "\n";
  string arg;
  int i, k= N(tag);
  for (i=0; i<k; i++)
    if (tag[i] == ' ') {
      arg= tag (i+1, k);
      tag= tag (0, i);
      break;
    }
  k= N(arg);
  if (k > 0)
    for (i=0; i<k; i++)
      if (arg[i] == '=') {
        val= arg (i+1, k);
        var= arg (0, i);
        break;
      }
  if (N(val) >= 2 && val[0] == '\"' && val[N(val)-1] == '\"')
    val= val (1, N(val) - 1);
  return tag;
}

tree
html_as_compressed (string s, int& pos, int mode) {
  //cout << "convert " << pos << "\n";
  int n= N(s);
  if (pos >= n) return "";
  if (s[pos] != '<') {
    int start= pos;
    while (pos < n && s[pos] != '<') pos++;
    string r= s (start, pos);
    r= replace (r, "&lt;", "<");
    r= replace (r, "&gt;", ">");
    r= replace (r, "&amp;", "&");
    return utf8_to_cork (r);
  }
  string var, val;
  string tag= parse_html_tag (s, pos, var, val);
  //cout << "tag = " << tag << ", " << var << ", " << val << "\n";
  if (tag == "p") {
    tree doc (DOCUMENT);
    doc << html_as_compressed (s, pos, "</p>", mode);
    while (test (s, pos, "\n")) pos++;
    while (test (s, pos, "<p>")) {
      pos += 3;
      doc << html_as_compressed (s, pos, "</p>", mode);
      while (test (s, pos, "\n")) pos++;
    }
    return doc;
  }
  else if (tag == "a") {
    if (var != "id") return "";
    return compound ("compressed", val);
  }
  else if (tag == "div") {
    if (var != "id") return "";
    string cont= "<div id=\"cont-" * val * "\">";
    tree r= compound ("compressed", val);
    r << html_as_compressed (s, pos, "</div>", mode);
    while (test (s, pos, cont)) {
      pos += N(cont);
      r << html_as_compressed (s, pos, "</div>", mode);
    }
    return r;
  }
  else return "";
}

tree
html_as_compressed (string s, int& pos, string close, int mode) {
  //cout << "convert " << pos << ", " << close << "\n";
  tree r (CONCAT);
  while (pos < N(s) && !test (s, pos, close))
    r << html_as_compressed (s, pos, mode);
  if (pos < N(s) && test (s, pos, close)) pos += N(close);
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
decompress_html (string s, int mode) {
  int pos= 0;
  tree t= html_as_compressed (s, pos, "<never>", mode);
  //cout << "t= " << t << "\n";
  return decompress_tree (t);
}
