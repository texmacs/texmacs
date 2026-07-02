
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

#define COMPRESS_LINE_FEEDS  1
#define COMPRESS_SNIPPET     2

#define INSIDE_BODY   0x10
#define INSIDE_P      0x20
#define INSIDE_DIV    0x40

/******************************************************************************
* Compressing non-textual markup as trees
******************************************************************************/

int
get_spc_mode (tree t, int i) {
  if (!is_concat (t)) return 0;
  int code= 0;
  if (i > 0 && is_atomic (t[i-1]) && ends (t[i-1]->label, " "))
    code += 1;
  if (i < N(t)-1 && is_atomic (t[i+1]) && starts (t[i+1]->label, " "))
    code += 2;
  return code;
}

tree
compress_tree (tree t, int spc_mode) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  if (is_format (t)) {
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= compress_tree (t[i], get_spc_mode (t, i));
    return r;
  }
  tree r= compound ("compressed", "");
  tree val (L(t));
  for (i=0; i<n; i++)
    if (the_drd->is_accessible_child (t, i) &&
        the_drd->get_env_child (t, i, MODE, "text") == "text" &&
        the_drd->get_env_child (t, i, LANGUAGE, "current") == "current" &&
        !is_compound (t, "bib-list")) {
      val << compound ("decompressed");
      r << compress_tree (t[i], get_spc_mode (t, i));
    }
    else val << t[i];

  tree key= tuple (val, as_string (spc_mode));
  if (compressed_code->contains (key)) {
    //cout << key << " <- " << compressed_code[key] << "\n";
    r[0]= compressed_code[key];
  }
  else {
    int nr= N(compressed);
    string code= "x" * as_string (nr);
    r[0]= code;
    //cout << code << ", " << N(compressed) << " -> " << key << "\n";
    compressed (code)= key;
    compressed_code (key)= code;
    //cout << "Compress " << code << " <- " << key << "\n";
  }
  return r;
}

tree
compress_tree (tree t) {
  //cout << "[decoded] " << t << LF;
  //cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << LF;
  //tree r= compress_tree (t, 0);
  //cout << "[encoded] " << r << LF;
  //cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << LF;
  //return r;
  return compress_tree (t, 0);
}

/******************************************************************************
* Decompressing non-textual markup from trees
******************************************************************************/

tree
decompress_tree (tree t, int& spc_mode) {
  if (is_atomic (t)) return t;
  int i, n= N(t);

  if (is_document (t)) {
    tree d (DOCUMENT);
    for (i=0; i<n; i++) {
      int sub_mode= 0;
      tree u= decompress_tree (t[i], sub_mode);
      if (is_document (u)) d << A(u);
      else d << u;
    }
    return d;
  }

  if (is_concat (t)) {
    tree d (DOCUMENT);
    tree c (CONCAT);
    int prev_mode= 0;
    for (i=0; i<n; i++) {
      int next_mode= 0;
      tree u= decompress_tree (t[i], next_mode);
      if ((next_mode & 1) == 0 && N(c) > 0 && is_atomic (c[N(c)-1])) {
        string s= trim_spaces_right (c[N(c)-1]->label);
        if (s == "") c= c (0, N(c) - 1);
        else c[N(c)-1]= s;
      }
      if ((next_mode & 1) != 0) {
        if (N(c) > 0 && is_atomic (c[N(c)-1])) {
          string s= c[N(c)-1]->label;
          int n= N(s);
          if (n == 0 || !is_whitespace (s (n-1, n)))
            c[N(c)-1]= s * " ";
        }
        else c << " ";
      }
      if (is_atomic (u)) {
        string s= u->label;
        if ((prev_mode & 2) == 0 && N(s) != 0)
          s= trim_spaces_left (s);
        if ((prev_mode & 2) != 0 && (N(s) == 0 || !is_whitespace (s (0, 1))))
          s= " " * s;
        if (s != "") c << s;
      }
      else {
        if ((prev_mode & 2) != 0) c << " ";
        if (is_multi_paragraph (u)) {
          if (N(c) == 0) {}
          else if (N(c) == 1) d << c[0];
          else d << c;
          if (is_document (u)) d << A(u);
          else d << u;
          c= tree (CONCAT);
        }
        else c << u;
      }
      prev_mode= next_mode;
    }
    if (N(c) == 0) c= "";
    else if (N(c) == 1) c= c[0];
    if (N(d) == 0) return c;
    if (N(d) == 1 && c == "") return d[0];
    if (c == "") return d;
    d << c;
    return d;
  }

  if (is_compound (t, "compressed") && N(t) >= 1 && is_atomic (t[0])) {
    string code= t[0]->label;
    if (compressed->contains (code)) {
      tree key= copy (compressed[code]);
      tree val= key[0];
      spc_mode= as_int (key[1]);
      //cout << "Decompress " << code << " -> " << key << "\n";
      int j=0, k=N(val);
      for (i=1; i<n; i++) {
        while (j<k && !is_compound (val[j], "decompressed", 0)) j++;
        if (j<k) {
          int sub_mode= 0;
          val[j]= decompress_tree (t[i], sub_mode);
        }
      }
      for (; j<k; j++)
        if (is_compound (val[j], "decompressed", 0))
          val[j]= "";
      return val;
    }
  }
  return "";
}

tree
decompress_tree (tree t) {
  int spc_mode= 0;
  //cout << "[encoded] " << t << LF;
  //cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << LF;
  //tree r= decompress_tree (t, spc_mode);
  //cout << "[decoded] " << r << LF;
  //cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << LF;
  //return r;
  return decompress_tree (t, spc_mode);
}

/******************************************************************************
* Check whether the compressed markup actually contains some text
******************************************************************************/

bool
compressed_contains_text (tree t) {
  if (is_atomic (t))
    return !is_whitespace (t->label);
  int i=0, n= N(t);
  if (is_compound (t, "compressed") && n > 0 && is_atomic (t[0])) i=1;
  for (; i<n; i++)
    if (compressed_contains_text (t[i]))
      return true;
  return false;
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
      if (i < (n-1) && (mode & COMPRESS_LINE_FEEDS) != 0) s << "\n";
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
compressed_to_html (tree c, int mode) {
  string r;
  compressed_as_html (r, c, mode);
  if ((mode & COMPRESS_SNIPPET) != 0) return r;
  return "<body>" * r * "</body>";
}

string
compress_html (tree t, int mode) {
  tree c= compress_tree (t);
  return compressed_to_html (c, mode);
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
  while (ends (tag, "/") || ends (tag, " "))
    tag= tag (0, N(tag) - 1);
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
  if (starts (val, "\"x"))
    // NOTE: we tolerate inconsistent quotes generated by AI
    val= val (1, N(val));
  if (starts (val, "x") && ends (val, "\""))
    // NOTE: we tolerate inconsistent quotes generated by AI
    val= val (0, N(val) - 1);
  return tag;
}

void
skip_html_space (string s, int& pos) {
  while (true) {
    if (test (s, pos, "\n")) pos++;
    else if (test (s, pos, " ")) pos++;
    else if (test (s, pos, "\\n")) pos+=2;
    else break;
  }
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
    //cout << "Old = " << r << "\n";
    r= replace (r, "&lt;", "<");
    r= replace (r, "&gt;", ">");
    r= replace (r, "&amp;", "&");
    r= html_to_utf8 (r);
    r= var_utf8_to_cork (r);
    r= replace (r, "<varspace>", " ");
    r= replace (r, "<#202F>", " ");
    //cout << "New = " << r << "\n";
    return r;
  }
  string var, val;
  string tag= parse_html_tag (s, pos, var, val);
  //cout << "tag = " << tag << ", " << var << ", " << val << "\n";
  if (tag == "body") {
    int old_pos= pos;
    skip_html_space (s, pos);
    if (test (s, old_pos, " "))
      if (!test (s, pos, "<p>"))
        pos= old_pos;
    tree r= html_as_compressed (s, pos, "</body>", mode | INSIDE_BODY);
    skip_html_space (s, pos);
    return r;
  }    
  if (tag == "p") {
    tree doc (DOCUMENT);
    doc << html_as_compressed (s, pos, "</p>", mode | INSIDE_P);
    skip_html_space (s, pos);
    while (test (s, pos, "<p>")) {
      pos += 3;
      if (test (s, pos, "\\n")) skip_html_space (s, pos);
      doc << html_as_compressed (s, pos, "</p>", mode | INSIDE_P);
      skip_html_space (s, pos);
    }
    return doc;
  }
  else if (tag == "a") {
    int old_pos= pos;
    skip_html_space (s, pos);
    if (test (s, pos, "</a>")) pos += 4;
    else pos= old_pos;
    if (var != "id") return "";
    return compound ("compressed", val);
  }
  else if (tag == "div") {
    //cout << "<DIV " << val << ">" << LF << INDENT;
    if (var != "id") return "";
    string cont= "<div id=\"cont-" * val * "\">";
    tree r= compound ("compressed", val);
    r << html_as_compressed (s, pos, "</div>", mode | INSIDE_DIV);
    //cout << r[N(r)-1] << LF;
    int old_pos= pos;
    skip_html_space (s, pos);
    while (test (s, pos, cont)) {
      pos += N(cont);
      r << html_as_compressed (s, pos, "</div>", mode | INSIDE_DIV);
      //cout << r[N(r)-1] << LF;
      old_pos= pos;
      skip_html_space (s, pos);
    }
    pos= old_pos;
    //cout << UNINDENT << "</DIV " << val << ">" << LF;
    return r;
  }
  else return "";
}

tree
html_as_compressed (string s, int& pos, string close, int mode) {
  //cout << "convert " << pos << ", " << close << "\n";
  tree r (CONCAT);
  while (pos < N(s) && !test (s, pos, close)) {
    if (test (s, pos, "</body>") && (mode & INSIDE_BODY) != 0) break;
    if (test (s, pos, "</p>") && (mode & INSIDE_P) != 0) break;
    if (test (s, pos, "</div>") && (mode & INSIDE_DIV) != 0) break;
    r << html_as_compressed (s, pos, mode);
  }
  if (pos < N(s) && test (s, pos, close)) pos += N(close);
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
decompress_html (string s, int mode) {
  int pos= 0;
  tree t= html_as_compressed (s, pos, mode);
  //cout << "t= " << t << "\n";
  return decompress_tree (t);
}

