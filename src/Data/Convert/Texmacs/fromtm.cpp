
/******************************************************************************
* MODULE     : fromtm.cpp
* DESCRIPTION: conversion from the TeXmacs file format to TeXmacs trees
*              older versions are automatically converted into the present one
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "path.hpp"
#include "vars.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Conversion of TeXmacs strings of the present format to TeXmacs trees
******************************************************************************/

struct tm_reader {
  string  version;            // document was composed using this version
  hashmap<string,int> codes;  // codes for to present version
  tree_label EXPAND_APPLY;    // APPLY (version < 0.3.3.22) or EXPAND (otherw)
  bool    backslash_ok;       // true for versions >= 1.0.1.23
  bool    with_extensions;    // true for versions >= 1.0.2.4
  string  buf;                // the string being read from
  int     pos;                // the current position of the reader
  string  last;               // last read string

  tm_reader (string buf2):
    version (TEXMACS_VERSION),
    codes (STD_CODE),
    EXPAND_APPLY (EXPAND),
    backslash_ok (true),
    with_extensions (true),
    buf (buf2), pos (0), last ("") {}
  tm_reader (string buf2, string version2):
    version (version2),
    codes (get_codes (version)),
    EXPAND_APPLY (version_inf (version, "0.3.3.22")? APPLY: EXPAND),
    backslash_ok (version_inf (version, "1.0.1.23")? false: true),
    with_extensions (version_inf (version, "1.0.2.4")? false: true),
    buf (buf2), pos (0), last ("") {}

  int    skip_blank ();
  string decode (string s);
  string read_char ();
  string read_next ();
  string read_function_name ();
  tree   read_apply (string s, bool skip_flag);
  tree   read (bool skip_flag);
};

int
tm_reader::skip_blank () {
  int n=0;
  for (; pos < N(buf); pos++) {
    if (buf[pos]==' ') continue;
    if (buf[pos]=='\t') continue;
    if (buf[pos]=='\r') continue;
    if (buf[pos]=='\n') { n++; continue; }
    break;
  }
  return n;
}

string
tm_reader::decode (string s) {
  int i, n=N(s);
  string r;
  for (i=0; i<n; i++)
    if (((i+1)<n) && (s[i]=='\\')) {
      i++;
      if (s[i] == ';');
      else if (s[i] == '0') r << '\0';
      else if (s[i] == 't') r << '\t';
      else if (s[i] == 'r') r << '\r';
      else if (s[i] == 'n') r << '\n';
      else if (s[i] == '\\') r << '\\';
      else if ((s[i] >= '@') && (s[i] < '`')) r << (s[i] - '@');
      else r << s[i];
    }
    else r << s[i];
  return r;
}

string
tm_reader::read_char () {
  while (((pos+1) < N(buf)) && (buf[pos] == '\\') && (buf[pos+1] == '\n')) {
    pos += 2;
    skip_spaces (buf, pos);
  }
  if (pos >= N(buf)) return "";
  pos++;
  return buf (pos-1, pos);
}

string
tm_reader::read_next () {
  int old_pos= pos;
  string c= read_char ();
  if (c == "") return c;
  switch (c[0]) {
  case '\t':
  case '\n':
  case '\r':
  case ' ': 
    pos--;
    if (skip_blank () <= 1) return " ";
    else return "\n";
  case '<':
    {
      old_pos= pos;
      c= read_char ();
      if (c == "") return "";
      if (c == "#") return "<#";
      if ((c == "\\") || (c == "|") || (c == "/")) return "<" * c;
      if (is_iso_alpha (c[0]) || (c == ">")) {
        pos= old_pos;
        return "<";
      }
      pos= old_pos;
      return "<";
      /*
      string d= read_char ();
      if ((d == "\\") || (d == "|") || (d == "/")) return "<" * c * d;
      pos= old_pos;
      return "<" * c;
      */
    }
  case '|':
  case '>':
    return c;
  }

  string r;
  pos= old_pos;
  while (true) {
    old_pos= pos;
    c= read_char ();
    if (c == "") return r;
    else if (c == "\\") {
      if ((pos < N(buf)) && (buf[pos] == '\\') && backslash_ok) {
        r << c << "\\";
        pos++;
      }
      else r << c << read_char ();
    }
    else if (c == "\t") break;
    else if (c == "\r") break;
    else if (c == "\n") break;
    else if (c == " ") break;
    else if (c == "<") break;
    else if (c == "|") break;
    else if (c == ">") break;
    else r << c;
  }
  pos= old_pos;
  return r;
}

string
tm_reader::read_function_name () {
  string name= decode (read_next ());
  // cout << "==> " << name << "\n";
  while (true) {
    last= read_next ();
    // cout << "~~> " << last << "\n";
    if ((last == "") || (last == "|") || (last == ">")) break;
  }
  return name;
}

static void
get_collection (tree& u, tree t) {
  if (is_func (t, COLLECTION) ||
           is_func (t, DOCUMENT) ||
           is_func (t, CONCAT)) {
    int i;
    for (i=0; i<N(t); i++)
      get_collection (u, t[i]);
  }
  else if (is_compound (t)) u << t;
}

tree
tm_reader::read_apply (string name, bool skip_flag) {
  // cout << "Read apply " << name << INDENT << LF;
  tree t (make_tree_label (name));
  if (!with_extensions)
    t= tree (EXPAND_APPLY, name);
  if (codes->contains (name)) {
    // cout << "  " << name << " -> " << as_string ((tree_label) codes [name]) << "\n";
    t= tree ((tree_label) codes [name]);
  }

  bool closed= !skip_flag;
  while (pos < N(buf)) {
    // cout << "last= " << last << LF;
    bool sub_flag= (skip_flag) && ((last == "") || (last[N(last)-1] != '|'));
    if (sub_flag) (void) skip_blank ();
    t << read (sub_flag);
    if ((last == "/>") || (last == "/|")) closed= true;
    if (closed && ((last == ">") || (last == "/>"))) break;
  }
  // cout << "last= " << last << UNINDENT << LF;
  // cout << "Done" << LF;

  if (is_func (t, COLLECTION)) {
    tree u (COLLECTION);
    get_collection (u, t);
    return u;
  }
  return t;
}

static void
flush (tree& D, tree& C, string& S, bool& spc_flag, bool& ret_flag) {
  if (spc_flag) S << " ";
  if (S != "") {
    if ((N(C) == 0) || (!is_atomic (C[N(C)-1]))) C << S;
    else C[N(C)-1]->label << S;
    S= "";
    spc_flag= false;
  }

  if (ret_flag) {
    if (N(C) == 0) D << "";
    else if (N(C) == 1) D << C[0];
    else D << C;
    C= tree (CONCAT);
    ret_flag= false;
  }
}

tree
tm_reader::read (bool skip_flag) {
  tree   D (DOCUMENT);
  tree   C (CONCAT);
  string S ("");
  bool   spc_flag= false;
  bool   ret_flag= false;

  while (true) {
    last= read_next ();
    // cout << "--> " << last << "\n";
    if (last == "") break;
    if (last == "|") break;
    if (last == ">") break;
    
    if (last[0] == '<') {
      if (last[N(last)-1] == '\\') {
        flush (D, C, S, spc_flag, ret_flag);
        string name= read_function_name ();
        if (last == ">") last= "\\>";
        else last= "\\|";
        C << read_apply (name, true);
      }
      else if (last[N(last)-1] == '|') {
        (void) read_function_name ();
        if (last == ">") last= "|>";
        else last= "||";
        break;
      }
      else if (last[N(last)-1] == '/') {
        (void) read_function_name ();
        if (last == ">") last= "/>";
        else last= "/|";
        break;
      }
      else if (last[N(last)-1] == '#') {
        string r;
        while ((buf[pos] != '>') && (pos+2<N(buf))) {
          r << ((char) from_hexadecimal (buf (pos, pos+2)));
          pos += 2;
        }
        if (buf[pos] == '>') pos++;
        flush (D, C, S, spc_flag, ret_flag);
        C << tree (RAW_DATA, r);
        last= read_next ();
        break;
      }
      else {
        flush (D, C, S, spc_flag, ret_flag);
        string name= decode (read_next ());
        string sep = ">";
        if (name == ">") name= "";
        else sep = read_next ();
        // cout << "==> " << name << "\n";
        // cout << "~~> " << sep << "\n";
        if (sep == '|') {
          last= "|";
          C << read_apply (name, false);
        }
        else {
          tree t (make_tree_label (name));
          if (!with_extensions)
            t= tree (EXPAND_APPLY, name);
          if (codes->contains (name)) {
            // cout << name << " -> " << as_string ((tree_label) codes [name]) << "\n";
            t= tree ((tree_label) codes [name]);
          }
          C << t;
        }
      }
    }
    else if (last == " ") spc_flag= true;
    else if (last == "\n") ret_flag= true;
    else {
      flush (D, C, S, spc_flag, ret_flag);
      // cout << "<<< " << last << "\n";
      // cout << ">>> " << decode (last) << "\n";
      S << decode (last);
      if ((S == "") && (N(C) == 0)) C << "";
    }
  }

  if (skip_flag) spc_flag= ret_flag= false;
  flush (D, C, S, spc_flag, ret_flag);
  if (N(C) == 1) D << C[0];
  else if (N(C)>1) D << C;
  // cout << "*** " << D << "\n";
  if (N(D)==0) return "";
  if (N(D)==1) {
    if (!skip_flag) return D[0];
    if (version_inf_eq (version, "0.3.4.10")) return D[0];
    if (is_func (D[0], COLLECTION)) return D[0];
  }
  return D;
}

tree
texmacs_to_tree (string s) {
  tm_reader tmr (s);
  return tmr.read (true);
}

tree
texmacs_to_tree (string s, string version) {
  tm_reader tmr (s, version);
  return tmr.read (true);
}

/******************************************************************************
* Conversion of TeXmacs strings to TeXmacs trees
******************************************************************************/

inline bool is_apply (tree t, string s, int n) {
  return (L(t) == APPLY) && (N(t) == (n+1)) && (t[0] == s); }

static bool
is_expand (tree t, string s, int n) {
  return (L(t) == EXPAND) && (N(t) == n+1) && (t[0] == s);
}

tree
texmacs_document_to_tree (string s) {
  tree error (ERROR, "bad format or data");
  if (starts (s, "edit") ||
      starts (s, "TeXmacs") ||
      starts (s, "\\(\\)(TeXmacs"))
  {
    string version= "0.0.0.0";
    tree t= string_to_tree (s, version);
    if (is_tuple (t) && (N(t)>0)) t= t (1, N(t));
    int n= arity (t);

    tree doc (DOCUMENT);
    doc << compound ("TeXmacs", version);
    if (n<3) return error;
    else if (n<4)
      doc << compound ("body", t[2])
          << compound ("style", t[0])
          << compound ("initial", t[1]);
    else if (n<7)
      doc << compound ("body", t[0])
          << compound ("style", t[1])
          << compound ("initial", t[2])
          << compound ("references", t[3]);
    else
      doc << compound ("body", t[0])
          << compound ("project", t[1])
          << compound ("style", t[2])
          << compound ("initial", t[3])
          << compound ("final", t[4])
          << compound ("references", t[5])
          << compound ("auxiliary", t[6]);
    return upgrade (doc, version);
  }

  if (starts (s, "<TeXmacs|")) {
    int i;
    for (i=9; i<N(s); i++)
      if (s[i] == '>') break;
    string version= s (9, i);
    tree doc= texmacs_to_tree (s, version);
    if (is_compound (doc, "TeXmacs", 1) ||
        is_expand (doc, "TeXmacs", 1) ||
        is_apply (doc, "TeXmacs", 1))
      doc= tree (DOCUMENT, doc);
    if (!is_document (doc)) return error;
    if (N(doc) == 0 || !is_compound (doc[0], "TeXmacs", 1)) {
      tree d (DOCUMENT);
      d << compound ("TeXmacs", version);
      d << A(doc);
      doc= d;
    }
    return upgrade (doc, version);
  }
  return error;
}

/******************************************************************************
* Extracting attributes from a TeXmacs document tree
******************************************************************************/

tree
extract (tree doc, string attr) {
  int i, n= arity (doc);
  for (i=0; i<n; i++)
    if (is_compound (doc[i], attr, 1) ||
        is_expand (doc[i], attr, 1) ||
        is_apply (doc[i], attr, 1))
      {
        tree r= doc[i][N(doc[i])-1];
        if ((attr == "body") && (!is_document (r))) return tree (DOCUMENT, r);
        if (attr == "style") {
          if (r == "none") return tree (TUPLE);
          if (r == "") return tree (TUPLE);
          if (r == "style") return tree (TUPLE);
          if (is_atomic (r)) return tree (TUPLE, r);
          if (!is_func (r, TUPLE)) return tree (TUPLE);
        }
        return r;
      }

  if (attr == "TeXmacs") return "";
  if (attr == "body") return tree (DOCUMENT, "");
  if (attr == "project") return "";
  if (attr == "style") return tree (TUPLE);
  if (attr == "initial") return tree (COLLECTION);
  if (attr == "final") return tree (COLLECTION);
  if (attr == "references") return tree (COLLECTION);
  if (attr == "auxiliary") return tree (COLLECTION);
  if (attr == "attachments") return tree (COLLECTION);
  return "";
}

tree
extract_document (tree doc) {
  if (is_func (doc, ERROR)) return doc;
  bool prj = (extract (doc, "project") != "");
  tree body= extract (doc, "body");
  tree init= extract (doc, "initial");
  if (is_func (init, COLLECTION)) {
    tree w (WITH);
    int i, n= N(init);
    for (i=0; i<n; i++)
      if (is_func (init[i], ASSOCIATE, 2)) {
        tree l= init[i][0];
        tree r= init[i][1];
        if ((l == PAGE_MEDIUM) ||
            (l == PAGE_PRINTED) ||
            (l == PAGE_TYPE) ||
            (l == PAGE_ORIENTATION) ||
            (l == PAGE_WIDTH_MARGIN) ||
            (l == PAGE_SCREEN_MARGIN) ||
            (l == PAGE_NR) ||
            (l == PAGE_WIDTH) ||
            (l == PAGE_HEIGHT) ||
            (l == PAGE_ODD) ||
            (l == PAGE_EVEN) ||
            (l == PAGE_RIGHT) ||
            (l == PAGE_ODD_SHIFT) ||
            (l == PAGE_EVEN_SHIFT) ||
            (l == PAGE_TOP) ||
            (l == PAGE_BOT) ||
            (l == PAGE_SCREEN_WIDTH) ||
            (l == PAGE_SCREEN_HEIGHT) ||
            (l == PAGE_SCREEN_LEFT) ||
            (l == PAGE_SCREEN_RIGHT) ||
            (l == PAGE_SCREEN_TOP) ||
            (l == PAGE_SCREEN_BOT) ||
            (l == PAGE_SHOW_HF) ||
            (l == PAGE_FIRST && prj) ||
            (l == "chapter-nr" && prj) ||
            (l == "section-nr" && prj) ||
            (l == "subsection-nr" && prj) ||
            (l == "subsubsection-nr" && prj)) continue;
        w << l << r;
      }
    if (N(w)>0) {
      w << body;
      body= w;
    }
  }
  return body;
}

tree
change_doc_attr (tree doc, string attr, tree val) {
  int i, n= arity (doc);
  tree r (doc, n);
  bool done= false;
  for (i=0; i<n; i++)
    if (is_compound (doc[i], attr, 1)) {
      r[i]= tree (L(doc[i]), val);
      done= true;
    }
    else r[i]= doc[i];
  if (!done) r << compound (attr, val);
  return r;
}

tree
remove_doc_attr (tree doc, string attr) {
  int i, n= arity (doc);
  tree r (L(doc));
  for (i=0; i<n; i++)
    if (!is_compound (doc[i], attr, 1))
      r << doc[i];
  return r;
}

/******************************************************************************
* Extracting metadata
******************************************************************************/

static tree
search_tag_quick (tree t, string tag) {
  if (is_compound (t, tag)) return t;
  if (!(is_func (t, DOCUMENT) || is_func (t, CONCAT) ||
        is_func (t, SURROUND) || is_func (t, WITH))) return "";
  for (int i=0; i<N(t); i++) {
    tree r= search_tag_quick (t[i], tag);
    if (r != "") return r;
  }
  return "";
}

static tree
search_tag (tree t, string tag) {
  if (is_atomic (t)) return tuple ();
  else if (is_compound (t, tag, 1)) return tuple (t[0]);
  else {
    tree r (TUPLE);
    for (int i=0; i<N(t); i++) {
      tree f= search_tag (t[i], tag);
      r << A(f);
    }
    return r;
  }
}

static string
search_metadata_tag (tree doc, string tag) {
  string r;
  tree t= search_tag (doc, tag);
  for (int i=0; i<N(t); i++) {
    if (N(r) != 0) r << ", ";
    r << tree_to_verbatim (t[i], false, "cork");
  }
  return r;
}

string
search_metadata (tree doc, string kind) {
  tree dd= search_tag_quick (doc, "doc-data");
  if (dd != "") {
    if (kind == "title")
      return search_metadata_tag (dd, "doc-title");
    if (kind == "author")
      return search_metadata_tag (dd, "author-name");
  }
  if (kind == "title") {
    tree t= search_tag_quick (doc, "tmdoc-title");
    if (t != "") return tree_to_verbatim (t[0], false, "cork");
  }
  return "";
}
