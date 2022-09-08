
/******************************************************************************
* MODULE     : parsebib.cpp
* DESCRIPTION: conversion of bibtex strings into logical bibtex trees
* COPYRIGHT  : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "wencoding.hpp"
#include "analyze.hpp"
#include "list.hpp"
#include "tree_traverse.hpp"
#include "Bibtex/bibtex_functions.hpp"

static string bib_current_tag= "";

bool
bibtex_non_empty_comment (string s) {
  for (int i=0; i<N(s); i++)
    if (is_alpha (s[i]) || is_numeric (s[i])) return true;
  return false;
}

bool
bib_ok (string s, int pos) {
  return 0 <= pos && pos < N(s);
}

void
bib_error () {
  if (bib_current_tag == "")
    convert_error << "BibTeX parse error encountered\n";
  else
    convert_error << "BibTeX parse error in " << bib_current_tag << "\n";
}

int
bib_char (string s, int& pos, char c) {
  if (!bib_ok (s, pos)) return -1;
  if (s[pos] == c) pos++;
  else {
    bib_error ();
    if (c) convert_error << "Invalid char: \'" << s[pos]
                         << "\', expected \'" << c << "\'\n";
    return -1;
  }
  return 0;
}

bool
bib_open (string s, int& pos, char& cend) {
  switch (s[pos]) {
  case '{': cend= '}'; return false;
  case '(': cend= ')'; return false;
  default:
    bib_error ();
    convert_error << "Expected '{' or '(' instead of '" << s[pos] << "'\n";
    while (pos < N(s) && s[pos] != '{' && s[pos] != '(') pos++;
    if (pos < N(s)) return bib_open (s, pos, cend);
    return true;
  }
}

bool
bib_is_in (char c, string cs) {
  int i= 0;
  while (i < N(cs) && cs[i] != c) i++;
  return i != N(cs);
}

void
bib_blank (string s, int& pos) {
  if (!bib_ok (s, pos)) return;
  string cs= " \t\n\r";
  while (bib_ok (s, pos) && bib_is_in (s[pos], cs)) pos++;
}

void
bib_within (string s, int& pos, char cbegin, char cend, string& content) {
  if (!bib_ok (s, pos)) return;
  int depth= 0;
  if (bib_char (s, pos, cbegin))
    return;
  while (bib_ok (s, pos) && (s[pos] != cend || depth > 0)) {
    if (cbegin != cend) {
      if (s[pos] == cbegin) depth++;
      else if (s[pos] == cend) depth--;
    }
    if (s[pos] == '\\' && bib_ok (s, pos+1)) {
      content << '\\';
      pos++;
    }
    content << s[pos];
    pos++;
  }
  bib_char (s, pos, cend);
}

void
bib_until (string s, int& pos, string cs, string& content) {
  if (!bib_ok (s, pos)) return;
  while (bib_ok (s, pos) && !bib_is_in (s[pos], cs)) {
    content << s[pos];
    pos++;
  }
}

void
bib_comment (string s, int& pos, tree& t) {
  if (!bib_ok (s, pos)) return;
  string content;
  while (bib_ok (s, pos) && s[pos] == '%') {
    bib_char (s, pos, '%');
    while (bib_ok (s, pos) && s[pos] != '\n') {
      content << s[pos];
      pos++;
    }
    t << compound ("bib-line", content);
    content= "";
    pos++;
  }
}

void bib_atomic_arg (string s, int& pos, string ce, tree& a) {
  if (!bib_ok (s, pos)) return;
  string sa;
  string f, v, j, l;
  switch (s[pos]) {
    case '\"': {
      bib_within (s, pos, '\"', '\"', sa);
      a= western_to_cork (sa);
      break;
    }
    case '{': {
      bib_within (s, pos, '{', '}', sa);
      a= western_to_cork (sa);
      break;
    }
    default: {
      string cs= ", \t\n\r";
      cs << ce;
      if (!is_digit (s[pos])) {
        bib_until (s, pos, cs, sa);
        a= compound ("bib-var", western_to_cork (sa));
      }
      else {
        bib_until (s, pos, cs, sa);
        a= western_to_cork (sa);
      }
      break;
    }
  }
}

void
bib_arg (string s, int& pos, string ce, tree& arg) {
  if (!bib_ok (s, pos)) return;
  string cs= ",";
  cs << ce;
  while (bib_ok (s, pos) && !bib_is_in (s[pos], cs)) {
    tree a;
    bib_atomic_arg (s, pos, ce, a);
    arg << a;
    bib_blank (s, pos);
    if (bib_ok (s, pos) && s[pos] == '#') {
      pos++;
      bib_blank (s, pos);
    }
    else break;
  }
}

tree
normalize_newlines (tree t) {
  if (is_atomic (t)) {
    string s= t->label, r;
    for (int i=0; i<N(s); )
      if (s[i] == '\n') {
        r << " ";
        i++;
        while (i<N(s) && s[i] == ' ') i++;
      }
      else r << s[i++];
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= normalize_newlines (t[i]);
    return r;
  }
}

void
bib_fields (string s, int& pos, string ce, string tag, tree& fields) {
  if (!bib_ok (s, pos)) return;
  int savpos;
  bib_blank (s, pos);
  while (bib_ok (s, pos) && s[pos] == ',') {
    pos++;
    bib_blank (s, pos);
  }
  while (bib_ok (s, pos) && !bib_is_in (s[pos], ce)) {
    savpos= pos;
    string param;
    tree arg (CONCAT);
    bib_until (s, pos, string ("={( \t\n\r"), param);
    if (bib_ok (s, pos) && (s[pos]=='{' || s[pos]=='(')) {
      pos= savpos;
      return;
    }
    bib_blank (s, pos);
    if (bib_char (s, pos, '='))
      return;
    bib_blank (s, pos);
    bib_arg (s, pos, ce, arg);
    if (tag == "bib-field") param= locase_all (param);
    arg= simplify_correct (arg);
    arg= normalize_newlines (arg);
    fields << compound (tag, param, arg);
    bib_blank (s, pos);
    string cend= ce;
    cend << ",";
    while (bib_ok (s, pos) && !bib_is_in (s[pos], cend)) pos++;
    while (bib_ok (s, pos) && s[pos] == ',') {
      pos++;
      bib_blank (s, pos);
    }
  }
}

void
bib_string (string s, int& pos, tree& t) {
  if (!bib_ok (s, pos)) return;
  tree fields= tree (DOCUMENT);
  string cs= ", \t\n\r";
  char cend;
  if (bib_open (s, pos, cend)) return;
  pos++;
  cs << cend;
  bib_blank (s, pos);
  string ce;
  ce << cend;
  bib_fields (s, pos, ce, string ("bib-assign"), fields);
  bib_blank (s, pos);
  bib_char (s, pos, cend);
  t << A (fields);
}

void
bib_preamble (string s, int& pos, tree& t) {
  if (!bib_ok (s, pos)) return;
  string cs= ",";
  char cend;
  if (bib_open (s, pos, cend)) return;
  pos++;
  cs << cend;
  bib_blank (s, pos);
  while (bib_ok (s, pos) && s[pos] == ',') {
    pos++;
    bib_blank (s, pos);
  }
  while (bib_ok (s, pos) && s[pos] != cend) {
    bib_blank (s, pos);
    tree arg (CONCAT);
    bib_arg (s, pos, cs, arg);
    arg= simplify_correct (arg);
    t << compound ("bib-latex", arg);
    bib_blank (s, pos);
    while (bib_ok (s, pos) && s[pos] == ',') {
      pos++;
      bib_blank (s, pos);
    }
  }
  bib_blank (s, pos);
  bib_char (s, pos, cend);
}

void
bib_entry (string s, int& pos, tree type, tree& t) {
  if (!bib_ok (s, pos)) return;
  tree entry;
  tree fields= tree (DOCUMENT);
  string cs= ",\t\n\r";
  char cend;
  if (bib_open (s, pos, cend)) return;
  pos++;
  cs << cend;
  bib_blank (s, pos);
  string tag;
  bib_until (s, pos, cs, tag);
  bib_current_tag= copy (tag);
  bib_blank (s, pos);
  string ce;
  ce << cend;
  bib_fields (s, pos, ce, string ("bib-field"), fields);
  bib_blank (s, pos);
  bib_char (s, pos, cend);
  entry= compound ("bib-entry");
  entry << type << utf8_to_cork (tag) << fields;
  t << entry;
}

void
bib_list (string s, int& pos, tree& t) {
  if (!bib_ok (s, pos)) return;
  tree tentry (DOCUMENT);
  tree tpreamble (DOCUMENT);
  tree tstring (DOCUMENT);
  string type;
  bool comment= true;
  int savpos;
  while (bib_ok (s, pos)) {
    bib_blank (s, pos);
    if (!bib_ok (s, pos)) break;
    switch (s[pos]) {
      case '%': {
        tree tc= tree (DOCUMENT);
        bib_comment (s, pos, tc);
        tentry << compound ("bib-comment", tc);
        break;
      }
      case '@': {
        pos++;
        comment= false;
      }
      default: {
        bib_blank (s, pos);
        savpos= pos;
        type= "";
        bib_until (s, pos, string ("{(= \t\n\r"), type);
        bib_blank (s, pos);
        if (bib_ok (s, pos) && s[pos] == '=') {
          tree fields (DOCUMENT);
          pos= savpos;
          bib_fields (s, pos, string (")}@"), string ("bib-field"), tentry);
          bib_blank (s, pos);
          if (bib_ok (s, pos) && (s[pos]==')' || s[pos]=='}')) {
            if (N(tpreamble) != 0) t << compound ("bib-preamble", tpreamble);
            if (N(tstring) != 0) t << compound ("bib-string", tstring);
            t << A(tentry);
            return;
          }
        }
        else {
          string stype= locase_all (type);
          if (stype == "string") {
            tree ts;
            if (comment) ts= tree (DOCUMENT);
            else ts= tstring;
            bib_string (s, pos, ts);
            if (comment) {
              if (N(ts) == 1) tstring << compound ("bib-comment", ts[0]);
              else tstring << compound ("bib-comment", ts);
            }
          }
          else if (stype == "preamble") {
            tree tp;
            if (comment) tp= tree (DOCUMENT);
            else tp= tpreamble;
            bib_preamble (s, pos, tp);
            if (comment) {
              if (N(tp) == 1) tpreamble << compound ("bib-comment", tp[0]);
              else tpreamble << compound ("bib-comment", tp);
            }
          }
          else {
            tree te;
            if (stype == "comment") comment= true;
            if (comment) te= tree (DOCUMENT);
            else te= tentry;
            bib_entry (s, pos, stype, te);
            /* dirty hack to get comments between entries */
            int start= pos;
            while (pos+1 < N(s) && s[pos+1] != '@') pos++;
            if (bibtex_non_empty_comment (s(start, pos+1))) {
              string ss= western_to_cork (s(start, pos+1));
              array<string> lines= tokenize (ss, "\n");
              tree doc (DOCUMENT);
              for (int l=0; l<N(lines); l++)
                if (trim_spaces (lines[l]) != "")
                  doc << tree (lines[l]);
              if (N(doc) != 0)
                tentry << compound ("bib-comment", doc);
            }
            /* end */
            if (comment) {
              if (N(te) == 1) tentry << compound ("bib-comment", te[0]);
              else tentry << compound ("bib-comment", te);
            }
          }
          comment= true;
        }
        break;
      }
    }
  }
//  convert_error << "ENTRIES: " << tentry << "\n";
//  convert_error << "PREAMBLE: " << tpreamble << "\n";
//  convert_error << "STRING: " << tstring << "\n";
//  if (N(tpreamble) != 0) t << compound ("bib-preamble", tpreamble);
//  if (N(tstring) != 0) t << compound ("bib-string", tstring);
//  t << A(tentry);
  hashmap<string,string> dict=
    bib_strings_dict (tree (DOCUMENT, compound ("bib-string", tstring)));
  t << A(bib_subst_vars (tentry, dict));
  bib_parse_fields (t);
}

tree
parse_bib (string s) {
  int pos= 0;
  tree r (DOCUMENT);
  bib_current_tag= "";
  bib_list (s, pos, r);
  if (N(s) == 0 || N(r) == 0) return tree ();
  if (pos < 0) {
    convert_error << "Failed to load BibTeX file.\n";
    return tree ();
  }
  return r;
}
