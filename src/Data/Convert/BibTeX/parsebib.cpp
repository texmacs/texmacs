
/******************************************************************************
* MODULE     : parsebib.cpp
* DESCRIPTION: conversion of bibtex strings into logical bibtex trees
* COPYRIGHT  : (C) 2010  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "list.hpp"
#include "tree_traverse.hpp"
#include "Bibtex/bibtex_functions.hpp"

bool
bib_ok (string s, int pos) {
  return 0 <= pos && pos < N(s);
}

void
bib_char (string s, int& pos, char c) {
  if (!bib_ok (s, pos)) return;
  if (s[pos] == c) pos++;
  else {
    cerr << "TeXmacs] Error: invalid BibTeX file.\n";
    if (c) cerr << "TeXmacs] Invalid char: \'" << s[pos]
		<< "\', expected \'" << c << "\'\n";
    pos= -1;
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
  bib_char (s, pos, cbegin);
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
      a= sa;
      break;
    }
    case '{': {
      bib_within (s, pos, '{', '}', sa);
      a= sa;
      break;
    }
    default: {
      string cs= ", \t\n\r";
      cs << ce;
      if (!is_digit (s[pos])) {
        bib_until (s, pos, cs, sa);
        a= compound ("bib-var", sa);
      }
      else {
        bib_until (s, pos, cs, sa);
        a= sa;
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
    if (bib_ok (s, pos) && s[pos] == '#') pos++;
    bib_blank (s, pos);
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
    bib_char (s, pos, '=');
    bib_blank (s, pos);
    bib_arg (s, pos, ce, arg);
    if (tag == "bib-field") param= locase_all (param);
    arg= simplify_correct (arg);
    fields << compound (tag, param, arg);
    bib_blank (s, pos);
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
  switch (s[pos]) {
    case '{': cend= '}'; break;
    case '(': cend= ')'; break;
    default: pos= -1; return;
  }
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
  switch (s[pos]) {
    case '{': cend= '}'; break;
    case '(': cend= ')'; break;
    default: pos= -1; return;
  }
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
  string cs= ", \t\n\r";
  char cend;
  switch (s[pos]) {
    case '{': cend= '}'; break;
    case '(': cend= ')'; break;
    default: pos= -1; return;
  }
  pos++;
  cs << cend;
  bib_blank (s, pos);
  string tag;
  bib_until (s, pos, cs, tag);
  bib_blank (s, pos);
  string ce;
  ce << cend;
  bib_fields (s, pos, ce, string ("bib-field"), fields);
  bib_blank (s, pos);
  bib_char (s, pos, cend);
  entry= compound ("bib-entry");
  entry << type << tag << fields;
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
            if (comment) te= tree (DOCUMENT);
            else te= tentry;
            bib_entry (s, pos, stype, te);
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
//  cerr << "ENTRIES: " << tentry << "\n";
//  cerr << "PREAMBLE: " << tpreamble << "\n";
//  cerr << "STRING: " << tstring << "\n";
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
  bib_list (s, pos, r);
  if (N(s) == 0 || N(r) == 0) return tree ();
  if (pos < 0) {
    cerr << "TeXmacs] Error: failed to load BibTeX file.\n";
    return tree ();
  }
  return r;
}
