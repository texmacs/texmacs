
/******************************************************************************
* MODULE     : parse_vernac.cpp
* DESCRIPTION: conversion of vernacular strings into texmacs trees
* COPYRIGHT  : (C) 2013  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree.hpp"
#include "string.hpp"
#include "array.hpp"
#include "convert.hpp"
#include "converter.hpp"

/******************************************************************************
* Low level syntax
******************************************************************************/

/* importing verbatim strings ************************************************/

static tree
from_verbatim (string s) {
  string r;
  int i, n=N(s);
  for (i=0; i<n; i++) {
    if (s[i] == '<') r << "<less>";
    else if (s[i] == '>') r << "<gtr>";
    else if (s[i] < ' ' || s[i] > '~') break;
    else r << s[i];
  }
  if (i == n) return as_tree (r);

  tree t= verbatim_to_tree (s, true, "utf-8");
  if (is_document (t) && N(t) == 1)
    return t[0];
  return t;
}

/* Parsing blanks ************************************************************/

bool
is_blank (char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

bool
start_comment (string s, int i) {
  return test (s, i, "(*");
}

bool
end_comment (string s, int i) {
  return test (s, i, "*)");
}

/* Parsing idents ************************************************************/

bool
start_utf8_seq (char c) {
  return ((0xE0 & c) == 0xC0) || ((0xF0 & c) == 0xE0) || ((0xF8 & c) == 0xF0);
}

bool
start_ident (char c) {
  // Note: unlike Coq, we don't discriminate utf8 letters from symbols. See
  // http://coq.inria.fr/distrib/current/refman/Reference-Manual003.html#sec17
  // for details.
  return is_alpha (c) || c == '_' || start_utf8_seq (c);
}

bool
continue_ident (char c) {
  return start_ident (c) || is_digit (c) || c == '\'';
}

string
parse_identifier (string s, int &i) {
  int n= N(s), start= i;
  if (i<n && start_ident (s[i])) {
    i++;
    while (i<n && continue_ident (s[i])) i++;
  }
  return as_string (from_verbatim (s (start, i)));
}

string
parse_identifier (string s) {
  int i= 0;
  return parse_identifier (s, i);
}

/* Parsing command names *****************************************************/

string
parse_command_name (string s, int &i) {
  int n= N(s), start= i;
  if (i<n && s[i] >= 'A' && s[i] <= 'Z') {
    i++;
    while (i<n && s[i] >= 'a' && s[i] <= 'z') i++;
  }
  return s (start, i);
}

string
parse_command_name (string s) {
  int i= 0;
  return parse_command_name (s, i);
}

/******************************************************************************
* Parse raw coq
******************************************************************************/

static tree
parse_comment (string s, int& i) {
  int start= i, cnt= 1;
  i++;
  while (i < N(s) && cnt > 0) {
    if (start_comment (s, i)) cnt++;
    if (end_comment   (s, i)) cnt--;
    i++;
  }
  if (end_comment (s, i-1)) i++;
  return compound ("coq-comment", from_verbatim (s (start, i+1)));
}

static bool
end_command (string s, int i) {
  return i < N(s) && s[i] == '.'
    && (i == N(s)-1 || s[i+1] == ' ' || s[i+1] == '\t' || s[i+1] == '\n');
}

static int
parse_indent (string s, int i) {
  int indent= 0, n= N(s);
  while (i<n && (s[i] == ' ' || s[i] == '\t')) i++, indent++;
  if (i<n && s[i] != '\n') return indent;
  else return -1;
}

static tree
parse_raw_coq (string s) {
  tree r (DOCUMENT);
  int i= 0, startcmd= 0, n= N(s), indent_level=0;
  bool in_cmd= false;
  while (i<n) {
    if (start_comment (s, i))
      r << parse_comment (s, i);
    else if (end_command (s, i)) {
      i++;
      r << compound ("coq-command", "", "dark grey",
          from_verbatim (s (startcmd, i)));
      in_cmd= false;
    }
    else if (!in_cmd && s[i] == '\n') {
      i++;
      int tmp= parse_indent (s, i);
      if (tmp >= 0 && tmp != indent_level) {
        r << compound ("coq-indent", as_string (tmp));
        indent_level= tmp;
      }
    }
    else {
      if (!in_cmd && !is_blank (s[i])) {
        in_cmd= true;
        startcmd= i;
      }
      i++;
    }
  }
  return r;
}

/******************************************************************************
* Indent parsed coq
******************************************************************************/

static int
get_indent (tree t) {
  if (is_compound (t, "coq-indent", 1))
    return as_int (t[0]);
  return -1;
}

static tree
indent_parsed_coq (tree t, int base_indent=0) {
  if (is_atomic (t) || is_compound (t, "coq-indent")
                    || is_compound (t, "coq-command")
                    || is_compound (t, "coq-comment")) return t;
  tree r (L(t));
  int i, n= N(t);
  for (i=0; i<n; i++) {
    if (get_indent (t[i]) > base_indent) {
      tree body (DOCUMENT);
      int indent= get_indent (t[i++]);
      bool stop= false;
      while (i<n && !stop) {
        int curr_ind= get_indent (t[i]);
        if (curr_ind > -1 && (curr_ind <= indent || curr_ind < base_indent)) {
          i--;
          stop= true;
        }
        else body << t[i++];
      }
      r << compound ("indent", indent_parsed_coq (body, indent));
    }
    else if (get_indent (t[i]) == base_indent);
    else
      r << indent_parsed_coq (t[i], base_indent);
  }
  return r;
}

/******************************************************************************
* Section parsed coq
******************************************************************************/

static bool
is_begin_section (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= as_string (t[2]);
  return parse_command_name (s) == "Section";
}

static bool
is_end (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= as_string (t[2]);
  return parse_command_name (s) == "End";
}

static string
get_section_name (tree t) {
  if (N(t) != 3) return "";
  string s= as_string (t[2]);
  int i= 0, n= N(s);
  string cmd= parse_command_name (s, i);
  if (cmd != "Section" && cmd != "End") return "";
  while (i<n && is_blank (s[i])) i++;
  string ident= parse_identifier (s, i);
  return ident;
}

static bool
is_end_section (tree t, string name) {
  return is_end (t) && get_section_name (t) == name;
}

static tree
section_parsed_coq (tree t) {
  tree r (DOCUMENT);
  int i, n= N(t);
  for (i=0; i<n; i++) {
    if (is_begin_section (t[i])) {
      string name= get_section_name (t[i++]);
      tree body (DOCUMENT);
      while (i<n && !is_end_section (t[i], name)) body << t[i++];
      r << compound ("coq-section", name, body);
    }
    else
      r << t[i];
  }
  return r;
}

/******************************************************************************
* Enonciate parsed coq
******************************************************************************/

static bool
is_enunciation (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= parse_command_name (as_string (t[2]));
  return s == "Lemma" || s == "Remark" || s == "Fact" || s == "Corollary" ||
    s == "Proposition" || s == "Theorem";
}

static bool
is_proof (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= parse_command_name (as_string (t[2]));
  return s == "Proof";
}

static bool
is_end_proof (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= parse_command_name (as_string (t[2]));
  return s == "Qed" || s == "Admitted" || s == "Defined";
}

static tree
parse_enunciation (string s) {
  int i= 0, n= N(s);
  string kind= parse_command_name (s, i);
  while (i<n && is_blank (s[i])) i++;
  string name= parse_identifier (s, i);
  while (i<n && is_blank (s[i])) i++;
  if (s[i] == ':') i++;
  while (i<n && is_blank (s[i])) i++;
  string body= s (i, n);
  tree r= compound ("coq-enunciation", "", "dark grey");
  r << kind << name << body;
  return r;
}

static tree
enunciate_parsed_coq (tree t) {
  if (is_atomic (t) || is_compound (t, "coq-indent")
                    || is_compound (t, "coq-command")
                    || is_compound (t, "coq-comment")) return t;
  tree r (L(t));
  int i, n= N(t);
  for (i=0; i<n; i++) {
    if (is_enunciation (t[i])) {
      tree enun= parse_enunciation (as_string (t[i++][2]));
      tree proof (DOCUMENT);
      while (i<n && !is_end_proof (t[i])) {
        if (!is_proof (t[i]))
          proof << t[i];
        i++;
      }
      enun << proof;
      r << enun;
    }
    else
      r << enunciate_parsed_coq (t[i]);
  }
  return r;
}

static tree
clean_indent (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  for (i=0; i<n; i++) {
    if (is_compound (t[i], "coq-enunciation", 6)) {
      if (is_document (t[i][5]) && N(t[i][5]) == 1) {
        if (is_compound (t[i][5][0], "indent", 1)) {
          t[i][5]= t[i][5][0][0];
        }
      }
      else if (is_compound (t[i][5], "indent", 1)) {
        t[i][5]= t[i][5][0];
      }
    }
    else
      t[i]= clean_indent (t[i]);
  }
  return t;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
vernac_to_tree (string s) {
  tree r (DOCUMENT);
  r= parse_raw_coq (s);
  r= section_parsed_coq (r);
  r= enunciate_parsed_coq (r);
  r= indent_parsed_coq (r);
  r= clean_indent (r);
  if (N(r) == 0) r << "";
  return r;
}

tree
vernac_document_to_tree (string s) {
  tree style= compound ("style", tuple ("generic", "coq"));
  tree body= compound ("body", vernac_to_tree (s));
  return document (body, style);
}
