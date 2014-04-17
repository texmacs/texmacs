
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
from_verbatim (string s, bool wrap= true) {
  string r;
  int i, n=N(s);
  for (i=0; i<n; i++) {
    if (s[i] == '<') r << "<less>";
    else if (s[i] == '>') r << "<gtr>";
    else if (s[i] < ' ' || s[i] > '~') break;
    else r << s[i];
  }
  if (i == n) return as_tree (r);

  tree t= verbatim_to_tree (s, wrap, "utf-8");
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
is_blank (string s) {
  int i= 0, n= N(s);
  while (i<n)
    if (!is_blank (s[i++]))
      return false;
  return true;
}

bool
is_blank (tree t) {
  if (is_atomic (t))
    return is_blank (as_string (t));
  int i= 0, n= N(t);
  if (is_compound (t, "with"))
    return is_blank (t[n-1]);
  else if (is_document (t) || is_concat (t)) {
    while (i<n)
      if (!is_blank (t[i++]))
        return false;
    return true;
  }
  else
    return false;
}

bool
start_coqdoc (string s, int i) {
  return test (s, i, "(** ");
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
* Parse coqdoc
******************************************************************************/

static tree coqdoc_to_tree (string s);

static string
parse_delimited (string s, int& i, char c) {
  int start= i + 1, n= N(s);
  i++;
  while (i < n) {
    if (s[i] == c && !(i+1 == n || s[i+1] == c))
      break;
    i++;
  }
  if (s[i] == c) i++;
  return s (start, i-1);
}

static string
parse_delimited (string s, int& i, string beg, string end, bool keep= true) {
  int start= i + (!keep)*N(beg), cnt= 1;
  i++;
  while (i < N(s) && cnt > 0) {
    if (test (s, i, beg)) cnt++;
    if (test (s, i, end)) cnt--;
    i++;
  }
  if (test (s, i-1, end)) i+=N(end)-1;
  return s (start, i - (!keep)*N(end));
}

static void
add_line(tree &c, tree &d) {
  tree tmp= simplify_concat (c);
  if (!is_blank (c))
    d << simplify_concat (c);
  c= tree (CONCAT);
}

/* Parse emphasis ************************************************************/

static tree
coqdoc_parse_emphasis (string s, int &i) {
  int n= N(s), start= ++i;
  while (i<n && !(s[i] == '_' && (i+1 == n || !start_ident (s[i+1])))) i++;
  return compound ("em", coqdoc_to_tree (s (start, i++)));
}

/* Parse lists ***************************************************************/

static bool
is_list_begining (string s, int i) {
  int n= N(s);
  if (!(i<n && s[i] == '\n')) return false;
  i++;
  while (i<n && s[i] == ' ' && !test(s, i, "----")) i++;
  if (!(i<n && s[i] == '-' && !test(s, i, "----"))) return false;
  return true;
}

static int
get_list_depth (string s, int i) {
  int iitem= 0, n= N(s);
  if (i<n && s[i] == '\n') i++;
  while (i<n && s[i] == ' ') {
    iitem++;
    i++;
  }
  if (i<n && s[i] == '-' && !test(s, i, "----"))
    return iitem;
  else
    return -iitem;
}

static bool
can_parse_item (string s, int i, int iitem) {
  return get_list_depth (s, i) >= iitem;
}

static bool
can_parse_line (string s, int i, int itext) {
  if (get_list_depth (s, i) <= -itext || get_list_depth (s, i) >= itext)
    return true;
  int n= N(s);
  i++;
  while (i<n && (s[i] == ' ' || s[i] == '\t'))
    i++;
  return s[i] == '\n';
}

static tree
parse_line (string s, int &i, int itext) {
  int n= N(s);
  int start= i;
  if (i<n && s[i] == '\n') i++;
  while (i<n && s[i] == ' ') i++;
  if (i<n && s[i] != '-' && !test(s, i, "----")) {
    start= i;
    while (i<n && s[i] != '\n') i++;
  }
  else {
    while (i<n && s[i] != '\n') i++;
    while (can_parse_line (s, i, itext+1)) {
      i++;
      while (i<n && s[i] != '\n') i++;
    }
  }
  return coqdoc_to_tree (s(start, i));
}

static tree
parse_item (string s, int &i, int iitem) {
  int itext= iitem, n= N(s);
  if (i<n && s[i] == '\n') i++;
  i += iitem;
  if (i<n && s[i] == '-' && !test(s, i, "----")) {
    itext++;
    i++;
  }
  while (i<n && s[i] == ' ') {
    itext++;
    i++;
  }
  tree r (CONCAT);
  r << compound ("item");
  tree tmp= parse_line (s, i, itext);
  if (tmp != "")
    r << tmp;
  if (can_parse_line (s, i, itext)) {
    tree rr (DOCUMENT, r);
    while (can_parse_line (s, i, itext)) {
      tmp= parse_line (s, i, itext);
      if (tmp != "")
        rr << tmp;
    }
    return rr;
  }
  else
    return r;
}

static tree
parse_list (string s, int &i) {
  int iitem= get_list_depth (s, i);
  tree r (DOCUMENT);
  while (can_parse_item (s, i, iitem))
    r << parse_item (s, i, iitem);
  return compound ("itemize", r);
}

/* Parse pretty printing managment *******************************************/

// TODO.
// Nota: this interesting feature is used nowhere in Coq sources

static void
parse_pretty_printing_definition (string s) {
}

static void
parse_pretty_printing_removal (string s) {
}

static bool
is_defining_pretty_printing (string s, int i) {
  return false;
}

static bool
is_removing_pretty_printing (string s, int i) {
  return false;
}

/* Main parse routine ********************************************************/

static tree
coqdoc_to_tree (string s) {
  bool newline= true;
  int i=0, n= N(s);
  tree coqdoc (DOCUMENT), line (CONCAT);
  while (i < n) {
    if (test (s, i, "[[\n")) {
      add_line (line, coqdoc);
      tree vernac=
        vernac_to_tree (parse_delimited (s, i, "[[\n", "\n]]", false));
      coqdoc << compound ("coqdoc-vernac", vernac);
      newline= true;
    }
    else if (s[i] == '[')
      line << compound ("coqdoc-coq", parse_delimited (s, i, "[", "]", false));
    else if (newline && (test (s, i, "**** ") || test (s, i, "*** ") ||
                         test (s, i, "** ")   || test (s, i, "* "))) {
      line= tree (CONCAT);
      string header= "section";
      if (test (s, i, "** "))    header= "subsection";
      if (test (s, i, "*** "))   header= "subsubsection";
      if (test (s, i, "**** "))  header= "paragraph";
      while (i<n && (s[i] == '*')) i++;
      while (i<n && (s[i] == ' ')) i++;
      int start= i;
      while (i<n && (s[i] != '\n')) i++;
      coqdoc << compound (header, coqdoc_to_tree (s (start, i)));
      newline= true;
    }
    else if (newline && is_defining_pretty_printing (s, i)) {
      string str= parse_delimited (s, i, "(*", "*)", false);
      parse_pretty_printing_definition (str);
    }
    else if (newline && is_removing_pretty_printing (s, i)) {
      string str= parse_delimited (s, i, "(*", "*)", false);
      parse_pretty_printing_removal (str);
    }
    else if (test (s, i, "%%")) {
      line << "%";
      newline= false;
    }
    else if (test (s, i, "$$")) {
      line << "$";
      newline= false;
    }
    else if (test (s, i, "##")) {
      line << "#";
      newline= false;
    }
    else if (s[i] == '#' || s[i] == '%' || s[i] == '$') {
      newline= false;
      char delim= s[i];
      string ext= parse_delimited (s, i, delim);
      tree tm;
      if (delim == '#')
        tm= compound ("coqdoc-html", generic_to_tree (ext, "html-snippet"));
      else if (delim == '$')
        tm= compound ("coqdoc-latex",
              generic_to_tree ("$"*ext*"$", "latex-snippet"));
      else if (delim == '%')
        tm= compound ("coqdoc-latex", generic_to_tree (ext, "latex-snippet"));
      if (is_multi_paragraph (tm)) {
        add_line (line, coqdoc);
        coqdoc << tm;
      }
      else
        line << tm;
    }
    else if (is_list_begining (s, i)) {
      tree list= parse_list (s, i);
      add_line (line, coqdoc);
      coqdoc << list;
      newline= true;
    }
    else if (test (s, i, "<<\n")) {
      add_line (line, coqdoc);
      tree verb=
        verbatim_to_tree (parse_delimited (s, i, "<<\n", "\n>>", false));
      coqdoc << compound ("coqdoc-verbatim", verb);
      newline= true;
    }
    else if (s[i] == '_' && (i == 0 || !start_ident(s[i-1]))) {
      line << coqdoc_parse_emphasis (s, i);
      newline= false;
    }
    else if (test (s, i, "----")) {
      i+=  4;
      add_line (line, coqdoc);
      coqdoc << compound ("hrule");
      while (i<n && s[i] == '-') i++;
      newline= true;
    }
    else if (s[i] == '\n') {
      add_line (line, coqdoc);
      i++;
      newline= true;
    }
    else if (s[i] == '<') {
      line << "<less>";
      i++;
      newline= false;
    }
    else if (s[i] == '>') {
      line << "<gtr>";
      i++;
      newline= false;
    }
    else {
      if (s[i] != ' ')
        newline= false;
      line << s(i, 1+i);
      i++;
    }
  }
  if (N(line) > 0)
    add_line (line, coqdoc);
  if (N(coqdoc) == 0)
    return "";
  else if (N(coqdoc) == 1)
    return coqdoc[0];
  else
    return coqdoc;
}

static tree
parse_coqdoc (string s, int& i) {
  string coqdoc= parse_delimited (s, i, "(*", "*)");
  return compound ("coq-coqdoc", coqdoc_to_tree (coqdoc));
}

/******************************************************************************
* Parse raw coq
******************************************************************************/

/* Parse comments ************************************************************/

static tree
parse_comment (string s, int& i) {
  string coqdoc= parse_delimited (s, i, "(*", "*)");
  return compound ("coq-comment", from_verbatim (coqdoc));
}

/* Parse enunciations ********************************************************/

static bool
is_enunciation (string s) {
  string r= parse_command_name (s);
  return r == "Lemma" || r == "Remark" || r == "Fact" || r == "Corollary" ||
    r == "Proposition" || r == "Theorem";
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
  tree body= from_verbatim (s (++i, n), false);
  tree r= compound ("coq-enunciation", "", "dark grey");
  r << kind << name << body;
  return r;
}

/* Parse vernac commands *****************************************************/

static bool
end_vernac_command (string s, int i) {
  int n= N(s);
  if (!(i<n && s[i] == '.')) return false;
  i++;
  while (i<n && (s[i] == ' ' || s[i] == '\t')) i++;
  return i == n || s[i] == '\n';
}

static array<string>
split_command (string s) {
  int start= 0, i=0, n= N(s);
  array<string> r;
  while (i<n) {
    if (s[i] == '.' && i+1<n && (s[i+1] == ' ' || s[i+1] == '\t')) {
      r << s(start, ++i);
      while (i<n && (s[i] == ' ' || s[i] == '\t')) i++;
      start= i;
    }
    else
      i++;
  }
  if (start < n)
    r << s(start, n);
  return r;
}

static tree
parse_vernac_command (string s) {
  tree r (CONCAT);
  array<string> a= split_command (s);
  if (N(a) == 1)
    return compound ("coq-command", "", "dark grey",
                     from_verbatim (a[0], false));
  else if (N(a) > 0) {
    for (int i=0; i<N(a)-1; i++)
      r << compound ("coq-command", "", "dark grey",
                     from_verbatim (a[i], false))
        << " ";
    r << compound ("coq-command", "", "dark grey",
                   from_verbatim (a[N(a)-1], false));
  }
  return r;
}

/* Parse indentation *********************************************************/

static int
parse_indent (string s, int i) {
  int indent= 0, n= N(s);
  while (i<n && (s[i] == ' ' || s[i] == '\t')) i++, indent++;
  if (i<n && s[i] != '\n') return indent;
  else return -1;
}

/* Main parse routine ********************************************************/

static tree
parse_coqdoc_hide_show (string str, int &i) {
  tree r (UNINIT);
  string s= parse_delimited (str, i, "(*", "*)", false);
  int j= 0, n= N(s);
  while (j<n && s[j] == ' ') j++;
  if (test(s, j, "begin")) {
    r= tree (BEGIN);
    j+= 5;
  }
  else if (test(s, j, "end")) {
    r= tree (END);
    j+= 3;
  }
  else return r;
  while (j<n && s[j] == ' ') j++;
  if (test(s, j, "hide") || test(s, j, "show")) {
    r << s (j, j+4);
  }
  return r;
}

static bool
is_hide_or_show (string s, int i) {
  return parse_coqdoc_hide_show (s, i) != tree (UNINIT);
}

/* Main parse routine ********************************************************/

static tree
parse_raw_coq (string s) {
  tree doc (DOCUMENT), proof (DOCUMENT), enun;
  tree *r= &doc;
  int i= 0, startcmd= 0, n= N(s), indent_level=-1;
  bool in_cmd= false;
  while (i<n) {
    if (start_coqdoc (s, i))
      doc << parse_coqdoc (s, i);
    else if (start_comment (s, i)) {
      if (is_hide_or_show (s, i))
        doc << parse_coqdoc_hide_show (s, i);
      else
        doc << parse_comment (s, i);
    }
    else if (end_vernac_command (s, i)) {
      string body= s (startcmd, ++i);
      if (is_enunciation (body)) {
        enun= parse_enunciation (body);
        r= &proof;
      }
      else {
        tree tmp= parse_vernac_command (body);
        if (r == &proof) {
          if (is_end_proof (tmp)) {
            proof << tmp;
            enun << proof;
            doc << enun;
            proof= tree (DOCUMENT);
            r= &doc;
          }
          else if (!is_proof (tmp))
            *r << tmp;
        }
        else
          *r << tmp;
      }
      in_cmd= false;
    }
    else if (!in_cmd && s[i] == '\n') {
      i++;
      int tmp= parse_indent (s, i);
      if (tmp >= 0 && tmp != indent_level) {
        *r << compound ("coq-indent", as_string (tmp));
        indent_level= tmp;
      }
      else if (i < n && s[i] == '\n') {
        *r << "";
        while (i+1 < n && s[i+1] == '\n') i++;
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
  return doc;
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
                    || is_compound (t, "coq-coqdoc")
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
        if (curr_ind > -1 && (curr_ind < indent || curr_ind < base_indent)) {
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
* Show / hide parsed coq
******************************************************************************/

static tree
show_hide_parsed_coq (tree t) {
  if (is_atomic (t)) return t;
  tree r (L(t)), tmp (DOCUMENT);
  string s, msg;
  int i, n= N(t), cnt= 0;
  if (is_document (t)) {
    for (i=0; i<n; i++) {
      if (is_func (t[i], BEGIN, 1)) {
        cnt= 1;
        if (as_string (t[i++][0]) == "hide") {
          s= "folded";
          msg= "(* hidden *)";
        }
        else {
          s= "unfolded";
          msg= "(* shown *)";
        }
        while (i<n && cnt > 0) {
          if (is_func (t[i], BEGIN, 1)) cnt++;
          if (is_func (t[i], END, 1))   cnt--;
          if (cnt > 0) tmp << t[i];
          i++;
        }
        r << compound (s, msg, show_hide_parsed_coq (tmp));
        if (i<n) i--;
      }
      else
        r << t[i];
    }
  }
  else
    for (i=0; i<n; i++)
      r << show_hide_parsed_coq (t[i]);
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
vernac_to_tree (string s) {
  tree r (DOCUMENT);
  r= parse_raw_coq (s);
  r= show_hide_parsed_coq (r);
  r= section_parsed_coq (r);
  r= indent_parsed_coq (r);
  if (N(r) == 0) r << "";
  return r;
}

tree
vernac_document_to_tree (string s) {
  tree style= compound ("style", tuple ("generic", "coq"));
  tree body= compound ("body", vernac_to_tree (s));
  return document (body, style);
}
