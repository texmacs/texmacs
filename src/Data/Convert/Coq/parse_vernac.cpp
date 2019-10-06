
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
#include "analyze.hpp"

static int  parse_indent   (string s, int i);
static tree parse_raw_coq  (string s);
static tree coqdoc_to_tree (string s);


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

  tree t= verbatim_to_tree (s, wrap, "SourceCode");
  if (is_document (t) && N(t) == 1)
    return t[0];
  return t;
}

/* Parsing blanks ************************************************************/

static bool
is_spacing (char c) {
  return c == ' ';
}

static bool
is_blank (char c) {
  return c == ' ' || c == '\n';
}

static bool
is_blank (string s) {
  int i= 0, n= N(s);
  while (i<n)
    if (!is_blank (s[i++]))
      return false;
  return true;
}

static bool
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

static bool
is_star_rule (string s, int i) {
  int n= N(s), stars=0, blanks= 0;
  i++;
  while (i<n) {
    if (s[i] == '*') stars++;
    else if (is_spacing (s[i])) blanks++;
    else break;
    i++;
  }
  return (i >= n || (i>0 && test (s, i-1, "*)"))) && stars > blanks;
}

static bool
start_coqdoc (string s, int i) {
  if (!test (s, i, "(**"))
    return false;
  return !is_star_rule (s, i);
}

static bool
start_comment (string s, int i) {
  return test (s, i, "(*");
}

/* Parsing idents ************************************************************/

static bool
start_utf8_seq (char c) {
  return ((0xE0 & c) == 0xC0) || ((0xF0 & c) == 0xE0) || ((0xF8 & c) == 0xF0);
}

static bool
start_ident (char c) {
  // Note: unlike Coq, we don't discriminate utf8 letters from symbols. See
  // http://coq.inria.fr/distrib/current/refman/Reference-Manual003.html#sec17
  // for details.
  return is_alpha (c) || c == '_' || start_utf8_seq (c);
}

static bool
continue_ident (char c) {
  return start_ident (c) || is_digit (c) || c == '\'';
}

static string
parse_identifier (string s, int &i) {
  int n= N(s), start= i;
  if (i<n && start_ident (s[i])) {
    decode_from_utf8 (s, i);
    while (i<n && continue_ident (s[i]))
      decode_from_utf8 (s, i);
  }
  return as_string (from_verbatim (s (start, i)));
}

/* Parsing command names *****************************************************/

static string
parse_command_name (string s, int &i) {
  int n= N(s), start= i;
  if (i<n && is_upcase(s[i])) {
    i++;
    while (i<n && is_locase (s[i])) i++;
  }
  return s (start, i);
}

static string
parse_command_name (string s) {
  int i= 0;
  return parse_command_name (s, i);
}

/******************************************************************************
* Parse coqdoc
******************************************************************************/

static string
parse_delimited (string s, int& i, char c) {
  int start= i + 1, n= N(s);
  string esc (c);
  esc= esc*esc;
  i++;
  while (i < n) {
    if (s[i] == c && !test (s, i, esc))
      break;
    else if (s[i] == c && test (s, i, esc))
      i++;
    i++;
  }
  if (i<n && s[i] == c)
    return s (start, i++);
  return s (start, n);
}

static string
parse_delimited (string s, int& i, string beg, string end, bool keep= true) {
  int start= i + (!keep)*N(beg), cnt= 1, n= N(s);
  i++;
  while (i < n && cnt > 0) {
    if (test (s, i, beg)) cnt++;
    if (test (s, i, end)) cnt--;
    i++;
  }
  if (cnt != 0) {
    convert_error << "Vernac import: unbalanced delimited string "
      << "started by \"" << beg << "\"" << LF;
    return s (start, n);
  }
  else if (test (s, i-1, end))
    i+=N(end)-1;
  return s (start, i - (!keep)*N(end));
}

static void
add_line(tree &c, tree &d) {
  tree tmp= trim_spaces (simplify_concat (c));
  if (!is_blank (tmp))
    d << copy (tmp);
  c= tree (CONCAT);
}

static string
unescape_coqdoc (string s) {
  string r= replace (s, "##", "#");
  r= replace (r, "%%", "%");
  r= replace (r, "$$", "$");
  return r;
}

static bool
is_whiteline (string s, int i) {
  int n= N(s);
  while (i<n && is_spacing (s[i])) i++;
  return s[i] == '\n';
}

static void
skip_whiteline (string s, int &i) {
  int n= N(s);
  while (i<n && is_spacing (s[i])) i++;
  i++;
}

/* Parse emphasis ************************************************************/

static tree
coqdoc_parse_emphasis (string s, int &i) {
  int n= N(s), start= ++i;
  while (i<n && !(s[i] == '_' && (i+1 >= n || !start_ident (s[i+1])))) i++;
  return compound ("em", coqdoc_to_tree (s (start, i++)));
}

/* Parse lists ***************************************************************/

/* FIXME:
 * Coqdoc lists are quite ugly. It is difficult to know when does an item ends.
 */

static bool
is_new_item (string s, int i) {
  return i<N(s) && s[i] == '-' && !test(s, i, "----");
}

static bool
is_list_begining (string s, int i) {
  int n= N(s);
  if (!(i<n && s[i] == '\n')) return false;
  i++;
  while (i<n && is_spacing (s[i])) i++;
  return is_new_item (s, i);
}

static int
get_list_depth (string s, int i) {
  int item_indent= 0, n= N(s), j;
  while (i<n && is_blank (s[i])) i++;
  if (i >= n) return 0;
  j= i-1;
  while (j>=0 && s[j] != '\n') {
    if (s[j] == ' ')
      item_indent++;
    j--;
  }
  if (is_new_item (s, i))
    return item_indent + 1;
  else if (item_indent == 0)
    return 0;
  else
    return -item_indent - 1;
}

static bool
can_parse_item (string s, int i, int item_indent) {
  return get_list_depth (s, i) == item_indent;
}

static bool
can_parse_line (string s, int i, int text_indent) {
  int d= get_list_depth (s, i);
  if (d != 0 && (d <= -text_indent || d >=  text_indent))
    return true;
  else
    return test (s, i, "\n<<") || test (s, i, "\n\n<<") ||
           test (s, i, "[[\n") || test (s, i, "\n[[\n");
}

static string
parse_line (string s, int &i, int text_indent) {
  int n= N(s);
  int start= i;
  string r= "";
  while (i<n && is_blank (s[i])) i++;
  if (!is_new_item (s, i)) {
    start= i;
    if (test (s, i, "<<")) {
      while (i<n && !test (s, i, "\n>>")) i++;
      if (i >= n) {
        i= start;
        while (i<n && s[i] != '\n') i++;
        while (can_parse_line (s, i, text_indent+1))
          parse_line (s, i, text_indent+1);
      }
      else {
        i+= 3;
        r= "\n" * s (start, i);
      }
    }
    else if (test (s, i, "[[\n")) {
      while (i<n && !test (s, i, "\n]]")) i++;
      i+= 3;
    }
    else
      while (i<n && s[i] != '\n') i++;
  }
  else {
    while (i<n && s[i] != '\n') i++;
    while (can_parse_line (s, i, text_indent+1))
      parse_line (s, i, text_indent+1);
  }
  if (r == "")
    r= s(start, i);
  return r;
}

static tree
parse_item (string s, int &i, int item_indent) {
  int text_indent= item_indent, n= N(s);
  while (i<n && is_blank (s[i])) i++;
  if (is_new_item (s, i)) {
    text_indent++;
    while (s[i] == '-') i++;
  }
  while (i<n && is_spacing (s[i])) {
    if (s[i] == ' ')
      text_indent++;
    i++;
  }
  tree r (CONCAT);
  r << compound ("item");
  string line= "";
  string tmp= parse_line (s, i, text_indent);
  if (tmp != "")
    line << tmp;
  while (can_parse_line (s, i, text_indent)) {
    tmp= parse_line (s, i, text_indent);
    if (tmp != "")
      line << "\n" <<  tmp;
  }
  r << coqdoc_to_tree (line);
  return r;
}

static tree
parse_list (string s, int &i) {
  int item_indent= get_list_depth (s, i);
  tree r (DOCUMENT);
  while (can_parse_item (s, i, item_indent))
    r << parse_item (s, i, item_indent);
  return compound ("itemize", r);
}

/* Parse pretty printing managment *******************************************/

// TODO.
// Nota: this interesting feature is used nowhere in Coq sources

static void
parse_pretty_printing_definition (string s) {
  (void) s;
}

static void
parse_pretty_printing_removal (string s) {
  (void) s;
}

static bool
is_defining_pretty_printing (string s, int i) {
  (void) s; (void) i;
  return false;
}

static bool
is_removing_pretty_printing (string s, int i) {
  (void) s; (void) i;
  return false;
}

/* Main parse routine ********************************************************/

static tree
coqdoc_to_tree (string s) {
  bool newline= true;
  int i=0, n= N(s);
  tree coqdoc (DOCUMENT), line (CONCAT);
  if (starts (s, "(**")) {
    line << "(**";
    i+= 3;
  }
  while (i < n) {
    if (test (s, i, "[[\n")) {
      add_line (line, coqdoc);
      tree vernac=
        vernac_to_tree (parse_delimited (s, i, "[[\n", "\n]]", false));
      coqdoc << compound ("coqdoc-vernac", vernac);
      newline= true;
    }
    else if (s[i] == '[')
      line << compound ("coqdoc-coq",
          from_verbatim (parse_delimited (s, i, "[", "]", false)));
    else if (newline && (test (s, i, "**** ") || test (s, i, "*** ") ||
                         test (s, i, "** ")   || test (s, i, "* "))) {
      string header= "section";
      if (test (s, i, "** "))    header= "subsection";
      if (test (s, i, "*** "))   header= "subsubsection";
      if (test (s, i, "**** "))  header= "paragraph";
      while (i<n && s[i] == '*') i++;
      while (i<n && is_spacing (s[i])) i++;
      int start= i;
      while (i<n && (s[i] != '\n' && !test (s, i, "*)"))) i++;
      line << compound (header, coqdoc_to_tree (s (start, i)));
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
      i+= 2;
    }
    else if (test (s, i, "$$")) {
      line << "$";
      newline= false;
      i+= 2;
    }
    else if (test (s, i, "##")) {
      line << "#";
      newline= false;
      i+= 2;
    }
    else if (s[i] == '#' || s[i] == '%' || s[i] == '$') {
      newline= false;
      char delim= s[i];
      string ext= unescape_coqdoc (parse_delimited (s, i, delim));
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
    else if (test (s, i, "\n<<")) {
      add_line (line, coqdoc);
      string parsed= parse_delimited (s, i, "\n<<", "\n>>", false);
      if (N(parsed) > 0 && parsed[0] == '\n')
        parsed= parsed(1, N(parsed));
      tree verb= verbatim_to_tree (parsed, false, "SourceCode");
      if (is_atomic (verb))
        verb= document (verb);
      coqdoc << compound ("coqdoc-verbatim", verb);
      newline= true;
    }
    else if (test (s, i, "<<")) {
      string parsed= parse_delimited (s, i, "<<", ">>", false);
      tree verb= verbatim_to_tree (parsed, true, "SourceCode");
      line << compound ("coqdoc-verbatim", verb);
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
      if (is_whiteline (s, i)) {
        coqdoc << "";
        do
          skip_whiteline (s, i);
        while (is_whiteline (s, i));
        i--;
      }
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
      if (!is_spacing (s[i]))
        newline= false;
      int start= i;
      decode_from_utf8 (s, i);
      line << from_verbatim (s(start, i));
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
  string coqdoc= parse_delimited (s, i, "(*", "*)", false);
  coqdoc= trim_spaces (coqdoc (1, N(coqdoc)));
  return compound ("coq-coqdoc", coqdoc_to_tree (coqdoc));
}

/******************************************************************************
* Parse raw coq
******************************************************************************/

/* Parse comments ************************************************************/

static tree
parse_comment (string s, int& i) {
  string comment= parse_delimited (s, i, "(*", "*)", false);
  if (is_star_rule (comment, 0))
    return compound ("hrule");
  return compound ("coq-comment", from_verbatim (comment, false));
}

/* Parse enunciations ********************************************************/

static bool
is_enunciation (string s) {
  string r= parse_command_name (s);
  return r == "Corollary"   || r == "Fact"   || r == "Lemma"   ||
         r == "Proposition" || r == "Remark" || r == "Theorem";
}

static bool
is_definition (string s) {
  string r= parse_command_name (s);
  return r == "Axiom"        || r == "CoFixpoint" || r == "CoInductive" ||
         r == "Conjecture"   || r == "Definition" || r == "Example"     ||
         r == "Fixpoint"     || r == "Hypothesis" || r == "Inductive"   ||
         r == "Let"          || r == "Parameter"  || r == "Variable";
}

static bool
is_begin_proof (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= parse_command_name (as_string (t[2]));
  return s == "Proof";
}

static bool
begin_proof (tree t) {
  if (is_concat (t) && N(t) > 0)
    return begin_proof (t[0]);
  return is_begin_proof (t);
}

static bool
is_end_proof (tree t) {
  if (!is_compound (t, "coq-command", 3)) return false;
  string s= parse_command_name (as_string (t[2]));
  return s == "Qed" || s == "Admitted" || s == "Defined" || s == "Abort";
}

static bool
end_proof (tree t) {
  if (is_concat (t) && N(t) > 0)
    return end_proof (t[N(t)-1]);
  return is_end_proof (t);
}

static void
append_commands (tree &t, tree u) {
  if (t == "")
    t= u;
  else if (is_atomic (t))
    t= concat (trim_spaces_right (t), u);
  else if (is_document (u)) {
    if (N(u) > 0) {
      append_commands (t, u[0]);
      u[0]= t;
      t= u;
    }
  }
  else if (is_document (t)) {
    int last= N(t)-1;
    append_commands (t[last], u);
  }
  else if (is_concat (t) && is_concat (u))
    t << A(u);
  else if (is_concat (t))
    t << u;
}

static tree
indent_subcommand (tree t) {
  if (is_document (t)) {
    tree r (DOCUMENT);
    for (int i=0; i<N(t); i++) {
      tree tmp= indent_subcommand (t[i]);
      if (is_atomic (tmp))
        r << tmp;
      else
        r << A(tmp);
    }
    return r;
  }
  else if (is_atomic (t)) {
    string s= as_string (t);
    int i= parse_indent (s, 0);
    if (i <= 0)
      return t;
    return tree (DOCUMENT, compound ("coq-indent", as_string (i)), s(i, N(s)));
  }
  else
    return t;
}

static tree
parse_subcommand (string s, bool wrap= false) {
  int start= 0, i= 0, n= N(s);
  tree r= "";
  while (i<n) {
    if (start_comment (s, i)) {
      append_commands (r,
          indent_subcommand (from_verbatim (s(start, i), wrap)));
      if (start_coqdoc (s, i))
        append_commands (r, parse_coqdoc (s, i));
      else
        append_commands (r, parse_comment (s, i));
      start= i;
    }
    else
      i++;
  }
  if (start < n)
    append_commands (r, indent_subcommand (from_verbatim (s(start, i), wrap)));
  if ((is_concat (r) || is_document (r)) && N(r) == 1)
    r= r[0];
  return r;
}

static tree
parse_enunciation (string s) {
  int i= 0, n= N(s);
  string kind= parse_command_name (s, i);
  while (i<n && is_blank (s[i])) i++;
  string name= parse_identifier (s, i);
  while (i<n && is_blank (s[i])) i++;
  tree body= parse_subcommand (s (i, n));
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
  while (i<n) {
   if (is_spacing (s[i]))
     i++;
   else if (start_comment (s, i))
     parse_comment (s, i);
   else
     break;
  }
  return i >= n || s[i] == '\n';
}

static array<string>
split_command (string s) {
  int start= 0, i=0, n= N(s);
  array<string> r;
  while (i<n) {
    if (s[i] == '.' && i+1<n && is_spacing (s[i+1])) {
      r << s(start, ++i);
      while (i<n && is_spacing (s[i])) i++;
      start= i;
    }
    else if (start_comment (s, i))
      parse_comment (s, i);
    else
      i++;
  }
  if (start < n)
    r << s(start, n);
  return r;
}

static tree
ensure_inline (tree t) {
  if (is_atomic (t)) return t;
  int n= N(t);
  tree r (L(t));
  bool sep= false;
  if (L(t) == DOCUMENT) {
    r= tree (CONCAT);
    sep= true;
  }
  for (int i= 0; i<n; i++) {
    if (!is_compound (t[i], "coq-indent", 1)) {
      r << ensure_inline (t[i]);
      if (sep && i < n-1)
        r << " ";
    }
  }
  return r;
}

static tree
parse_vernac_command (string s, bool wrap= false) {
  tree r (CONCAT);
  array<string> a= split_command (s);
  if (N(a) == 1)
    return compound ("coq-command", "", "dark grey",
                     parse_subcommand (a[0], wrap));
  else if (N(a) > 0) {
    for (int i=0; i<N(a)-1; i++)
      r << compound ("coq-command", "", "dark grey",
                     ensure_inline (parse_subcommand (a[i], wrap)))
        << " ";
    r << compound ("coq-command", "", "dark grey",
                   ensure_inline (parse_subcommand (a[N(a)-1], wrap)));
  }
  return r;
}

static array<tree>
parse_vernac_proof (string s) {
  array<tree> r;
  array<string> a= split_command (s);
  if (N(a) == 0) return r;
  r << compound ("coq-command", "", "dark grey", parse_subcommand (a[0]));
  if (N(a) > 1) {
    string pf= recompose (range (a, 1, N(a)), " ");
    r << parse_vernac_command (pf);
  }
  return r;
}

static tree
format_proof (tree t) {
  tree r= compound ("coq-proof", "", "dark grey");
  if (!is_document (t) || N(t) == 0)
    r << "" << "";
  else if (N(t) > 0 && is_begin_proof (t[0])) {
    r << t[0];
    if (N(t) > 1)
      r << t(1,N(t));
    else
      r << "";
  }
  else if (N(t) > 1 && is_compound (t[0], "coq-indent", 1)
                    && is_begin_proof (t[1])) {
    r << t[1];
    if (N(t) > 2) {
      tree tmp= tree (DOCUMENT, t[0]);
      tmp << A(t(2,N(t)));
      r << tmp;
    }
    else
      r << "";
  }
  else
    r << "Proof." << t;
  return r;
}
/* Parse indentation *********************************************************/

static int
parse_indent (string s, int i) {
  int indent= 0, n= N(s);
  while (i<n && is_spacing (s[i])) i++, indent++;
  if (i<n && s[i] != '\n') return indent;
  else return -1;
}

/* Parse hide/show blocks ****************************************************/

static tree
parse_coqdoc_hide_show_comment (string str, int &i) {
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
    return r;
  }
  return tree (UNINIT);
}

static bool
is_hide_or_show (string s, int i) {
  return parse_coqdoc_hide_show_comment (s, i) != tree (UNINIT);
}

static tree
parse_coqdoc_hide_show (string s, int &i) {
  tree beg= parse_coqdoc_hide_show_comment (s, i);
  int cnt= 1, n= N(s), start= i, stop= i;
  while (i<n && cnt > 0) {
    if (start_comment (s, i)) {
      if (is_hide_or_show (s, i)) {
        stop= i;
        tree tmp= parse_coqdoc_hide_show_comment (s, i);
        if (N(tmp) != 1);
        else if (tmp == beg)
          cnt++;
        else if (is_func (tmp, END, 1) && tmp[0] == beg[0])
          cnt--;
      }
      else
        parse_comment (s, i);
    }
    else
      i++;
  }
  tree body;
  if (cnt == 0)
    body= parse_raw_coq (s(start, stop));
  else
    body= parse_raw_coq (s(start, n));
  string lbl, msg;
  if (as_string (beg[0]) == "hide") {
    lbl= "folded";
    msg= "(* hidden *)";
  }
  else {
    lbl= "unfolded";
    msg= "(* shown *)";
  }
  return compound (lbl, msg, body);
}

/* Main parse routine ********************************************************/

static tree
parse_raw_coq (string s) {
  tree doc (DOCUMENT), proof (DOCUMENT);
  tree *r= &doc;
  int i= 0, startcmd= 0, n= N(s), indent_level=-1;
  bool in_cmd= false;
  while (i<n) {
    if (!in_cmd && start_comment (s, i)) {
      if (start_coqdoc (s, i))
        *r << parse_coqdoc (s, i);
      else if (is_hide_or_show (s, i))
        *r << parse_coqdoc_hide_show (s, i);
      else
        *r << parse_comment (s, i);
    }
    else if (start_comment (s, i)) {
      if (is_hide_or_show (s, i))
        *r << parse_coqdoc_hide_show (s, i);
      else
        parse_comment (s, i);
    }
    else if (end_vernac_command (s, i)) {
      string body= s (startcmd, ++i);
      if (is_enunciation (body)) {
        *r << parse_enunciation (body);
        r= &proof;
      }
      else if (is_definition (body)) {
        *r << parse_enunciation (body);
      }
      else {
        tree tmp= parse_vernac_command (body);
        if (begin_proof (tmp) && end_proof (tmp)) {
          proof << parse_vernac_proof (body);
          doc << format_proof (proof);
          proof= tree (DOCUMENT);
          r= &doc;
        }
        else if (r == &proof) {
          if (end_proof (tmp)) {
            proof << tmp;
            doc << format_proof (proof);
            proof= tree (DOCUMENT);
            r= &doc;
          }
          else if (begin_proof (tmp)) {
            r= &proof;
            *r << parse_vernac_proof (body);
          }
          else
            *r << tmp;
        }
        else if (begin_proof (tmp)) {
          r= &proof;
          *r << parse_vernac_proof (body);
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
  if (N(proof) > 0)
    doc << format_proof (proof);
  if (in_cmd)
    doc << parse_vernac_command (s (startcmd, n));
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
remove_all_indent (tree t) {
  if (is_atomic (t)                      ||
      is_compound (t, "coqdoc-verbatim") ||
      is_compound (t, "coq-comment"))
    return t;
  if (is_compound (t, "coq-indent"))
    return "";
  int i, n= N(t);
  tree r (L(t));
  for (i=0; i<n; i++)
    if (!is_compound (t[i], "coq-indent", 1))
      r << t[i];
  if (N(r) == 0 && (is_document (r) || is_concat (r)))
    r= "";
  else if (N(r) == 1 && (is_document (r) || is_concat (r)))
    r= r[0];
  return r;
}

static tree
indent_parsed_coq (tree t, int base_indent=0) {
  if (is_atomic (t) || is_compound (t, "coq-indent")
                    || is_compound (t, "coqdoc-verbatim")
                    || is_compound (t, "coq-comment")) return t;
  tree r (L(t));
  int i, n= N(t);
  for (i=0; i<n; i++) {
    if (get_indent (t[i]) > base_indent) {
      tree body (DOCUMENT);
      int indent= get_indent (t[i]);
      bool stop= false;
      while (i<n && !stop) {
        int curr_ind= get_indent (t[i]);
        if (curr_ind > -1 && curr_ind > base_indent && curr_ind < indent)
          indent= curr_ind;
        if (curr_ind > -1 && curr_ind <= base_indent) {
          i--;
          stop= true;
        }
        else body << t[i++];
      }
      tree tmp= indent_parsed_coq (body, indent);
      if (is_compound (tmp, "indent", 1))
        r << tmp;
      else if (is_atomic (tmp))
        r << compound ("indent", document (tmp));
      else
        r << compound ("indent", tmp);
    }
    else if (get_indent (t[i]) == base_indent);
    else
      r << indent_parsed_coq (t[i], base_indent);
  }
  return remove_all_indent (r);
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
      r << compound ("coq-section", name, section_parsed_coq (body));
    }
    else
      r << t[i];
  }
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
vernac_to_tree (string s) {
  tree r (DOCUMENT);
  s= convert_tabs_to_spaces (s, 8);
  r= parse_raw_coq (s);
  r= section_parsed_coq (r);
  r= indent_parsed_coq (r);
  return r;
}

tree
vernac_document_to_tree (string s) {
  tree style= compound ("style", tuple ("generic", "coq"));
  tree body= compound ("body", vernac_to_tree (s));
  return document (body, style);
}
