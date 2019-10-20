
/******************************************************************************
* MODULE     : xml.cpp
* DESCRIPTION: routines on xml used in scheme
* COPYRIGHT  : (C) 2019  Joris van der Hoeven, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

/******************************************************************************
* Convert between TeXmacs and XML strings
******************************************************************************/

/*
 * According to: https://www.w3.org/TR/2008/REC-xml-20081126/#sec-common-syn
 * NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
 *   | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D]
 *   | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF]
 *   | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
 * NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
 *   | [#x0300-#x036F] | [#x203F-#x2040]
 * Name ::= NameStartChar (NameChar)*
 *
 * Currently, we only handle the visible ascii characters
 */
static bool
is_xml_name (char c, int i) {
  if ((c == ':') || (c == '_') || is_alpha (c)) {
    return true;
  } else if (i == 0) {
    return false;
  } else {
    return (c == '-') || (c == '.') || is_digit (c);
  }
}

string
tm_to_xml_name (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (is_xml_name (s[i], i)) r << s[i];
    else r << "_" << as_string ((int) ((unsigned char) s[i])) << "_";
  return r;
}

string
xml_name_to_tm (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] != '_') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!='_')) i++;
      r << (char) ((unsigned char) as_int (s (start, i)));
    }
  return r;
}

string
old_tm_to_xml_cdata (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '>') r << "&gt;";
    else if (s[i] != '<') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!='>')) i++;
      r << "&" << tm_to_xml_name (s (start, i)) << ";";
    }
  return r;
}

object
tm_to_xml_cdata (string s) {
  array<object> a;
  a << symbol_object ("!concat");
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '>') r << "&gt;";
    else if (s[i] == '\\') r << "\\";
    else if (s[i] != '<') r << cork_to_utf8 (s (i, i+1));
    else {
      int start= i++;
      while ((i<n) && (s[i]!='>')) i++;
      string ss= s (start, i+1);
      string rr= cork_to_utf8 (ss);
      string qq= utf8_to_cork (rr);
      if (rr != ss && qq == ss && ss != "<less>" && ss != "<gtr>") r << rr;
      else {
        if (r != "") a << object (r);
        a << cons (symbol_object ("tm-sym"),
                   cons (ss (1, N(ss)-1),
                         null_object ()));
        r= "";
      }
    }
  if (r != "") a << object (r);
  if (N(a) == 1) return object ("");
  else if (N(a) == 2) return a[1];
  else return call ("list", a);
}

string
old_xml_cdata_to_tm (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '<') r << "<less>";
    else if (s[i] == '>') r << "<gtr>";
    else if (s[i] != '&') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!=';')) i++;
      string x= "<" * xml_name_to_tm (s (start, i)) * ">";
      if (x == "<amp>") r << "&";
      else r << x;
    }
  return r;
}

string
xml_unspace (string s, bool first, bool last) {
  string r;
  int i= 0, n= N(s);
  if (first) while ((i<n) && is_space (s[i])) i++;
  while (i<n)
    if (!is_space (s[i])) r << s[i++];
    else {
      while ((i<n) && is_space (s[i])) i++;
      if ((i<n) || (!last)) r << ' ';
    }
  return r;
}
