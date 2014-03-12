
/******************************************************************************
* MODULE     : latex_tools.cpp
* DESCRIPTION: Tools for direct manipulation and analysis of LaTeX files
* COPYRIGHT  : (C) 2014  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

/******************************************************************************
* TeXmacs preamble management
******************************************************************************/

static string tmpre_start= "%%%%%%%%%% Start TeXmacs macros\n";
static string tmpre_end  = "%%%%%%%%%% End TeXmacs macros\n";

static bool
latex_search_texmacs_preamble (string s, int& b, int& e) {
  b= search_forwards (tmpre_start, s);
  if (b < 0) return false;
  e= search_forwards (tmpre_end, b, s);
  if (e < 0) return false;
  e += N(tmpre_end);
  return true;
}

string
latex_get_texmacs_preamble (string s) {
  int b, e;
  if (!latex_search_texmacs_preamble (s, b, e)) return "";
  return s (b + N(tmpre_start), e - N(tmpre_start));
}

string
latex_remove_texmacs_preamble (string s) { 
  int b, e;
  if (!latex_search_texmacs_preamble (s, b, e)) return s;
  if (b > 1 && s (b-2, b) == "\n\n")
    while (e < N(s) && s[e] == '\n') e++;
  return s (0, b) * s (e, N(s));
}

string
latex_set_texmacs_preamble (string s, string p) {
  int b= search_forwards ("\\begin{document}", s);
  if (b == -1) return s;
  s= latex_remove_texmacs_preamble (s);
  b= search_forwards ("\\begin{document}", s);
  if (b == -1) return s;
  if (!ends (p, "\n")) p= p * "\n";
  string ins= tmpre_start * p * (tmpre_end * "\n");
  int bb= b;
  while (bb>0 && s[bb-1] == '\n') bb--;
  if (bb != 0) {
    if (b == bb) ins= "\n\n" * ins;
    else if (b == bb + 1) ins= "\n" * ins;
  }
  return s (0, b) * ins * s (b, N(s));
}

/******************************************************************************
* Collecting the LaTeX packages being used
******************************************************************************/

hashmap<string,path>
latex_get_packages (string s) {
  int pos= search_forwards (s, "\\begin{document}");
  if (pos != -1) s= s (0, pos);
  hashmap<string,path> h;
  string usepackage= "\\usepackage";  
  int i, n= N(s);
  for (i=0; i<n; )
    if (test (s, i, usepackage)) {
      i += N(usepackage);
      skip_square (s, i);
      skip_spaces (s, i);
      if (i<n && s[i] == '{') {
        int start= i+1;
        if (skip_curly (s, i)) {
          int j= start;
          skip_spaces (s, j);
          while (true) {
            int b= j;
            while (s[j] != ' ' && s[j] != ',' && s[j] != '}') j++;
            int e= j;
            h (s (b, e))= path (b, e);
            //cout << s (b, e) << " ~~> " << s (start, i-1) << LF;
            skip_spaces (s, j);
            if (s[j] == '}') break;
            if (s[j] == ',') j++;
          }
        }
      }
    }
    else if (s[i] == '%') skip_line (s, i);
    else i++;
  return h;
}

/******************************************************************************
* Collecting LaTeX declarations
******************************************************************************/

static bool
parse_declaration_sub (string s, int& i, string cmd,
                       int arity, bool trail_opt,
                       hashmap<string,path>& h) {
  // NOTE: routine parses all options between arguments,
  // as well as any trailing option if trail_opt holds.
  int b= i, n= N(s);
  i += N(cmd);
  string first;
  for (int arg=0; arg<arity; arg++) {
    skip_whitespace (s, i);
    while (i<n && s[i] == '[') {
      skip_square (s, i);
      skip_whitespace (s, i);
    }
    if (i<n && s[i] == '{') {
      int start= i+1;
      if (!skip_curly (s, i)) return false;
      int end= i-1;
      if (arg == 0) first= s (start, end);
    }
    else {
      int start= i;
      if (i<n && s[i] == '\\') i++;
      if (is_alpha (s[i-1])) return false;
      if (i<n && is_alpha (s[i]))
        while (i<n && is_alpha (s[i])) i++;
      else if (i<n) i++;
      if (i == n) return false;
      int end= i;
      if (arg == 0) first= s (start, end);
    }
  }
  int e= i;
  skip_whitespace (s, i);
  if (i<n && s[i] == '[') {
    skip_square (s, i);
    e= i;
  }
  if (starts (first, "\\"))
    first= first (1, N(first));
  h (first)= path (b, e);
  //cout << first << " ~~> " << s (b, e) << LF;
  return true;
}

static bool
parse_declaration (string s, int& i, string cmd,
                   int arity, bool trail_opt,
                   hashmap<string,path>& h) {
  int b= i;
  if (!parse_declaration_sub (s, i, cmd, arity, trail_opt, h))
    i= b + N(cmd);
}

hashmap<string,path>
latex_get_declarations (string s) {
  int pos= search_forwards (s, "\\begin{document}");
  if (pos != -1) s= s (0, pos);
  hashmap<string,path> h;
  int i, n= N(s);
  for (i=0; i<n; )
    if (test (s, i, "\\def"))
      parse_declaration (s, i, "\\def", 2, false, h);
    else if (test (s, i, "\\newcommand"))
      parse_declaration (s, i, "\\newcommand", 2, false, h);
    else if (test (s, i, "\\renewcommand"))
      parse_declaration (s, i, "\\renewcommand", 2, false, h);
    else if (test (s, i, "\\newenvironment"))
      parse_declaration (s, i, "\\newenvironment", 3, false, h);
    else if (test (s, i, "\\renewenvironment"))
      parse_declaration (s, i, "\\renewenvironment", 3, false, h);
    else if (test (s, i, "\\newtheorem"))
      parse_declaration (s, i, "\\newtheorem", 2, false, h);
    else if (s[i] == '%') skip_line (s, i);
    else i++;
  return h;
}

hashmap<int,int>
latex_declaration_positions (string s) {
  hashmap<int,int> r (-1);
  hashmap<string,path> h= latex_get_declarations (s);
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    path p= h [it->next ()];
    r (p[0])= 0;
    r (p[1])= 1;
  }
  return r;
}
