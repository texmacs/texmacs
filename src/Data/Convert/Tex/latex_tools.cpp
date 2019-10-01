
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
#include "file.hpp"

/******************************************************************************
* Language translation
******************************************************************************/

string
latex_to_texmacs_languages (string s) {
  if (s == "frenchb")  return "french";
  if (s == "ngermanb") return "german";
  if (s == "magyar")   return "hungarian";
  return s;
}

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
  return s (b + N(tmpre_start), e - N(tmpre_end));
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

string
latex_get_style (string s, int& b, int& e) {
  b= search_forwards ("\\documentclass", s);
  if (b >= 0) e= b + N (string ("\\documentclass"));
  else {
    b= search_forwards ("\\documentstyle", s);
    if (b >= 0) e= b + N (string ("\\documentstyle"));
    else { e= -1; return ""; }
  }
  skip_spaces (s, e);
  if (e<N(s) && s[e] == '[')
    if (!skip_square (s, e)) {
      b= e= -1; return ""; }
  skip_spaces (s, e);
  if (e<N(s) && s[e] == '{') {
    int bb= e+1;
    if (skip_curly (s, e)) {
      int ee= e-1;
      return s (bb, ee);
    }
  }
  b= e= -1; return "";
}

hashmap<string,path>
latex_get_packages (string s) {
  int pos= search_forwards (s, "\\begin{document}");
  if (pos != -1) s= s (0, pos);
  hashmap<string,path> h;
  string usepackage= "\\usepackage";  
  int i, n= N(s);
  for (i=0; i<n; )
    if (test (s, i, usepackage)) {
      int bb= i;
      i += N(usepackage);
      skip_square (s, i);
      skip_spaces (s, i);
      if (i<n && s[i] == '{') {
        int start= i+1;
        if (skip_curly (s, i)) {
          int ee= i;
          int j= start;
          skip_spaces (s, j);
          while (true) {
            int b= j;
            while (s[j] != ' ' && s[j] != ',' && s[j] != '}') j++;
            int e= j;
            h (s (b, e))= path (bb, ee);
            //cout << s (b, e) << " ~~> " << s (bb, ee) << LF;
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

static void
rewind_start_line (string s, int& i) {
  while (i>0 && s[i-1] != '\n') i--;
}

static void
skip_end_line (string s, int& i) {
  while (i<N(s) && s[i] != '\n') i++;
}

static bool
parse_declaration_sub (string s, int& i, string cmd,
                       int arity, bool trail_opt,
                       hashmap<string,path>& h) {
  // NOTE: routine parses all options between arguments,
  // as well as any trailing option if trail_opt holds.
  (void) trail_opt;
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
  if (cmd == "\\newtheorem") {
    int bb= b;
    rewind_start_line (s, bb);
    if (test (s, bb, "{\\theorembodyfont") && e<N(s) && s[e] == '}') {
      b= bb;
      skip_end_line (s, e);
    }
    else if (bb>0) {
      bb--;
      rewind_start_line (s, bb);
      if (test (s, bb, "\\theoremstyle")) {
        int bbb= bb-1;
        rewind_start_line (s, bb);
        if (test (s, bbb, "\\newtheoremstyle")) bb= bbb;
        b= bb;
      }
    }
  }
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
  return true;
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
    else if (test (s, i, "\\providecommand"))
      parse_declaration (s, i, "\\providecommand", 2, false, h);
    else if (test (s, i, "\\newenvironment"))
      parse_declaration (s, i, "\\newenvironment", 3, false, h);
    else if (test (s, i, "\\renewenvironment"))
      parse_declaration (s, i, "\\renewenvironment", 3, false, h);
    else if (test (s, i, "\\newtheorem"))
      parse_declaration (s, i, "\\newtheorem", 2, true, h);
    else if (test (s, i, "\\newlength"))
      parse_declaration (s, i, "\\newlength", 1, false, h);
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

/******************************************************************************
* Collecting LaTeX metadata
******************************************************************************/

static hashmap<string,int> metadata_commands (-1);
static hashmap<string,int> abstract_commands (-1);

static int
get_metadata_arity (string s, bool abs_flag) {
  if (N (metadata_commands) == 0) {
    metadata_commands ("\\accepted")= 1;
    metadata_commands ("\\address")= 1;
    metadata_commands ("\\additionalauthors")= 1;
    metadata_commands ("\\addtocmark")= 1;
    metadata_commands ("\\affiliation")= 1;
    metadata_commands ("\\altaffiliation")= 1;
    metadata_commands ("\\author")= 1;
    metadata_commands ("\\authorrunning")= 1;
    metadata_commands ("\\category")= 1;
    metadata_commands ("\\classification")= 1;
    metadata_commands ("\\contrib")= 1;
    metadata_commands ("\\copyrightholder")= 1;
    metadata_commands ("\\copyrightyear")= 1;
    metadata_commands ("\\curaddr")= 1;
    metadata_commands ("\\date")= 1;
    metadata_commands ("\\dedicatory")= 1;
    metadata_commands ("\\email")= 1;
    metadata_commands ("\\homepage")= 1;
    metadata_commands ("\\institute")= 1;
    metadata_commands ("\\footnotetext")= 1;
    metadata_commands ("\\maketitle")= 0;
    metadata_commands ("\\noaffiliation")= 1;
    metadata_commands ("\\numberofauthors")= 1;
    metadata_commands ("\\preprint")= 1;
    metadata_commands ("\\received")= 1;
    metadata_commands ("\\revised")= 1;
    metadata_commands ("\\subtitle")= 1;
    metadata_commands ("\\title")= 1;
    metadata_commands ("\\titlerunning")= 1;
    metadata_commands ("\\thanks")= 1;
    metadata_commands ("\\tmaffiliation")= 1;
    metadata_commands ("\\tmemail")= 1;
    metadata_commands ("\\tmhomepage")= 1;
    metadata_commands ("\\tmfnaffiliation")= 1;
    metadata_commands ("\\tmfnemail")= 1;
    metadata_commands ("\\tmfnhomepage")= 1;
    metadata_commands ("\\tmmisc")= 1;
    metadata_commands ("\\tmnote")= 1;
    metadata_commands ("\\tmsubtitle")= 1;
    metadata_commands ("\\toctitle")= 1;
    metadata_commands ("\\tocauthor")= 1;
    metadata_commands ("\\translator")= 1;
    metadata_commands ("\\urladdr")= 1;

    abstract_commands ("\\keywords")= 1;
    abstract_commands ("\\pacs")= 1;
    abstract_commands ("\\subclass")= 1;
    abstract_commands ("\\subjclass")= 1;
    abstract_commands ("\\terms")= 1;
    abstract_commands ("\\category")= 3;
    abstract_commands ("\\tmacm")= 1;
    abstract_commands ("\\tmarxiv")= 1;
    abstract_commands ("\\tmkeywords")= 1;
    abstract_commands ("\\tmmsc")= 1;
    abstract_commands ("\\tmpacs")= 1;
  }
  if (abs_flag) return abstract_commands [s];
  else return metadata_commands [s];
}

static bool
parse_command_sub (string s, int& i, string cmd, int arity,
                   hashmap<string,path>& h) {
  int b= i, n= N(s);
  i += N(cmd);
  for (int arg=0; arg<arity; arg++) {
    skip_whitespace (s, i);
    while (i<n && s[i] == '[') {
      if (!skip_square (s, i)) return false;
      skip_whitespace (s, i);
    }
    if (i<n && s[i] == '{') {
      if (!skip_curly (s, i)) return false;
    }
    else return false;
  }
  int e= i;
  if (cmd == "\\category") {
    skip_whitespace (s, i);
    if (i<n && s[i] == '[') {
      skip_square (s, i);
      e= i;
    }
  }
  string key= cmd;
  while (h->contains (key)) key << "#";
  h (key)= path (b, e);
  //cout << key << " ~~> " << s (b, e) << LF;
  return true;
}

static void
parse_command (string s, int& i, string cmd, int arity,
               hashmap<string,path>& h) {
  int b= i;
  if (!parse_command_sub (s, i, cmd, arity, h))
    i= b + N(cmd);
}

static void
parse_environment (string s, int& i, string env, hashmap<string,path>& h) {
  string envb= "\\begin{" * env * "}";
  string enve= "\\end{" * env * "}";
  skip_whitespace (s, i);
  int start= i;
  if (test (s, i, envb)) {
    int pos= search_forwards (enve, i + N(envb), s);
    if (pos > i) {
      string key= env;
      while (h->contains (key)) key << "#";
      h (key)= path (start, pos + N(enve));
      //cout << key << " ~~> " << s (start, pos + N(enve)) << LF;
      i= pos;
    }
  }
}

hashmap<string,path>
latex_get_metadata (string s, bool abs_flag) {
  hashmap<string,path> h;
  int i= 0, n= N(s);
  if (abs_flag) i= search_forwards ("\\begin{document}", s);
  if (i<0) return h;
  while (i<n)
    if (s[i] == '%') {
      if (test (s, i, "%%%%%%%%%% Start TeXmacs macros")) {
        int pos= search_forwards ("%%%%%%%%%% End TeXmacs macros", i, s);
        if (pos > i) i= pos;
      }
      skip_line (s, i);
    }
    else if (s[i] != '\\') i++;
    else if (!abs_flag && test (s, i, "\\begin{frontmatter}"))
      parse_environment (s, i, "frontmatter", h);
    else if (abs_flag && test (s, i, "\\begin{abstract}"))
      parse_environment (s, i, "abstract", h);
    else {
      int start= i;
      i++;
      while (i<n && is_alpha (s[i])) i++;
      string cmd= s (start, i);
      int arity= get_metadata_arity (cmd, abs_flag);
      if (arity >= 0) {
        i= start;
        parse_command (s, i, cmd, arity, h);
      }
      if (!abs_flag && cmd == "\\maketitle") break;
      if (abs_flag && cmd == "\\section") break;
    }
  return h;
}

array<path>
latex_get_metadata_snippets (string s, bool abs_flag) {
  hashmap<string,path> h= latex_get_metadata (s, abs_flag);
  hashmap<int,int> portions;
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    path p= h [it->next ()];
    portions (p[0])= p[1];
  }
  array<path> a;
  int i, n= N(s);
  for (i=0; i<n; ) {
    if (portions->contains (i)) {
      int j= portions[i];
      while (j<n)
        // TODO: we might refine the inclusion/exclusion of comments
        // inside the portions, by a careful examination of double line breaks
        if (s[j] == ' ' || s[j] == '\t' || s[j] == '\n') j++;
        else if (s[j] == '%') skip_line (s, j);
        else break;
      if (N(a) > 0 && a[N(a)-1][1] == i) a[N(a)-1][1]= j;
      else {
        //if (N(a) > 0) cout << "Skipped " << s (a[N(a)-1][1], i) << LF;
        a << path (i, j);
      }
      i= j;
    }
    else i++;
  }
  //cout << "a= " << a << ", " << abs_flag << LF;
  return a;
}

bool
latex_unchanged_metadata (string olds, string news, bool abs_flag) {
  array<path> oldps= latex_get_metadata_snippets (olds, abs_flag);
  array<path> newps= latex_get_metadata_snippets (news, abs_flag);
  if (N(oldps) != N(newps)) return false;
  for (int i=0; i<N(oldps); i++)
    if (olds (oldps[i][0], oldps[i][1]) != news (newps[i][0], newps[i][1]))
      return false;
  return true;
}
