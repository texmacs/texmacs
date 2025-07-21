
/******************************************************************************
* MODULE     : llama.cpp
* DESCRIPTION: interface for the llama big language model
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

/******************************************************************************
* Quoting and unquoting
******************************************************************************/

string
llama_quote (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\"':
    case '\'':
    case '\\':
      r << '\\' << s[i];
      break;
    default:
      r << s[i];
    }
  return r;
}

string
llama_unquote (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\\' && (i+1 < n) &&
        (s[i+1] == '\\' || s[i+1] == '\"' || s[i+1] == '\''))
      r << s[++i];
    else r << s[i];
  return r;
}

/******************************************************************************
* Chat with llama
******************************************************************************/

string
llama_chat (string s) {
  string cmd= "curl http://localhost:11434/api/generate -d '{\n";
  cmd << "\"model\": \"llama3\",\n"
      << "\"prompt\": \"" << llama_quote (s) << "\",\n"
      << "\"stream\": false\n"
      << "}'";
  string val= eval_system (cmd);
  int pos= search_forwards ("\"response\":\"", val);
  if (pos < 0) return "";
  pos += 12;
  int end= search_forwards ("\",\"done\":", pos, val);
  if (end < 0) return "";
  //cout << "in = " << llama_quote (s) << "\n";
  //cout << "out= " << llama_unquote (val (pos, end)) << "\n";
  string r= llama_unquote (val (pos, end));
  r= replace (r, "`\\u003c", "<");
  r= replace (r, "\\u003e`", ">");
  r= replace (r, "\\u0026", "&");
  r= replace (r, "\\u003c", "<");
  r= replace (r, "\\u003e", ">");
  if (DEBUG_IO) {
    debug_io << "llama input, " << s << LF;
    debug_io << "llama output, " << r << LF;
  } 
  return r;
}

/******************************************************************************
* Automatic correction of spelling and grammar
******************************************************************************/

string
llama_correct (string s, string lan) {
  string q= "If necessary, then please correct the spelling and grammar of the following " * lan * " text, and show me just the result, without further explanations or justifications: " * s;
  string r= llama_chat (q);
  //cout << "\n";
  int lf= search_forwards ("\\n\\n", 0, r);
  if (lf >= 0) lf += 4;
  else {
    lf= search_forwards ("\n\n", 0, r);
    if (lf >= 0) lf += 2;
  }
  if (lf >= 0)
    if (search_forwards ("corrected", r (0, lf)) >= 0)
      r= r (lf, N(r));
  //cout << "r= " << r << "\n";
  return r;
}

tree
llama_correct (tree t, string lan) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= llama_correct (s, lan);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  while (is_document (t) && is_document (u) && N(t) > 0 && N(u) > 1 &&
         u[N(u)-1] == "" && t[N(t)-1] != "")
    u= u (0, N(u) - 1);
  //cout << "u = " << u << "\n";
  return u;
}

/******************************************************************************
* Automatic translation
******************************************************************************/

string
llama_translate (string s, string from, string into) {
  string q= "Please translate the following HTML snippet from ";
  q << from << " into " << into << ", without explanations: " << s;
  string r= llama_chat (q);
  //cout << "\n";
  int lf= search_forwards ("\\n\\n", 0, r);
  if (lf >= 0) lf += 4;
  else {
    lf= search_forwards ("\n\n", 0, r);
    if (lf >= 0) lf += 2;
  }
  if (lf >= 0)
    if (search_forwards ("translation", r (0, lf)) >= 0 ||
        search_forwards ("traduction", r (0, lf)) >= 0)
      r= r (lf, N(r));
  //cout << "r= " << r << "\n";
  return r;
}

tree
llama_translate (tree t, string from, string into) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= llama_translate (s, from, into);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  while (is_document (t) && is_document (u) && N(t) > 0 && N(u) > 1 &&
         u[N(u)-1] == "" && t[N(t)-1] != "")
    u= u (0, N(u) - 1);
  //cout << "u = " << u << "\n";
  return u;
}
