
/******************************************************************************
* MODULE     : ai.cpp
* DESCRIPTION: interface for AI big language model
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
#include "file.hpp"

/******************************************************************************
* Various engines
******************************************************************************/

string
ai_engine (string model) {
  if (starts (model, "chatgpt")) return "chatgpt";
  if (starts (model, "llama3")) return "llama";
  if (starts (model, "open-mistral")) return "mistral";
  return "unknown";
}

/******************************************************************************
* Useful syntactic subroutines
******************************************************************************/

string
ai_quote (string s) {
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
ai_unquote (string s) {
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
* Producing the query command for various engines
******************************************************************************/

string
chatgpt_command (string s, string model) {
  url u ("$TEXMACS_HOME_PATH/system/tmp/chatgpt.txt");
  cout << u << ", " << as_string (u) << "\n";
  if (save_string (u, s)) return "";
  string cmd= "openai -k 5000 complete " * as_string (u);
  cout << "cmd= " << cmd << "\n";
  return cmd;
}

string
llama_command (string s, string model) {
  string cmd= "curl http://localhost:11434/api/generate -d '{\n";
  cmd << "\"model\": \"" << model << "\",\n"
      << "\"prompt\": \"" << ai_quote (s) << "\",\n"
      << "\"stream\": false\n"
      << "}'";
  return cmd;
}

string
mistral_command (string s, string model) {
  string key= get_env ("MISTRAL_API_KEY");
  string cmd= "curl -X POST \\\n";
  cmd << "  -H \"Authorization: Bearer " << key << "\" \\\n"
      << "  -H \"Content-Type: application/json\" \\\n"
      << "  -d '{\n"
      << "    \"model\": \"" << model << "\",\n"
      << "    \"messages\": [ {\n"
      << "      \"role\": \"user\",\n"
      << "      \"content\": \"" << ai_quote (s) << "\"\n"
      << "    } ]\n"
      << "  }' \\\n"
      << "  https://api.mistral.ai/v1/chat/completions";
  return cmd;
}

string
ai_command (string s, string model) {
  string engine= ai_engine (model);
  if (engine == "chatgpt") return chatgpt_command (s, model);
  if (engine == "llama") return llama_command (s, model);
  if (engine == "mistral") return mistral_command (s, model);
  return "";
}

string
ai_latex_command (string s, string model) {
  string pre= "Please provide your answer in the form of an untitled LaTeX document.";
  return ai_command (pre * " " * s, model);
}

/******************************************************************************
* Extracting the output for various engines
******************************************************************************/

string
chatgpt_output (string val, string model) {
  (void) model;
  return val;
}

string
llama_output (string val, string model) {
  int pos= search_forwards ("\"response\":\"", val);
  if (pos < 0) return "";
  pos += 12;
  int end= search_forwards ("\",\"done\":", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  r= replace (r, "`\\u003c", "<");
  r= replace (r, "\\u003e`", ">");
  r= replace (r, "\\u0026", "&");
  r= replace (r, "\\u003c", "<");
  r= replace (r, "\\u003e", ">");
  return r;
}

string
mistral_output (string val, string model) {
  int pos= search_forwards ("\"content\":\"", val);
  if (pos < 0) return "";
  pos += 11;
  int end= search_forwards ("\"}}]", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  return r;
}

string
ai_output (string s, string model) {
  string engine= ai_engine (model);
  if (engine == "chatgpt") return chatgpt_output (s, model);
  if (engine == "llama") return llama_output (s, model);
  if (engine == "mistral") return mistral_output (s, model);
  return "";
}

tree
ai_latex_output (string s, string model) {
  string r= ai_output (s, model);
  string pre, post;
  int start= search_forwards ("\\begin{document}", r);
  if (start < 0) { pre= ""; post= ""; return r; }
  int end= search_forwards ("\\end{document}", start, r);
  if (end < 0) { pre= ""; post= ""; return r; }
  start += 16;
  pre= r (0, start);
  post= r (end, N(r));
  r= r (start, end);
  r= replace (r, "\\maketitle", "");
  r= replace (r, "\\n", "\n");
  tree t= generic_to_tree (r, "latex-snippet");
  return tree (WITH, MODE, "text", t);
}

/******************************************************************************
* Chat with ai
******************************************************************************/

string
ai_chat (string s, string model) {
  string cmd= ai_command (s, model);
  string val= eval_system (cmd);
  //if (DEBUG_IO) {
  //  debug_io << "ai input, " << cmd << LF;
  //  debug_io << "ai output, " << val << LF;
  //}
  string r= ai_output (val, model);
  if (DEBUG_IO) {
    debug_io << "ai input, " << s << LF;
    debug_io << "ai output, " << r << LF;
  }
  return r;
}

string
ai_chat (string s, string model, string& pre, string& post) {
  string r= ai_chat (s, model);
  int start= search_forwards ("<body>", r);
  if (start < 0) { pre= ""; post= ""; return r; }
  int end= search_forwards ("</body>", start, r);
  if (end < 0) { pre= ""; post= ""; return r; }
  pre= r (0, start);
  post= r (end, N(r));
  return r (start, end);
}

/******************************************************************************
* Automatic correction of spelling and grammar
******************************************************************************/

string
ai_correct (string s, string lan, string model) {
  string q= "If necessary, then please correct the spelling and grammar of the following " * lan * " text, and show me just the result, without further explanations or justifications: " * s;
  string pre, post;
  return ai_chat (q, model, pre, post);
}

tree
ai_post (tree t, tree u) {
  while (is_document (t) && is_document (u) && N(t) > 0 && N(u) > 1 &&
         u[N(u)-1] == "" && t[N(t)-1] != "")
    u= u (0, N(u) - 1);
  return u;
}

tree
ai_correct (tree t, string lan, string model) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_correct (s, lan, model);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  return ai_post (r, u);
}

/******************************************************************************
* Automatic translation
******************************************************************************/

string
ai_translate (string s, string from, string into, string model) {
  string q= "Please translate the following HTML snippet from ";
  q << from << " into " << into << ", without explanations: " << s;
  string pre, post;
  return ai_chat (q, model, pre, post);
}

tree
ai_translate (tree t, string from, string into, string model) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_translate (s, from, into, model);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  return ai_post (r, u);
}
