
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
  if (starts (model, "llama")) return "llama";
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
      r << '\\' << s[i];
      break;
    case '\'':
      r << "'\\''";
      break;
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
* History management
******************************************************************************/

hashmap<string,string> ia_last_id ("");

void
ai_get_continuation (string& s, string model, string chat) {
  if (chat == "") return;
  if (starts (model, "none")) {
    string key= model * "-" * chat;
    if (ia_last_id->contains (key)) {
      string id= ia_last_id[key];
      s= "Please follow up on your last answer with ID " * id * ". " * s;
    }
  }
}

void
ai_set_continuation (string s, string model, string chat) {
  if (chat == "") return;
  if (starts (model, "none")) {
    int pos= search_forwards ("\"id\":\"", s);
    if (pos < 0) return;
    pos += 6;
    int end= search_forwards ("\"", pos, s);
    if (end < 0) return;
    string key= model * "-" * chat;
    ia_last_id (key)= s (pos, end);
  }
}

/******************************************************************************
* Producing the query command for various engines
******************************************************************************/

string
chatgpt_command (string s, string model, string chat) {
  (void) chat;
  url u ("$TEXMACS_HOME_PATH/system/tmp/chatgpt.txt");
  if (save_string (u, s)) return "";
  string cmd= "openai -k 5000 complete " * as_string (u);
  return cmd;
}

string
llama_command (string s, string model, string chat) {
  (void) chat;
  string cmd= "curl http://localhost:11434/api/generate -d '{\n";
  cmd << "\"model\": \"" << model << "\",\n"
      << "\"prompt\": \"" << ai_quote (s) << "\",\n"
      << "\"stream\": false\n"
      << "}'";
  return cmd;
}

string
mistral_command (string s, string model, string chat) {
  (void) chat;
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
ai_command (string s, string model, string chat) {
  ai_get_continuation (s, model, chat);
  string engine= ai_engine (model);
  if (engine == "chatgpt") return chatgpt_command (s, model, chat);
  if (engine == "llama") return llama_command (s, model, chat);
  if (engine == "mistral") return mistral_command (s, model, chat);
  return "";
}

string
ai_latex_command (string s, string model, string chat) {
  string pre= "Please provide your answer in the form of an untitled LaTeX document.";
  return ai_command (pre * " " * s, model, chat);
}

/******************************************************************************
* Extracting the output for various engines
******************************************************************************/

string
chatgpt_output (string val, string model, string chat) {
  (void) model; (void) chat;
  return val;
}

string
llama_output (string val, string model, string chat) {
  (void) chat;
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
mistral_output (string val, string model, string chat) {
  (void) chat;
  int pos= search_forwards ("\"content\":\"", val);
  if (pos < 0) return "";
  pos += 11;
  int end= search_forwards ("\"}}]", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  return r;
}

string
ai_output (string s, string model, string chat) {
  ai_set_continuation (s, model, chat);
  string engine= ai_engine (model);
  if (engine == "chatgpt") return chatgpt_output (s, model, chat);
  if (engine == "llama") return llama_output (s, model, chat);
  if (engine == "mistral") return mistral_output (s, model, chat);
  return "";
}

string
un_escape_cr (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; )
    if (test (s, i, "\\neq")) r << s[i++];
    else if (test (s, i, "\\nearrow")) r << s[i++];
    else if (test (s, i, "\\n")) { r << '\n'; i += 2; }
    else r << s[i++];
  return r;
}

tree
ai_latex_output (string s, string model, string chat) {
  string r= ai_output (s, model, chat);
  if (DEBUG_IO) {
    cout << r << "\n";
    string x= un_escape_cr (r);
    x= "] " * replace (x, "\n", "\n] ");
    debug_io << x << "\n";
  }
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
  r= un_escape_cr (r);
  tree t= generic_to_tree (r, "latex-snippet");
  return tree (WITH, MODE, "text", t);
}

/******************************************************************************
* Chat with ai
******************************************************************************/

string
ai_chat (string s, string model, string chat) {
  string cmd= ai_command (s, model, chat);
  string val= eval_system (cmd);
  //if (DEBUG_IO) {
  //  debug_io << "input, " << cmd << LF;
  //  debug_io << "output, " << val << LF;
  //}
  string r= ai_output (val, model, chat);
  if (DEBUG_IO) {
    debug_io << "ai input, " << s << LF;
    debug_io << "ai output, " << r << LF;
  }
  return r;
}

string
ai_chat (string s, string model, string chat, string& pre, string& post) {
  string r= ai_chat (s, model, chat);
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
ai_correct (string s, string lan, string model, string chat) {
  string q= "If necessary, then please correct the spelling and grammar of the following " * lan * " text, and show me just the result, without further explanations or justifications: " * s;
  string pre, post;
  return ai_chat (q, model, chat, pre, post);
}

tree
ai_post (tree t, tree u) {
  while (is_document (t) && is_document (u) && N(t) > 0 && N(u) > 1 &&
         u[N(u)-1] == "" && t[N(t)-1] != "")
    u= u (0, N(u) - 1);
  return u;
}

tree
ai_correct (tree t, string lan, string model, string chat) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_correct (s, lan, model, chat);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  return ai_post (r, u);
}

/******************************************************************************
* Automatic translation
******************************************************************************/

string
ai_translate (string s, string from, string into, string model, string chat) {
  string q= "Please translate the following HTML snippet from ";
  q << from << " into " << into << ", without explanations: " << s;
  string pre, post;
  return ai_chat (q, model, chat, pre, post);
}

tree
ai_translate (tree t, string from, string into, string model, string chat) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_translate (s, from, into, model, chat);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  return ai_post (r, u);
}
