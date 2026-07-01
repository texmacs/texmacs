
/******************************************************************************
* MODULE     : ai.cpp
* DESCRIPTION: interface for AI big language model
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*                  2026  Gregoire Lecerf
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
#include "scheme.hpp"

/******************************************************************************
* Various engines
******************************************************************************/

string
ai_engine (string model) {
  if (starts (model, "chatgpt")) return "chatgpt";
  if (starts (model, "gemini")) return "gemini";
  if (starts (model, "ollama")) return "ollama";
  if (starts (model, "open-mistral")) return "mistral";
  if (starts (model, "albert")) return "albert";
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
    case '\n':
      r << "\\n";
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
    if (s[i] == '\\' && (i+1 < n)) {
      if (s[i+1] == '\\' || s[i+1] == '\"' || s[i+1] == '\'')
	r << s[++i];
      else if (s[i+1] == 'n') {
	r << '\n'; i++;
      }
    }
    else r << s[i];
  return r;
}

/******************************************************************************
* TikZ pictures
******************************************************************************/

static bool
run_pdflatex (url tex) {
  if (!exists_in_path ("pdflatex")) {
    static bool warned= false;
    if (!warned) {
      convert_warning <<
	"pdflatex is not installed: TikZ pictures cannot be rendered" << LF;
      warned= true;
    }
    return false;
  }
  array<string> cmd;
  cmd << string ("pdflatex");
  cmd << string ("-output-directory=") * sys_concretize (head (tex));
  cmd << concretize (tex);
  //cout << cmd << LF;
  array<int> out; out << 1; out << 2;
  array<string> ret= evaluate_system (cmd, array<int> (),
				      array<string> (), out);
  //cout << "ret= " << ret << LF;
  if (ret [0] != "0" || ret[2] != "") {
    convert_warning << "cannot render TikZ picture" << LF;
    convert_warning << ret[1] << LF;
    convert_warning << ret[2] << LF;
    return false;
  }
  return true;
}

static string
replace_tikz_by_pdf (string s) {
  static int counter= 0;
  counter++;
  const string document_class_tag ("\\documentclass");
  const string beg_document_tag ("\\begin{document}");
  const string end_document_tag ("\\end{document}");
  const string beg_tikz_tag ("\\begin{tikzpicture}");
  const string end_tikz_tag ("\\end{tikzpicture}");
  int beg_document_class_pos= search_forwards (document_class_tag, s);
  if (beg_document_class_pos < 0) return s;
  int end_document_class_pos= beg_document_class_pos; 
  while (end_document_class_pos < N(s) &&
	 s[end_document_class_pos] != '}') end_document_class_pos++;
  if (end_document_class_pos == N(s)) return s;
  end_document_class_pos++;
  int beg_document_pos= search_forwards (beg_document_tag, s);
  int end_document_pos= search_forwards (end_document_tag, s);
  if (beg_document_pos < 0 || end_document_pos < 0) return s;
  int beg_tikz_pos= search_forwards (beg_tikz_tag, s);
  int end_tikz_pos= search_forwards (end_tikz_tag, s);
  if (beg_tikz_pos < 0 || end_tikz_pos < 0) return s;
  string r= string ("\\documentclass[border=3pt]{standalone}\n") *
    s (end_document_class_pos, beg_document_pos) *
    string ("\n") *  "\\usepackage{amsfonts}\n" *
    "\\usetikzlibrary{calc}\n" *
    beg_document_tag * string ("\n") *
    s (beg_tikz_pos, end_tikz_pos + N(end_tikz_tag)) *
    string ("\n") * end_document_tag * string ("\n");
  url temp= url_temp_dir ();
  url tex= temp * (as_string (counter) * ".tex");
  save_string (tex, r);
  if (!run_pdflatex (tex)) return s;
  url pdf= temp * (as_string (counter) * ".pdf");
  string aux= s (0, beg_tikz_pos) *
    string ("\n") * "\\includegraphics{" * sys_concretize (pdf) * "}\n" *
    s (end_tikz_pos + N(end_tikz_tag), N(s));
  return replace_tikz_by_pdf (aux);
}

/******************************************************************************
* History management
******************************************************************************/

// By ID

hashmap<string,string> ai_last_id ("");

void
ai_get_continuation (string& s, string model, string chat) {
  if (chat == "") return;
  //cout << "model= " << model << "\n";
  if (starts (model, "none")) {
    string key= model * "-" * chat;
    if (ai_last_id->contains (key)) {
      string id= ai_last_id[key];
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
    ai_last_id (key)= s (pos, end);
  }
}

// For albert, by passing previous prompts and answers

static const int ai_default_history_size= 3;
static hashmap<string,string> ai_current_prompt ("");
static list<string> null_string_list;
static hashmap<string,list<string> > ai_last_prompts (null_string_list);
static hashmap<string,list<string> > ai_last_answers (null_string_list);

static int
ai_get_history_size () {
  string s= get_preference ("albert chat history size");
  if (is_int (s)) return as_int (s);
  return ai_default_history_size;
}

static void
ai_set_current_prompt (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  ai_current_prompt(key)= s;
}

static string
ai_get_current_prompt (string model, string chat) {
  if (chat == "") return "";
  string key= model * "-" * chat;
  return ai_current_prompt[key];
}

static void
ai_set_last_prompt (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  list<string> l (s, ai_last_prompts[key]);
  ai_last_prompts(key)= l;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_prompts[key]) > max_size)
    ai_last_prompts(key)= head (ai_last_prompts[key], max_size); 
}

static list<string>
ai_get_last_prompts (string model, string chat) {
  if (chat == "") return null_string_list;
  string key= model * "-" * chat;
  return ai_last_prompts[key];
}

static void
ai_set_last_answer (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  list<string> l (s, ai_last_answers[key]);
  ai_last_answers(key)= l;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_answers[key]) > max_size)
    ai_last_answers(key)= head (ai_last_answers[key], max_size); 
}

static list<string>
ai_get_last_answers (string model, string chat) {
  if (chat == "") return null_string_list;
  string key= model * "-" * chat;
  return ai_last_answers[key];
}

/******************************************************************************
* Producing the query command for various engines
******************************************************************************/

string
chatgpt_command (string s, string model, string chat) {
  (void) model;
  (void) chat;
  url u ("$TEXMACS_HOME_PATH/system/tmp/chatgpt.txt");
  if (save_string (u, s)) return "";
  string cmd= "openai -k 5000 complete " * as_string (u);
  return cmd;
}

string
gemini_command (string s, string model, string chat) {
  (void) model;
  (void) chat;
  string key= get_env ("GEMINI_API_KEY");
  string gem= "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent";
  string cmd= "curl \"" * gem * "\" \\\n";
  cmd << "  -H 'Content-Type: application/json' \\\n"
      << "  -H 'X-goog-api-key: " << key << "' \\\n"
      << "  -X POST \\"
      << "  -d '{\n"
      << "    \"contents\": [ {\n"
      << "      \"parts\": [ {\n"
      << "        \"text\": \"" << ai_quote (s) << "\"\n"
      << "      } ]\n"
      << "    } ]\n"
      << "  }'";
  return cmd;
}

string
ollama_command (string s, string model, string chat) {
  (void) chat;
  string server= get_preference ("ollama server", "localhost");
  string port  = get_preference ("ollama port", "11434");
  string model_= get_preference ("ollama model", "default");
  if (model_ == "default") model_= as_string (call ("ollama-default-model"));
  string cmd= "curl http://" * server * ":" * port * "/api/generate -d '{\n";
  cmd << "\"model\": \"" << model_ << "\",\n"
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
albert_command (string s, string model, string agent, string chat) {
  (void) chat;
  string key= get_env ("ALBERT_API_KEY");
  string model_= get_preference (model * " model", model);
  ai_set_current_prompt (s, model, chat);
  list<string> last_prompts= reverse (ai_get_last_prompts (model, chat));
  list<string> last_answers= reverse (ai_get_last_answers (model, chat));
  string cmd= "curl -X POST \\\n";
  cmd << "  -H \"Authorization: Bearer " << key << "\" \\\n"
      << "  -H \"Content-Type: application/json\" \\\n"
      << "  -d '{\n"
      << "    \"model\": \"" << model_ << "\",\n"
      << "    \"messages\": [\n"
      << "      {\n"
      << "       \"role\": \"system\",\n"
      << "       \"content\": \"" << ai_quote (agent) << "\"\n"
      << "      },\n";
  while (!is_nil (last_prompts) && !is_nil (last_answers)) { cmd
      << "      {\n"
      << "       \"role\": \"user\",\n"
      << "       \"content\": \"" << ai_quote (last_prompts->item) << "\"\n"
      << "      },\n"
      << "      {\n"
      << "       \"role\": \"assistant\",\n"
      << "       \"content\": \"" << ai_quote (last_answers->item) << "\"\n"
      << "      },\n";
    last_prompts= last_prompts->next;
    last_answers= last_answers->next;
  }
  cmd << "      {\n"
      << "       \"role\": \"user\",\n"
      << "       \"content\": \"" << ai_quote (s) << "\"\n"
      << "      }\n"
      << "    ]\n"
      << "  }' \\\n"
      << "  https://albert.api.etalab.gouv.fr/v1/chat/completions";
    //cout << cmd << LF;
  return cmd;
}

string
ai_command (string s, string model, string agent, string chat) {
  ai_get_continuation (s, model, chat);
  string engine= ai_engine (model);
  string s_= agent * " " * s;
  if (engine == "chatgpt") return chatgpt_command (s_, model, chat);
  if (engine == "gemini") return gemini_command (s_, model, chat);
  if (engine == "ollama") return ollama_command (s_, model, chat);
  if (engine == "mistral") return mistral_command (s_, model, chat);
  if (engine == "albert") return albert_command (s, model, agent, chat);
  return "";
}

static string
ai_latex_agent_description (string model) {
  string engine= ai_engine (model);
  if (engine == "albert") return string ("Provide your answer in ")
    * "the form of an untitled utf-8 LaTeX document without any comments.";
  return string ("Please provide your answer in the form of an ")
    * "untitled LaTeX document.";
}

string
ai_latex_command (string s, string model, string chat) {
  string agent= ai_latex_agent_description (model);
  string r= ai_command (s, model, agent, chat);
  return r;
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
gemini_output (string val, string model, string chat) {
  //string x= val;
  //x= "> " * replace (x, "\n", "\n> ");
  //cout << x << "\n";
  (void) chat;
  (void) model;
  int pos= search_forwards ("\"text\": \"", val);
  if (pos < 0) return "";
  pos += 9;
  int end= pos;
  while (true) {
    end= search_forwards ("\"\n", end, val);
    if (end < 0) return "";
    int next= end + 2;
    while (next < N(val) && val[next] == ' ') next++;
    if (val[next] == '}') break;
    end= next;
  }
  string r= ai_unquote (val (pos, end));
  r= replace (r, "\\u0026", "&");
  r= replace (r, "\\u003c", "<");
  r= replace (r, "\\u003e", ">");
  return r;
}

string
ollama_output (string val, string model, string chat) {
  (void) chat;
  (void) model;
  int pos= search_forwards ("\"response\":\"", val);
  if (pos < 0) return "";
  pos += 12;
  int end= search_forwards ("\",\"done\":", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  r= replace (r, "\r\n", "\n");
  r= replace (r, "`\\u003c", "<");
  r= replace (r, "\\u003e`", ">");
  r= replace (r, "\\u0026", "&");
  r= replace (r, "\\u003c", "<");
  r= replace (r, "\\u003e", ">");
  if (occurs ("u003cbodyu003e", r) ||
      occurs ("u003cdiv id=", r)) {
    r= replace (r, "u003c", "<");
    r= replace (r, "u003e", ">");
  }
  return r;
}

string
mistral_output (string val, string model, string chat) {
  (void) chat;
  (void) model;
  int pos= search_forwards ("\"content\":\"", val);
  if (pos < 0) return "";
  pos += 11;
  int end= search_forwards ("\"}}]", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  return r;
}

string
albert_output (string val, string model, string chat) {
  (void) chat;
  (void) model;
  int pos= search_forwards ("\"content\":\"", val);
  if (pos < 0) return "";
  pos += 11;
  int end= search_forwards (",\"refusal\"", pos, val);
  if (end < 0) return "";
  string r= ai_unquote (val (pos, end));
  if (N(ai_get_current_prompt (model, chat)) > 0) {
    ai_set_last_prompt (ai_get_current_prompt (model, chat), model, chat);
    ai_set_last_answer (r, model, chat);
  }
  r= replace_tikz_by_pdf (r);
  // replace uft8 e2 80 af by ' '
  char* aux= (char*) malloc (N(r)+1);
  int j= 0;
  for (int i= 0; i < N(r); i++) {
    if (i+2 < N(r) &&
	(unsigned char) r[i]   == 0xe2 &&
	(unsigned char) r[i+1] == 0x80 &&
	(unsigned char) r[i+2] == 0xaf) {
      aux[j]= ' ';
      j++; i+=2;
      continue;
    }
    aux[j]= r[i]; j++;
  }
  r= string (aux, j);
  free (aux);
  return r;
}

string
ai_output (string s, string model, string chat) {
  ai_set_continuation (s, model, chat);
  string engine= ai_engine (model);
  if (engine == "chatgpt") return chatgpt_output (s, model, chat);
  if (engine == "gemini") return gemini_output (s, model, chat);
  if (engine == "ollama") return ollama_output (s, model, chat);
  if (engine == "mistral") return mistral_output (s, model, chat);
  if (engine == "albert") return albert_output (s, model, chat);
  return "";
}

string
un_escape_cr (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; )
    if (test (s, i, "\\nearrow")) r << s[i++];
    else if (test (s, i, "\\neq")) r << s[i++];
    else if (test (s, i, "\\noindent")) r << s[i++];
    else if (test (s, i, "\\n")) { r << '\n'; i += 2; }
    else r << s[i++];
  return r;
}

static tree
embed_images (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, IMAGE, 5)) {
    array<tree> a= A(t);
    url image= cork_to_utf8 (as_string (a[0]));
    string type= "", data;
    tree s (IMAGE);
    load_string (image, data, false);
    if (data == "") {
      std_error << "ai.cpp, cannot embed image\n";
      return t;
    }
    s << tuple (tree (RAW_DATA, data), as_string (tail (image)));
    s << a[1] << a[2] << a[3] << a[4];
    return s;
  }
  array<tree> a= A(t);
  for (int i= 0; i < N(a); i++)
    a[i]= embed_images (a[i]);
  return tree (L(t), a);
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
  r= replace (r, "\\begin{lstlisting}", "\\begin{verbatim}");
  r= replace (r, "\\end{lstlisting}", "\\end{verbatim}");
  r= un_escape_cr (r);
  tree t= generic_to_tree (r, "latex-snippet");
  t= embed_images (t);
  return tree (WITH, MODE, "text", t);
}

/******************************************************************************
* Chat with ai
******************************************************************************/

string
ai_chat (string s, string model, string agent, string chat) {
  string cmd= ai_command (s, model, agent, chat);
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

array<string>
ai_get_body (string r) {
  int start= search_forwards ("<body>", r);
  if (start < 0) { return array<string> (r, "", ""); }
  int end= search_forwards ("</body>", start, r);
  if (end < 0) { return array<string> (r, "", ""); }
  return array<string> (r (start, end), r (0, start), r (end+7, N(r)));
}

string
ai_chat (string s, string model, string agent, string chat,
	 string& pre, string& post) {
  string r= ai_chat (s, model, agent, chat);
  array<string> body= ai_get_body (r);
  pre = body[1];
  post= body[2];
  return body[0];
}

/******************************************************************************
* Automatic correction of spelling and grammar
******************************************************************************/

static string
ai_correct_agent_description (string lan, string model) {
  string engine= ai_engine (model);
  string q= string ("If necessary, then please correct the spelling ")
    * string ("and grammar of the following ") * lan
    * string (" text, and show me just the result, ")
    * string ("without further explanations or justifications:");
  if (engine == "albert")
    q = string ("You are a native " * lan * " speaker. ")
      * string ("Correct spelling, grammar, and improve the writing ")
      * string ("style of scientific documents. Show any explanations ")
      * string ("and justifications in comment tags at the end. ")
      * string ("Preserve HTML tags. Do not add new lines");
  return q;
}

string
ai_correct (string s, string lan, string model, string chat,
	    array<string>& comments) {
  string agent= ai_correct_agent_description (lan, model);
  string pre, post;
  string ret= ai_chat (s, model, agent, chat, pre, post);
  comments= array<string> ();
  int pos= 0, start;
  while ((start= search_forwards ("<!--", pos, post)) >= 0) {
    int end= search_forwards ("-->", start, post);
    if (end <= start) break;
    string comment= trim_spaces (post (start+4, end));
    if (N(comment) > 0)
      comments << comment;
    pos= end;
  }
  return ret;
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
  array<string> comments;
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_correct (s, lan, model, chat, comments);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  tree ret= tree (TUPLE);
  ret << ai_post (r, u);
  for (int i= 0; i < N(comments); i++)
    ret << decompress_html (comments[i]);
  return ret;
}

/******************************************************************************
* Automatic translation
******************************************************************************/

static string
ai_translate_agent_description (string from, string into, string model) {
  string engine= ai_engine (model);
  string q= "Please translate the following HTML snippet from ";
  q << from << " into " << into << ", without explanations: ";
  return q;
}

string
ai_translate (string s, string from, string into, string model, string chat) {
  string agent= ai_translate_agent_description (from, into, model);
  string pre, post;
  return ai_chat (s, model, agent, chat, pre, post);
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
