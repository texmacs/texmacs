
/******************************************************************************
* MODULE     : input.cpp
* DESCRIPTION: Generic TeXmacs input
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "path.hpp"
#include "convert.hpp"
#include "hashmap.hpp"
#include "tm_link.hpp"
#include "Generic/input.hpp"
#include "scheme.hpp"
#include "vars.hpp"
#include "image_files.hpp"
#include "file.hpp"

#define STATUS_NORMAL  0
#define STATUS_ESCAPE  1
#define STATUS_BEGIN   2

#define MODE_VERBATIM  0
#define MODE_UTF8      1
#define MODE_SCHEME    2
#define MODE_LATEX     3
#define MODE_HTML      4
#define MODE_PS        5
#define MODE_MATH      6
#define MODE_CHANNEL   7
#define MODE_COMMAND   8
#define MODE_XFORMAT   9
#define MODE_FILE     10

/******************************************************************************
* Universal data input
******************************************************************************/

texmacs_input_rep::texmacs_input_rep (string type2):
  type (type2),
  status (STATUS_NORMAL),
  buf (""),
  format ("verbatim"),
  mode (get_mode (format)),
  channel (type),
  stack (""),
  ignore_verb (false),
  docs (tree (DOCUMENT, "")) { bof (); }

texmacs_input::texmacs_input (string type):
  rep (tm_new<texmacs_input_rep> (type)) {}

/******************************************************************************
* Mode and channel handling
******************************************************************************/

int
texmacs_input_rep::get_mode (string s) {
  if (s == "verbatim")  return MODE_VERBATIM;
  if (s == "utf8")  return MODE_UTF8;
  if (s == "latex") return MODE_LATEX;
  if (s == "scheme") return MODE_SCHEME;
  if (s == "html")  return MODE_HTML;
  if (s == "ps")  return MODE_PS;
  if (s == "math")  return MODE_MATH;
  if (s == "channel")  return MODE_CHANNEL;
  if (s == "command")  return MODE_COMMAND;
  if (s == "file") return MODE_FILE;
  if (format_exists (s)) return MODE_XFORMAT;
  return MODE_VERBATIM;
}

void
texmacs_input_rep::begin_mode (string s) {
  stack = tuple (format, channel, stack);
  format= s;
  mode  = get_mode (format);
}

void
texmacs_input_rep::begin_channel (string s) {
  stack  = tuple (format, channel, stack);
  channel= s;
  if ((channel == "prompt") || (channel == "input"))
    docs (channel)= tree (DOCUMENT, "");
}

void
texmacs_input_rep::end () {
  if (stack != "") {
    format = stack[0]->label;
    mode   = get_mode (format);
    channel= stack[1]->label;
    stack  = stack[2];
  }
}

/******************************************************************************
* Main routines
******************************************************************************/

bool
texmacs_input_rep::put (char c) { // returns true when expecting input
  /*
  if (c == DATA_BEGIN) cout << "[BEGIN]";
  else if (c == DATA_END) cout << "[END]";
  else if (c == DATA_ESCAPE) cout << "[ESCAPE]";
  else cout << c;
  */

  bool block_done= false;
  switch (status) {
  case STATUS_NORMAL:
    if (c == DATA_ESCAPE) status= STATUS_ESCAPE;
    else if (c == DATA_BEGIN) {
      flush (true);
      status= STATUS_BEGIN;
    }
    else if (c == DATA_ABORT &&
             (format == "verbatim" || format == "utf8") &&
             buf == "") {
      // Aborting sessions allows completion with a naive read-eval loop
      ignore_verb= true;
    }
    else if (c == DATA_END) {
      flush (true);
      end ();
      block_done= (stack == "");
      ignore_verb= (ignore_verb && stack != "");
    }
    else buf << c;
    break;
  case STATUS_ESCAPE:
    buf << c;
    status= STATUS_NORMAL;
    break;
  case STATUS_BEGIN:
    if (c == ':') {
      begin_mode (buf);
      buf   = "";
      status= STATUS_NORMAL;
    }
    else if (c == '#') {
      begin_channel (buf);
      buf   = "";
      status= STATUS_NORMAL;
    }
    else buf << c;
    break;
  }
  if (status == STATUS_NORMAL) flush ();
  return block_done;
}

void
texmacs_input_rep::bof () {
  format = "verbatim";
  channel= type;
  docs (channel)= tree (DOCUMENT, "");
}

void
texmacs_input_rep::eof () {
  flush (true);
}

void
texmacs_input_rep::write (tree u) {
  if (!docs->contains (channel))
    docs (channel)= tree (DOCUMENT, "");
  tree& t= docs (channel);
  if (!is_document (u)) u= tree (DOCUMENT, u);
  if (t[N(t)-1] == "") t[N(t)-1]= u[0];
  else if (u[0] != "") {
    if (!is_concat (t[N(t)-1])) t[N(t)-1]= tree (CONCAT, t[N(t)-1]);
    if (!is_concat (u[0])) u[0]= tree (CONCAT, u[0]);
    t[N(t)-1] << A(u[0]);
  }
  if (N(u)>1) t << A (u (1, N(u)));
}

tree
texmacs_input_rep::get (string ch) {
  if (!docs->contains (channel))
    docs (channel)= tree (DOCUMENT, "");
  tree& doc= docs (ch);
  if (doc == tree (DOCUMENT, "")) return "";
  tree t= doc;
  doc= tree (DOCUMENT, "");
  return t;
}

/******************************************************************************
* Flushing
******************************************************************************/

void
texmacs_input_rep::flush (bool force) {
  if ((!force) && (channel == "error") && (stack != "")) return;
  switch (mode) {
  case MODE_VERBATIM:
    verbatim_flush (force);
    break;
  case MODE_UTF8:
    utf8_flush (force);
    break;
  case MODE_SCHEME:
    scheme_flush (force);
    break;
  case MODE_LATEX:
    latex_flush (force);
    break;
  case MODE_HTML:
    html_flush (force);
    break;
  case MODE_PS:
    ps_flush (force);
    break;
  case MODE_MATH:
    math_flush (force);
    break;
  case MODE_CHANNEL:
    channel_flush (force);
    break;
  case MODE_COMMAND:
    command_flush (force);
    break;
  case MODE_XFORMAT:
    xformat_flush (force);
    break;
  case MODE_FILE:
    file_flush (force);
    break;
  default:
    FAILED ("invalid mode");
    break;
  }
}

void
texmacs_input_rep::verbatim_flush (bool force) {
  if (force || ends (buf, "\n")) {
    if (!ignore_verb)
      write (verbatim_to_tree (buf, false, "auto"));
    else if (DEBUG_IO)
      debug_io << "ignore verbatim (aborted input)" << LF;
    buf= "";
  }
}

void
texmacs_input_rep::utf8_flush (bool force) {
  if (force || ends (buf, "\n")) {
    if (!ignore_verb) {
      string tms= utf8_to_cork (buf);
      write (verbatim_to_tree (tms, false, "auto"));
    }
    else if (DEBUG_IO)
      debug_io << "ignore verbatim (aborted input)" << LF;
    buf= "";
  }
}

void
texmacs_input_rep::scheme_flush (bool force) {
  if (force) {
    write (simplify_correct (scheme_to_tree (buf)));
    buf= "";
  }
}

void
texmacs_input_rep::latex_flush (bool force) {
  if (force || ends (buf, "\n\n") || ends (buf, "\r\n\r\n")) {
    write (generic_to_tree (buf, "latex-snippet"));
    buf= "";
  }
}

void
texmacs_input_rep::html_flush (bool force) {
  if (force || ends (buf, "</P>")) {
    write (compound ("html-text", generic_to_tree (buf, "html-snippet")));
    buf= "";
  }
}

void
texmacs_input_rep::ps_flush (bool force) {
  if (force) {
    string pref= get_preference ("plugins:embedded postscript width");
    string w= (pref == "default") ? "0.7par" : pref;
    string h= "";
    string b= copy (buf);
    while (true)
      if (starts (b, "width=") || starts (b, "height=")) {
        int i=0;
        for (i=0; i<N(b); i++)
          if (b[i] == '\n') break;
        if (i == N(b)) break;
        if (b[0] == 'w') w= b (6, i);
        else h= b (7, i);
        b= b (i+1, N(b));
      }
      else break;
    tree t (IMAGE, tuple (tree (RAW_DATA, b), "ps"));
    t << w << h << "" << "";
    write (t);
    buf= "";
  }
}

void
texmacs_input_rep::math_flush (bool force) {
  if (force) {
    object obj= call ("string->object", buf);
    object cvr= call ("cas->stree", obj);
    tree t= as_tree (call ("tm->tree", cvr));
    write (tree (WITH, MODE, "math", t));
    buf= "";
  }
}

void
texmacs_input_rep::channel_flush (bool force) {
  if (force) {
    if ((buf == "prompt") || (buf == "input"))
      docs (buf)= tree (DOCUMENT, "");
    stack[1]= buf;
    buf= "";
  }
}

void
texmacs_input_rep::command_flush (bool force) {
  if (force) {
    eval ("(begin " * buf * ")");
    buf= "";
  }
}

void
texmacs_input_rep::xformat_flush (bool force) {
  if (force) {
    write (generic_to_tree (buf, format * "-snippet"));
    buf= "";
  }
}

void
texmacs_input_rep::image_flush (string content, string type, string w_unit,
                                string h_unit, double width, double height) {
  tree t (IMAGE);
  t << tuple (tree (RAW_DATA, content), type);
  if (width == 0 && height == 0) {
    t << tree ("0.618par") << tree ("");
  } else if (width != 0 && height == 0) {
    t << tree (as_string (width) * w_unit) << tree ("");
  } else if (width == 0 && height != 0) {
    t << tree ("") << tree (as_string (height) * h_unit);
  } else {
    t << tree (as_string (width) * w_unit) << tree (as_string (height) * h_unit);
  }
  t << tree ("") << tree ("");
  write (t);
}

void parse_url (string buf, string& path_file, string& w_unit,
                string& h_unit, double& width, double& height) {
  width= 0;
  height= 0;

  array<string> path_toks= tokenize (buf, "?");
  path_file= path_toks[0];
  string path_extra= (N(path_toks) >= 2? path_toks[1]: string (""));
  array<string> param_toks= tokenize (path_extra, "&");
  int i= 0;
  while (i < N(param_toks)) {
    string param= param_toks[i];
    if (starts (param, "width=")) {
      param= replace (param, "width=", "");
      parse_length (param, width, w_unit);
    }
    if (starts (param, "height=")) {
      param= replace (param, "height=", "");
      parse_length(param, height, h_unit);
    }
    i++;
  }
  if (is_empty (w_unit)) w_unit= "px";
  if (is_empty (h_unit)) h_unit= "px";
}

bool validate_h_unit (string unit) {
  return unit == "pt" || unit == "px" || unit == "pag";
}

bool validate_w_unit (string unit) {
  return unit == "pt" || unit == "px" || unit == "par";
}

void
texmacs_input_rep::file_flush (bool force) {
  if (force) {
    string path_file, w_unit, h_unit;
    double width, height;
    parse_url (buf, path_file, w_unit, h_unit, width, height);
    url file= url_system (path_file);

    if (! validate_h_unit (h_unit)) {
      string err_msg= h_unit * " is not allowed, please pt, px or pag!";
      write (verbatim_to_tree (err_msg, false, "auto"));
    }
    else if (! validate_w_unit (w_unit)) {
      string err_msg= w_unit * " is not allowed, please pt, px or par!";
      write (verbatim_to_tree (err_msg, false, "auto"));
    }
    else if (! exists (file)) {
      string err_msg= "[" * as_string(file) * "] does not exist";
      write (verbatim_to_tree (err_msg, false, "auto"));
    }
    else {
      string type= suffix (file);
      string content;
      load_string (file, content, false);

      // Set default width and height
      int real_w=0, real_h=0;
      if (width == 0 && height == 0) {
        if (type == "png") native_image_size (file, real_w, real_h);
        else if (type == "svg") svg_image_size (file, real_w, real_h);

        if (real_w > 0) width= real_w;
        if (real_h > 0) height= real_h;
      }

      if (type == "png") {
        image_flush (content, "png", w_unit, h_unit, width, height);
      }
      else if (type == "eps") {
        image_flush (content, "ps", w_unit, h_unit, width, height);
      } 
      else if (type == "pdf") {
        image_flush (content, "pdf", w_unit, h_unit, width, height);
      }
      else if (type == "svg") {
        if (real_w > 0) w_unit= "pt";
        if (real_h > 0) h_unit= "pt";
        image_flush (content, "svg", w_unit, h_unit, width, height);
      }
      else {
        string err_msg = "Do not support file type with suffix: [" * type * "]";
        write (verbatim_to_tree (err_msg, false, "auto"));
      }
    }
    buf= "";
  }
}
