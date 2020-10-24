
/******************************************************************************
* MODULE     : basic.cpp
* DESCRIPTION: fast global new and delete
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "Freetype/tt_file.hpp"
#include "dictionary.hpp"
#include "sys_utils.hpp"
#include "convert.hpp"

/******************************************************************************
* debugging
******************************************************************************/

static int debug_status= 0;

bool
debug (int which, bool write_flag) {
  if (write_flag) {
    debug_status= debug_status | (1 << which);
    return 0;
  }
  else return (debug_status & (1 << which)) > 0;
}

int
debug_off () {
  int status= debug_status;
  debug_status= 0;
  return status;
}

void
debug_on (int status) {
  debug_status= status;
}

static void
debug_set (int which, bool on) {
  if (on) debug_status= debug_status | (1 << which);
  else debug_status= debug_status & (~(1 << which));
}

void
debug_set (string s, bool on) {
  if (s == "auto") debug_set (DEBUG_FLAG_AUTO, on);
  else if (s == "verbose") debug_set (DEBUG_FLAG_VERBOSE, on);
  else if (s == "events") debug_set (DEBUG_FLAG_EVENTS, on);
  else if (s == "std") debug_set (DEBUG_FLAG_STD, on);
  else if (s == "io") debug_set (DEBUG_FLAG_IO, on);
  else if (s == "bench") debug_set (DEBUG_FLAG_BENCH, on);
  else if (s == "history") debug_set (DEBUG_FLAG_HISTORY, on);
  else if (s == "qt") debug_set (DEBUG_FLAG_QT, on);
  else if (s == "qt-widgets") debug_set (DEBUG_FLAG_QT_WIDGETS, on);
  else if (s == "keyboard") debug_set (DEBUG_FLAG_KEYBOARD, on);
  else if (s == "packrat") debug_set (DEBUG_FLAG_PACKRAT, on);
  else if (s == "flatten") debug_set (DEBUG_FLAG_FLATTEN, on);
  else if (s == "correct") debug_set (DEBUG_FLAG_CORRECT, on);
  else if (s == "convert") debug_set (DEBUG_FLAG_CONVERT, on);
}

static bool
debug_get (int which) {
  return (debug_status & (1 << which)) != 0;
}

bool
debug_get (string s) {
  if (s == "auto") return debug_get (DEBUG_FLAG_AUTO);
  else if (s == "verbose") return debug_get (DEBUG_FLAG_VERBOSE);
  else if (s == "events") return debug_get (DEBUG_FLAG_EVENTS);
  else if (s == "std") return debug_get (DEBUG_FLAG_STD);
  else if (s == "io") return debug_get (DEBUG_FLAG_IO);
  else if (s == "bench") return debug_get (DEBUG_FLAG_BENCH);
  else if (s == "history") return debug_get (DEBUG_FLAG_HISTORY);
  else if (s == "qt") return debug_get (DEBUG_FLAG_QT);
  else if (s == "qt-widgets") return debug_get (DEBUG_FLAG_QT_WIDGETS);
  else if (s == "keyboard") return debug_get (DEBUG_FLAG_KEYBOARD);
  else if (s == "packrat") return debug_get (DEBUG_FLAG_PACKRAT);
  else if (s == "flatten") return debug_get (DEBUG_FLAG_FLATTEN);
  else if (s == "correct") return debug_get (DEBUG_FLAG_CORRECT);
  else if (s == "convert") return debug_get (DEBUG_FLAG_CONVERT);
  else return false;
}

/******************************************************************************
* debugging messages
******************************************************************************/

tree debug_messages (TUPLE);
bool debug_lf_flag= false;
extern bool texmacs_started;

void
debug_message_sub (string channel, string msg) {
  if (occurs ("\n", msg)) {
    int pos= search_forwards ("\n", 0, msg);
    debug_message_sub (channel, msg (0, pos));
    debug_lf_flag= true;
    cout << "\n";
    if (pos+1 < N(msg))
      debug_message_sub (channel, msg (pos+1, N(msg)));
  }
  else {
    int n= N(debug_messages);
    if (!debug_lf_flag && n>0 && is_tuple (debug_messages[n-1], channel)) {
      tree *t= &(debug_messages[n-1][1]);
      *t= (*t)->label * msg;
      cout << msg;
    }
    else {
      debug_messages << tuple (channel, msg, "");
      debug_lf_flag= false;
      if (channel != "debug-boot") {
        cout << "TeXmacs] ";
        if (channel != "debug-automatic" &&
            channel != "boot-error")
          cout << channel << ", ";
      }
      cout << msg;
    }
  }
}

void
debug_message (string channel, string msg) {
  debug_message_sub (channel, msg);
  if (texmacs_started && channel != "debug-widgets")
    call ("notify-debug-message", object (channel));
}

void
debug_formatted (string channel, tree msg) {
  int n= N(debug_messages);
  if (n>0 && is_tuple (debug_messages[n-1], channel)) {
    debug_messages[n-1][2]= msg;
    if (texmacs_started && channel != "debug-widgets")
      call ("notify-debug-message", object (channel));
  }
}

tree
get_debug_messages (string kind, int max_number) {
  tree m (TUPLE);
  for (int i=N(debug_messages)-1; i>=0; i--) {
    tree t= debug_messages[i];
    if (!is_func (t, TUPLE, 3) || !is_atomic (t[0])) continue;
    string s= t[0]->label;
    if (kind == "Debugging console" ||
        ends (s, "-error") ||
        ends (s, "-warning"))
      m << t;
    if (N(m) >= max_number) break;
  }
  tree r (TUPLE);
  for (int i=N(m)-1; i>=0; i--) r << m[i];
  return r;
}

void
clear_debug_messages (string channel) {
  tree r= tree (TUPLE);
  for (int i=0; i<N(debug_messages); i++)
    if (is_func (debug_messages[i], TUPLE, 3) &&
        debug_messages[i][0] != channel)
      r << debug_messages[i];
  debug_messages= r;
  debug_lf_flag = false;
}

void
clear_debug_messages () {
  debug_messages= tree (TUPLE);
  debug_lf_flag = false;
}

#ifdef USE_EXCEPTIONS
string the_exception;
string the_report;
string get_crash_report (const char* msg);

void
tm_throw (const char* msg) {
  the_exception= msg;
  the_report   = get_crash_report (msg);
  cout << "Throwing " << msg << LF;
  cout << "-------------------------------------------------\n";
  cout << the_report << LF;
  cout << "-------------------------------------------------\n";
  throw string (msg);
}

void
handle_exceptions () {
  if (N (the_exception) != 0) {
    formatted arg (verbatim_to_tree (the_report, false, "utf-8"));
    failed_error << "Exception, " << the_exception << arg << LF;
    the_exception= "";
    the_report   = "";
  }
}
#endif

/******************************************************************************
* miscellaneous routines
******************************************************************************/

int
new_type_identifier () {
  static int id= 0;
  id--;
  return id;
}

static int current_indent= 0;

tm_ostream&
operator << (tm_ostream& out, display_control ctrl) {
  int i;
  switch (ctrl) {
  case INDENT:
    out << "  ";
    current_indent += 2;
    break;
  case UNINDENT:
    out << "\b\b";
    current_indent -= 2;
    break;
  case HRULE:
    for (i=current_indent; i<78; i++) out << "-";
  case LF:
    out << "\n";
    for (i=0; i<current_indent; i++) out << " ";
    break;    
  }
  return out;
}

/******************************************************************************
* Various TeXmacs blends
******************************************************************************/

bool
gui_is_x () {
#ifdef QTTEXMACS
  return false;
#else
  return true;
#endif
}

bool
gui_is_qt () {
#ifdef QTTEXMACS
  return true;
#else
  return false;
#endif
}

bool
os_win32 () {
#if defined (OS_WIN32)
  return true;
#else
  return false;
#endif
}

bool
os_mingw () {
#ifdef OS_MINGW
  return true;
#else
  return false;
#endif
}

bool
os_macos () {
#if defined (OS_MACOS)
  return true;
#else
  return false;
#endif
}

bool
use_macos_fonts () {
#ifdef OS_MACOS
  if (gui_is_qt ()) return true;
  string s= get_preference ("look and feel");
  if (s != "default" && s != "macos") return false;
  string l= get_output_language ();
  if (l == "bulgarian" || l == "russian" || l == "ukrainian") return false;
  return tt_font_exists ("LucidaGrande");
#else
  return false;
#endif
}

static const char*
default_look_and_feel_impl () {
  if (os_mingw () || os_win32 ()) return "windows";
  if (os_macos ()) return "macos";
  string session= get_env ("DESKTOP_SESSION");
  if (session == "") session= get_env ("XDG_CURRENT_DESKTOP");
  session= locase_all (session);
  if (occurs ("gnome", session))    return "gnome";
  if (occurs ("cinnamon", session)) return "gnome";
  if (occurs ("mate", session))     return "gnome";
  //if (occurs ("ubuntu", session))   return "unity";
  if (occurs ("kde", session))      return "kde";
  //if (occurs ("xfce", session))     return "xfce";
  //if (occurs ("lxde", session))     return "lxde";
  //return "emacs";
  return "gnome"; // default UI (much more "standard" than emacs) 
}

const char*
default_look_and_feel () {
  static const char* ret= default_look_and_feel_impl ();
  return ret;
}
