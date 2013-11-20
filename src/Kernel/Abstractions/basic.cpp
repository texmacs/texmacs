
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
  else return false;
}

/******************************************************************************
* debugging messages
******************************************************************************/

tree debug_messages (TUPLE);
bool debug_lf_flag= false;
extern bool texmacs_started;

void
debug_message_sub (string channel, tree msg) {
  if (is_atomic (msg) && occurs ("\n", msg->label)) {
    string s= msg->label;
    int pos= search_forwards ("\n", 0, s);
    debug_message_sub (channel, s (0, pos));
    debug_lf_flag= true;
    cout << "\n";
    if (pos+1 < N(s))
      debug_message_sub (channel, s (pos+1, N(s)));
  }
  else {
    int n= N(debug_messages);
    if (!debug_lf_flag && n>0 && is_tuple (debug_messages[n-1], channel)) {
      tree *t= &(debug_messages[n-1][1]);
      if (is_concat (*t) && is_atomic ((*t)[N(*t)-1])) t= &((*t)[N(*t)-1]);
      if (is_atomic (msg) && is_atomic (*t)) *t= (*t)->label * msg->label;
      else if (is_concat (*t)) (*t) << msg;
      else *t= tree (CONCAT, *t, msg);
      cout << msg;
    }
    else {
      debug_messages << tuple (channel, msg);
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
debug_message (string channel, tree msg) {
  debug_message_sub (channel, msg);
  if (texmacs_started) call ("notify-debug-message", object (channel));
}

tree
get_debug_messages () {
  return debug_messages;
}

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
#if defined (__MINGW__) || defined (__MINGW32__)
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
  if (occurs ("gnome", session))    return "gnome";
  if (occurs ("cinnamon", session)) return "gnome";
  if (occurs ("mate", session))     return "gnome";
  if (occurs ("ubuntu", session))   return "unity";
  if (occurs ("kde", session))      return "kde";
  if (occurs ("xfce", session))     return "xfce";
  if (occurs ("lxde", session))     return "lxde";
  return "emacs";
}

const char*
default_look_and_feel () {
  static const char* ret= default_look_and_feel_impl ();
  return ret;
}
