
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
  else if (s == "keyboard") debug_set (DEBUG_FLAG_KEYBOARD, on);
  else if (s == "packrat") debug_set (DEBUG_FLAG_PACKRAT, on);
  else if (s == "flatten") debug_set (DEBUG_FLAG_FLATTEN, on);
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
  else if (s == "keyboard") return debug_get (DEBUG_FLAG_KEYBOARD);
  else if (s == "packrat") return debug_get (DEBUG_FLAG_PACKRAT);
  else if (s == "flatten") return debug_get (DEBUG_FLAG_FLATTEN);
  else return false;
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
  else if (os_macos ()) return "macos";
  else if (get_env ("KDE_FULL_SESSION") != "") return "kde";
  else if (get_env ("GNOME_DESKTOP_SESSION_ID") != "") return "gnome";
  else if (eval_system ("pidof ksmserver") != "") return "kde";
  else if (eval_system ("pidof gnome-session") != "") return "gnome";
  //else if (eval_system ("pidof xfce-mcs-manage") != "") return "xfce";
  else return "emacs";
}

const char*
default_look_and_feel () {
  static const char* ret= default_look_and_feel_impl ();
  return ret;
}
