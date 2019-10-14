
/******************************************************************************
* MODULE     : ispell.cpp
* DESCRIPTION: interface with the ispell spell checker
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Ispell/ispell.hpp"
#include "file.hpp"
#include "resource.hpp"
#include "tm_link.hpp"
#include "convert.hpp"
#include "locale.hpp"

string ispell_encode (string lan, string s);
string ispell_decode (string lan, string s);

/******************************************************************************
* The connection resource
******************************************************************************/

RESOURCE(ispeller);
struct ispeller_rep: rep<ispeller> {
  string  lan; // name of the session
  tm_link ln;  // the pipe

public:
  ispeller_rep (string lan);
  string start ();
  string retrieve ();
  void   send (string cmd);
};
RESOURCE_CODE(ispeller);

/******************************************************************************
* Routines for ispellers
******************************************************************************/

ispeller_rep::ispeller_rep (string lan2): rep<ispeller> (lan2), lan (lan2) {}

#ifdef OS_MINGW
bool find_win_spell(string &cmd , string &name) {
    url u;
    u= url_system ("$PROGRAMFILES\\Aspell\\bin\\aspell.exe");
    if (exists (u)) {
          cmd= escape_sh (as_string (u));
          name= "aspell";
          return true;
        }
    u= url_system ("$PROGRAMFILES(x86)\\Aspell\\bin\\aspell.exe");
    if (exists (u)) {
          cmd= escape_sh (as_string (u));
          name= "aspell";
          return true;
        }
    u= url_system ("$PROGRAMFILES\\Hunspell\\bin\\hunspell.exe");
    if (exists (u)){
          cmd= escape_sh (as_string (u));
          name= "hunspell";
          return true;
        }
    u= url_system ("$PROGRAMFILES(x86)\\Hunspell\\bin\\hunspell.exe");
    if (exists (u)) {
          cmd= escape_sh (as_string (u));
          name= "hunspell";
          return true;
        }
    return false;
}
#endif

string
ispeller_rep::start () {
  if (is_nil (ln)) {
    string cmd,lang_opt,enc_opt,name;
#ifdef OS_WIN32
    string prg= "\"$TEXMACS_PATH/bin/aspell/aspell.exe\"";
    cmd= prg * " --data-dir=.%//data --dict-dir=.%//dict -a";
#else
    if (exists_in_path ("aspell")) {
      cmd= "aspell";
      name = cmd;
    }
    else 
      if (exists_in_path ("hunspell")) {
        cmd= "hunspell";
        name = cmd;
      }
      else 
#ifdef OS_MINGW
      if (!find_win_spell(cmd , name))
#endif
        return "Error: Cannot find spellchecker (neither Aspell nor Hunspell) ";
    
    if (DEBUG_IO) debug_spell << "using "<< name <<"\n";
    if (name == "hunspell") {
      lang_opt= "-d ";
      enc_opt= "-i ";
    }
    else {
        lang_opt= "-l ";
        enc_opt= "--encoding=";
    }

    cmd << " -a "<< enc_opt << "utf-8 ";
    if (language_to_locale (lan) != "")
      cmd << lang_opt << language_to_locale (lan);
#endif
    ln= make_pipe_link (cmd);
  }
  if (ln->alive) return "ok";
  string message= ln->start ();
  if (DEBUG_IO) debug_spell << "Received " << message << "\n";
  if (starts (message, "Error: ")) {
    if (ln->alive) ln->stop ();
    return message;
  }
  message= retrieve ();
  if (DEBUG_IO) debug_spell << "Received " << message << "\n";
#ifdef OS_WIN32
  if (search_forwards (message, 0, "@(#)")) return "ok";
#else
  if (starts (message, "@(#)")) return "ok";
#endif
  if (ln->alive) ln->stop ();
  return "Error: no dictionary for " * lan;
}

string
ispeller_rep::retrieve () {
  string ret;
#ifdef OS_MINGW
  while ((ret != "\r\n") && (!ends (ret, "\r\n\r\n")) &&
	 ((!ends (ret, "\r\n")) || (!starts (ret, "@(#)"))))
#else
  while ((ret != "\n") && (!ends (ret, "\n\n")) &&
	 ((!ends (ret, "\n")) || (!starts (ret, "@(#)"))))
#endif
    {
      ln->listen (10000);
      string mess = ln->read (LINK_ERR);
      string extra= ln->read (LINK_OUT);
      if (mess  != "") io_error << "Aspell error: " << mess << "\n";
      if (extra == "") {
	ln->stop ();
	return "Error: aspell does not respond";
      }
      ret << extra;
    }
  return ispell_decode (lan, ret);
}

void
ispeller_rep::send (string cmd) {
  ln->write (ispell_encode (lan, cmd) * "\n", LINK_IN);
}

/******************************************************************************
* Internationalization
******************************************************************************/

string
ispell_encode (string lan, string s) {
  (void) lan;
  return cork_to_utf8 (s);
}

string
ispell_decode (string lan, string s) {
  (void) lan;
  return utf8_to_cork (s);
}

/******************************************************************************
* Subroutines
******************************************************************************/

static tree
parse_ispell (string s) {
#ifdef OS_MINGW
  while (ends (s, "\r\n")) s= s (0, N(s)-2);
#else
  while (ends (s, "\n")) s= s (0, N(s)-1);
#endif
  bool flag= true;
  int i, j;
  tree t (TUPLE);
  for (i=0, j=0; j<N(s); j++)
    if (s[j]==':') flag= false;
    else if (((s[j]==' ') && (flag || (j==i) || (s[j-1]==':'))) || (s[j]==','))
      {
	if (j>i) t << s (i, j);
	i= j+1;
      }
  t << s (i, j);

  if (N(t) == 0) return tree (TUPLE, "0");
  if ((t[0] == "+") || (t[0] == "*") || (t[0] == "-")) return "ok";
  if ((N(t)>=4) && ((t[0] == "&") || (t[0]=="?"))) {
    tree u (TUPLE, t[2]);
    u << A (t (4, N (t)));
    return u;
  }
  return tree (TUPLE, "0");
}

static void
ispell_send (string lan, string s) {
  ispeller sc= ispeller (lan);
  if ((!is_nil (sc)) && sc->ln->alive) sc->send (s);
}

static string
ispell_eval (string lan, string s) {
  ispeller sc= ispeller (lan);
  if ((!is_nil (sc)) && sc->ln->alive) {
    sc->send (s);
    return sc->retrieve ();
  }
  return "";
}

/******************************************************************************
* Spell checking interface
******************************************************************************/

string
ispell_start (string lan) {
  if (DEBUG_IO) debug_spell << "Start " << lan << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  return sc->start ();
}

tree
ispell_check (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Check " << s << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc) || (!sc->ln->alive)) {
    string message= ispell_start (lan);
    if (starts (message, "Error: ")) return message;
  }
  string ret_s= ispell_eval (lan, "^" * s);
  if (starts (ret_s, "Error: ")) return ret_s;
  return parse_ispell (ret_s);
}

void
ispell_accept (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Accept " << s << "\n";
  ispell_send (lan, "@" * s);
}

void
ispell_insert (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Insert " << s << "\n";
  ispell_send (lan, "*" * s);
}

void
ispell_done (string lan) {
  if (DEBUG_IO) debug_spell << "End " << lan << "\n";
  ispell_send (lan, "#");
}
