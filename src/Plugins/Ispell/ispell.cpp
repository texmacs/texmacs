
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

string ispell_encode (string lan, string s);
string ispell_decode (string lan, string s);
string ietf_languages_code (string s);

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

string
ispeller_rep::start () {
  if (is_nil (ln)) {
    string cmd;
#ifdef OS_WIN32
    string prg= "\"$TEXMACS_PATH/bin/aspell/aspell.exe\"";
    cmd= prg * " --data-dir=.%//data --dict-dir=.%//dict -a";
#else
    if (exists_in_path ("aspell")) cmd= "aspell";
    else
#if defined (__MINGW__) || defined (__MINGW32__)
      if (exists (url_system ("C:\\Program Files\\Aspell\\bin\\aspell.exe")))
        cmd= "\"C:\\Program Files\\Aspell\\bin\\aspell.exe\"";
      else
#endif
        return "Error: Aspell is not installed";
    cmd << " -a --encoding=utf-8 ";
    if (ietf_languages_code (lan) != "")
      cmd << "-l " << ietf_languages_code (lan);
#endif
    ln= make_pipe_link (cmd);
  }
  if (ln->alive) return "ok";
  string message= ln->start ();
  if (DEBUG_IO) cout << "Aspell] Received " << message << "\n";
  if (starts (message, "Error: ")) {
    if (ln->alive) ln->stop ();
    return message;
  }
  message= retrieve ();
  if (DEBUG_IO) cout << "Aspell] Received " << message << "\n";
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
#if defined (__MINGW__) || defined (__MINGW32__)
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
      if (mess  != "") cerr << "TeXmacs] aspell error: " << mess << "\n";
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
* Ispell dictionaries
******************************************************************************/

string
ietf_languages_code (string lang) {
  if (lang == "british")    return "en_GB";
  if (lang == "bulgarian")  return "bg_BG";
  if (lang == "chinese")    return "zh_CN";
  if (lang == "czech")      return "cs_CZ";
  if (lang == "danish")     return "da_DK";
  if (lang == "dutch")      return "nl_NL";
  if (lang == "finnish")    return "fi_FI";
  if (lang == "french")     return "fr_FR";
  if (lang == "german")     return "de_DE";
  if (lang == "hungarian")  return "hu_HU";
  if (lang == "italian")    return "it_IT";
  if (lang == "japanese")   return "ja_JP";
  if (lang == "korean")     return "ko_KR";
  if (lang == "polish")     return "pl_PL";
  if (lang == "portuguese") return "pt_PT";
  if (lang == "romanian")   return "ro_RO";
  if (lang == "russian")    return "ru_RU";
  if (lang == "slovene")    return "sl_SI";
  if (lang == "spanish")    return "es_ES";
  if (lang == "swedish")    return "sv_SE";
  if (lang == "taiwanese")  return "th_TH";
  if (lang == "ukrainian")  return "uk_UA";
  if (lang == "english")    return "en_US";
  return "";
}

/******************************************************************************
* Internationalization
******************************************************************************/

string
ispell_encode (string lan, string s) {
  if (lan == "bulgarian" || lan == "russian" || lan == "ukrainian")
    return t2a_to_utf8 (s);
  else return cork_to_utf8 (s);
}

string
ispell_decode (string lan, string s) {
  if (lan == "bulgarian" || lan == "russian" || lan == "ukrainian")
    return utf8_to_t2a (s);
  return utf8_to_cork (s);
}

/******************************************************************************
* Subroutines
******************************************************************************/

static tree
parse_ispell (string s) {
#if defined (__MINGW__) || defined (__MINGW32__)
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
  if (DEBUG_IO) cout << "Aspell] Start " << lan << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  return sc->start ();
}

tree
ispell_check (string lan, string s) {
  if (DEBUG_IO) cout << "Aspell] Check " << s << "\n";
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
  if (DEBUG_IO) cout << "Aspell] Accept " << s << "\n";
  ispell_send (lan, "@" * s);
}

void
ispell_insert (string lan, string s) {
  if (DEBUG_IO) cout << "Aspell] Insert " << s << "\n";
  ispell_send (lan, "*" * s);
}

void
ispell_done (string lan) {
  if (DEBUG_IO) cout << "Aspell] End " << lan << "\n";
  ispell_send (lan, "#");
}
