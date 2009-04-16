
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

string ispell_dictionary (string lang);
string ispell_extra_args (string lang);
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

string
ispeller_rep::start () {
  if (is_nil (ln)) {
#ifdef OS_WIN32
    string prg= "\"$TEXMACS_PATH/bin/aspell/aspell.exe\"";
    string cmd= prg * " --data-dir=.%//data --dict-dir=.%//dict -a";
#else
    string cmd= "ispell -a -d " * ispell_dictionary (lan)
      * ispell_extra_args (lan);
    if (exists_in_path ("aspell")) cmd[0]= 'a';
#endif
    ln= make_pipe_link (cmd);
  }
  if (ln->alive) return "ok";
  string message= ln->start ();
  if (starts (message, "Error: ")) return message;
  message= retrieve ();
#ifdef OS_WIN32
  if (search_forwards (message, 0, "@(#)")) return "ok";
#else
  if (starts (message, "@(#)")) return "ok";
#endif
  return "Error: no dictionary for#" * lan;
}

string
ispeller_rep::retrieve () {
  string ret;
  while ((ret != "\n") && (!ends (ret, "\n\n")) &&
	 ((!ends (ret, "\n")) || (!starts (ret, "@(#)"))))
    {
      ln->listen (10000);
      string mess = ln->read (LINK_ERR);
      string extra= ln->read (LINK_OUT);
      if (mess  != "") cerr << "TeXmacs] ispell error: " << mess << "\n";
      if (extra == "") return "Error: ispell does not respond";
      ret << extra;
    }
  return ispell_decode(lan,ret);
}

void
ispeller_rep::send (string cmd) {
  ln->write (ispell_encode(lan,cmd) * "\n", LINK_IN);
}

/******************************************************************************
* Ispell dictionaries
******************************************************************************/

static hashmap<string,string> the_dict ("");

static void
init_dictionary (string lang, string dict) {
  if (the_dict->contains (lang)) return;
  if (exists ("/usr/lib/ispell/" * dict * ".hash") ||
      exists ("/usr/lib/aspell/" * dict) ||
      exists ("/usr/lib/aspell/" * dict * ".multi"))
    the_dict (lang)= dict;
}

string
ispell_dictionary (string lang) {
  if (N(the_dict) == 0) {
    init_dictionary ("english", "english");
    init_dictionary ("english", "american");
    init_dictionary ("danish", "danish");
    init_dictionary ("danish", "dansk");
    init_dictionary ("dutch", "dutch");
    init_dictionary ("dutch", "nederlands");
    init_dictionary ("french", "french");
    init_dictionary ("french", "francais");
    init_dictionary ("german", "german");
    init_dictionary ("german", "deutsch");
    init_dictionary ("german", "ngerman");
    init_dictionary ("german", "ndeutsch");
    init_dictionary ("german", "ogerman");
    init_dictionary ("german", "odeutsch");
    init_dictionary ("german", "swiss");
    init_dictionary ("portuguese", "portuguese");
    init_dictionary ("portuguese", "portugues");
    init_dictionary ("portuguese", "brazilian");
    init_dictionary ("portuguese", "brasileiro");
    init_dictionary ("spanish", "spanish");
    init_dictionary ("spanish", "español");
    init_dictionary ("spanish", "espa~nol");
    init_dictionary ("spanish", "espanol");
    init_dictionary ("spanish", "castellano");
    init_dictionary ("swedish", "swedish");
    init_dictionary ("swedish", "svenska");
  }
  if (the_dict->contains (lang)) return the_dict [lang];
  return lang;
}

/******************************************************************************
* Language dependent arguments to ispell
******************************************************************************/

string
ispell_extra_args (string lan) {
  if (lan == "german")
    return " -T latin1";
  else
    return "";
}

/******************************************************************************
* Internationalization
******************************************************************************/

string
ispell_encode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return cork_to_il2 (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return koi8_to_iso (s);
  else if (lan == "ukrainian")
    return koi8uk_to_iso (s);
  else if (lan == "spanish")
    return spanish_to_ispanish (s);
  else if (lan == "german")
    return german_to_igerman (s);
  else return s;
}

string
ispell_decode (string lan, string s) {
  if ((lan == "czech") || (lan == "hungarian") ||
      (lan == "polish") || (lan == "slovene"))
    return il2_to_cork (s);
  else if ((lan == "bulgarian") || (lan == "russian"))
    return iso_to_koi8 (s);
  else if (lan == "ukrainian")
    return iso_to_koi8uk (s);
  else if (lan == "spanish")
    return ispanish_to_spanish (s);
  else if (lan == "german")
    return igerman_to_german (s);
  else return s;
}

/******************************************************************************
* Subroutines
******************************************************************************/

static tree
parse_ispell (string s) {
  while (ends (s, "\n")) s= s (0, N(s)-1);
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
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  return sc->start ();
}

tree
ispell_check (string lan, string s) {
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
  ispell_send (lan, "@" * s);
}

void
ispell_insert (string lan, string s) {
  ispell_send (lan, "*" * s);
}

void
ispell_done (string lan) {
  ispell_send (lan, "#");
}
