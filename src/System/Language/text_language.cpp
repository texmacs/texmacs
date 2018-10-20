
/******************************************************************************
* MODULE     : text_language.cpp
* DESCRIPTION: natural textual languages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_configure.hpp"
#ifndef OS_MINGW
#include <langinfo.h>
#else
#include <winnls.h>
#endif

#include "analyze.hpp"
#include "hyphenate.hpp"
#include "impl_language.hpp"
#include "sys_utils.hpp"

#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

/******************************************************************************
* Western text languages / 8 bit charset
******************************************************************************/

struct text_language_rep: language_rep {
  hashmap<string,string> patterns;
  hashmap<string,string> hyphenations;

  text_language_rep (string lan_name, string hyph_name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

text_language_rep::text_language_rep (string lan_name, string hyph_name):
  language_rep (lan_name), patterns ("?"), hyphenations ("?") {
    load_hyphen_tables (hyph_name, patterns, hyphenations, true); }

text_property
text_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos]==' ') {
    pos++;
    // while ((pos<N(s)) && (s[pos]==' ')) pos++;
    if ((pos == N(s)) || (!is_punctuation (s[pos])))
      return &tp_space_rep;
    return &tp_nb_space_rep;
  }
  
  if (is_punctuation (s[pos])) {
    while ((pos<N(s)) && is_punctuation (s[pos])) pos++;
    if ((pos==N(s)) || (s[pos]!=' ')) return &tp_normal_rep;
    switch (s[pos-1]) {
    case ',': case ':': case ';': case '`': case '\'':
      return &tp_space_rep;
    case '.': case '!': case '?':
      return &tp_period_rep;
    }
    return &tp_space_rep;
  }

  if (s[pos]=='-') {
    pos++;
    while ((pos<N(s)) && (s[pos]=='-')) pos++;
    return &tp_hyph_rep;
  }

  if (is_iso_alpha (s[pos])) {
    while ((pos<N(s)) && is_iso_alpha (s[pos])) pos++;
    return &tp_normal_rep;
  }

  if (is_numeric (s[pos])) { // can not be a '.'
    while ((pos<N(s)) && is_numeric (s[pos])) pos++;
    while (s[pos-1]=='.') pos--;
    return &tp_normal_rep;
  }

  if (s[pos]=='<') {
    while ((pos<N(s)) && (s[pos]!='>')) pos++;
    if (pos<N(s)) pos++;
    return &tp_normal_rep;
  }

  pos++;
  return &tp_normal_rep;
}

array<int>
text_language_rep::get_hyphens (string s) {
  return ::get_hyphens (s, patterns, hyphenations);
}

void
text_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  array<int> penalty= get_hyphens (s);
  std_hyphenate (s, after, left, right, penalty[after]);
}

/******************************************************************************
* French typography
******************************************************************************/

struct french_language_rep: language_rep {
  hashmap<string,string> patterns;
  hashmap<string,string> hyphenations;

  french_language_rep (string lan_name, string hyph_name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

french_language_rep::french_language_rep (string lan_name, string hyph_name):
  language_rep (lan_name), patterns ("?"), hyphenations ("?") {
    load_hyphen_tables (hyph_name, patterns, hyphenations, true); }

inline bool
is_french_punctuation (register char c) {
  return is_punctuation (c) || (c=='\23') || (c=='\24');
}

text_property
french_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos]==' ') {
    pos++;
    if (pos>1 && s[pos-2] == '\23')
      return &tp_nb_thin_space_rep;
    // while ((pos<N(s)) && (s[pos]==' ')) pos++;
    if ((pos == N(s)) || (!is_french_punctuation (s[pos])))
      return &tp_space_rep;
    if (s[pos] == '\23')
      return &tp_space_rep;
    if (/*s[pos] == ':' ||*/ s[pos] == ';' ||
        s[pos] == '!' || s[pos] == '?' || s[pos] == '\24')
      return &tp_nb_thin_space_rep;
    return &tp_nb_space_rep;
  }
  
  if (is_french_punctuation (s[pos])) {
    while ((pos<N(s)) && is_french_punctuation (s[pos])) pos++;
    if ((pos==N(s)) || (s[pos]!=' ')) return &tp_normal_rep;
    switch (s[pos-1]) {
    case '\23':
      return &tp_nb_thin_space_rep;
    case '\24':
    case ',': case ':': case ';': case '`': case '\'':
      return &tp_space_rep;
    case '.': case '!': case '?':
      return &tp_period_rep;
    }
    return &tp_space_rep;
  }

  if (s[pos]=='-') {
    pos++;
    while ((pos<N(s)) && (s[pos]=='-')) pos++;
    return &tp_hyph_rep;
  }

  if (is_iso_alpha (s[pos])) {
    while ((pos<N(s)) && is_iso_alpha (s[pos])) pos++;
    return &tp_normal_rep;
  }

  if (is_numeric (s[pos])) { // can not be a '.'
    while ((pos<N(s)) && is_numeric (s[pos])) pos++;
    while (s[pos-1]=='.') pos--;
    return &tp_normal_rep;
  }

  if (s[pos]=='<') {
    while ((pos<N(s)) && (s[pos]!='>')) pos++;
    if (pos<N(s)) pos++;
    return &tp_normal_rep;
  }

  pos++;
  return &tp_normal_rep;
}

array<int>
french_language_rep::get_hyphens (string s) {
  return ::get_hyphens (s, patterns, hyphenations);
}

void
french_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  array<int> penalty= get_hyphens (s);
  std_hyphenate (s, after, left, right, penalty[after]);
}

/******************************************************************************
* Eastern text languages / using UCS entities
******************************************************************************/

struct ucs_text_language_rep: language_rep {
  hashmap<string,string> patterns;
  hashmap<string,string> hyphenations;

  ucs_text_language_rep (string lan_name, string hyph_name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  bool unicode;
};

ucs_text_language_rep::ucs_text_language_rep (string lan_name, string hyph_name):
  language_rep (lan_name), patterns ("?"), hyphenations ("?")
  { load_hyphen_tables (hyph_name, patterns, hyphenations, false); }

text_property
ucs_text_language_rep::advance (tree t, int& pos) {
  //TODO: replace methods is_punctuation (), is_iso_alpha () and is_numeric (),
  //      by equivalents taking into account unicode entities.
  string s= t->label;
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos]==' ') {
    pos++;
    // while ((pos<N(s)) && (s[pos]==' ')) pos++;
    if ((pos == N(s)) || (!is_punctuation (s[pos])))
      return &tp_space_rep;
    return &tp_nb_space_rep;
  }
  
  if (is_punctuation (s[pos])) {
    while ((pos<N(s)) && is_punctuation (s[pos])) pos++;
    if ((pos==N(s)) || (s[pos]!=' ')) return &tp_normal_rep;
    switch (s[pos-1]) {
    case ',': case ':': case ';': case '`': case '\'':
      return &tp_space_rep;
    case '.': case '!': case '?':
      return &tp_period_rep;
    }
    return &tp_space_rep;
  }

  if (s[pos]=='-') {
    pos++;
    while ((pos<N(s)) && (s[pos]=='-')) pos++;
    return &tp_hyph_rep;
  }

  if (is_iso_alpha (s[pos]) || (s[pos]=='<')) {
    while ((pos<N(s)) && (is_iso_alpha (s[pos]) || (s[pos]=='<'))) {
      if (s[pos]=='<') {
        while ((pos<N(s)) && (s[pos]!='>')) pos++;
        if (pos<N(s)) pos++;
      }
      else
        pos++;
    }
    return &tp_normal_rep;
  }

  if (is_numeric (s[pos])) { // can not be a '.'
    while ((pos<N(s)) && is_numeric (s[pos])) pos++;
    while (s[pos-1]=='.') pos--;
    return &tp_normal_rep;
  }

  pos++;
  return &tp_normal_rep;
}

array<int>
ucs_text_language_rep::get_hyphens (string s) {
  return ::get_hyphens (s, patterns, hyphenations, true);
}

void
ucs_text_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  array<int> penalty= get_hyphens (s);
  std_hyphenate (s, after, left, right, penalty[after], true);
}

/******************************************************************************
* Oriental languages
******************************************************************************/

struct oriental_language_rep: language_rep {
  hashmap<string,bool> punct;
  hashmap<string,bool> wide_punct;
  oriental_language_rep (string lan_name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

oriental_language_rep::oriental_language_rep (string lan_name):
  language_rep (lan_name), punct (false)
{
  punct (".")= true;
  punct (",")= true;
  punct (":")= true;
  punct (";")= true;
  punct ("!")= true;
  punct ("?")= true;
  punct ("<#3000>")= true;
  punct ("<#3001>")= true;
  punct ("<#3002>")= true;
  punct ("<#3003>")= true;
  punct ("<#3004>")= true;
  punct ("<#3005>")= true;
  punct ("<#3006>")= true;
  punct ("<#3007>")= true;
  punct ("<#3008>")= true;
  punct ("<#3009>")= true;
  punct ("<#300a>")= true;
  punct ("<#300b>")= true;
  punct ("<#300c>")= true;
  punct ("<#300d>")= true;
  punct ("<#300e>")= true;
  punct ("<#300f>")= true;
  punct ("<#300A>")= true;
  punct ("<#300B>")= true;
  punct ("<#300C>")= true;
  punct ("<#300D>")= true;
  punct ("<#300E>")= true;
  punct ("<#300F>")= true;
  punct ("<#ff01>")= true;
  punct ("<#ff0c>")= true;
  punct ("<#ff0e>")= true;
  punct ("<#ff1a>")= true;
  punct ("<#ff1b>")= true;
  punct ("<#ff1f>")= true;
  punct ("<#FF01>")= true;
  punct ("<#FF0C>")= true;
  punct ("<#FF0E>")= true;
  punct ("<#FF1A>")= true;
  punct ("<#FF1B>")= true;
  punct ("<#FF1F>")= true;

  wide_punct ("<#3001>")= true;
  wide_punct ("<#ff01>")= true;
  wide_punct ("<#ff0c>")= true;
  wide_punct ("<#ff0e>")= true;
  wide_punct ("<#ff1a>")= true;
  wide_punct ("<#ff1b>")= true;
  wide_punct ("<#ff1f>")= true;
  wide_punct ("<#FF01>")= true;
  wide_punct ("<#FF0C>")= true;
  wide_punct ("<#FF0E>")= true;
  wide_punct ("<#FF1A>")= true;
  wide_punct ("<#FF1B>")= true;
  wide_punct ("<#FF1F>")= true;
}

text_property
oriental_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos] == ' ') {
    pos++;
    return &tp_space_rep;
  }

  if (pos < N(s) && !test (s, pos, "<#")) {
    while (pos < N(s) && s[pos] != ' ' && !test (s, pos, "<#"))
      tm_char_forwards (s, pos);
    return &tp_cjk_no_break_rep;
  }

  int start= pos;
  tm_char_forwards (s, pos);
  string c= s (start, pos);
  int next= pos;
  tm_char_forwards (s, next);
  string x= s (pos, next);

  if (punct->contains (c)) {
    if (punct->contains (x) || pos == N(s))
      return &tp_cjk_no_break_period_rep;
    else if (wide_punct->contains (c))
      return &tp_cjk_wide_period_rep;
    else return &tp_cjk_period_rep;
  }
  else {
    if (punct->contains (x) || pos == N(s))
      return &tp_cjk_no_break_rep;
    else return &tp_cjk_normal_rep;
  }
}

array<int>
oriental_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  for (i=0; i<N(penalty); i++) penalty[i]= HYPH_INVALID;
  return penalty;
}

void
oriental_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after+1);
  right= s (after+1, N(s));
}

/******************************************************************************
* Locales
******************************************************************************/

#ifdef OS_MINGW
const string
windows_locale_to_language () {
  static string language;

  if (N(language) == 0) {
    LANGID lid= GetUserDefaultUILanguage();
    switch(PRIMARYLANGID(lid)) {
    case LANG_BULGARIAN:  language= "bulgarian"; break;
    case LANG_CHINESE:	   language= "chinese"; break;
    case LANG_CHINESE_TRADITIONAL: language= "taiwanese"; break;
    case LANG_CROATIAN:   language= "croatian"; break;
    case LANG_CZECH:      language= "czech"; break;
    case LANG_DANISH:     language= "danish"; break;
    case LANG_DUTCH:      language= "dutch"; break;
    case LANG_ENGLISH:
      switch(SUBLANGID(lid)) {
      case SUBLANG_ENGLISH_UK: language= "british"; break;
      default:            language= "english"; break;
      }
      break;
    case LANG_FRENCH:     language= "french"; break;
    case LANG_GERMAN:     language= "german"; break;
    case LANG_GREEK:      language= "greek"; break;
    case LANG_HUNGARIAN:  language= "hungarian"; break;
    case LANG_ITALIAN:    language= "italian"; break;
    case LANG_JAPANESE:   language= "japanese"; break;
    case LANG_KOREAN:     language= "korean"; break;
    case LANG_POLISH:     language= "polish"; break;
    case LANG_PORTUGUESE: language= "portuguese"; break;
    case LANG_ROMANIAN:   language= "romanian"; break;
    case LANG_RUSSIAN:    language= "russian"; break;
    case LANG_SLOVENIAN:  language= "slovene"; break;
    case LANG_SPANISH:    language= "spanish"; break;
    case LANG_SWEDISH:    language= "swedish"; break;
    case LANG_UKRAINIAN:  language= "ukrainian"; break;
    default:              language= "english"; break;
    }
  }
  return language;
}
#endif

string
locale_to_language (string s) {
  if (N(s) > 5) s= s (0, 5);
  if (s == "en_GB") return "british";
  if (s == "zh_TW") return "taiwanese";
  if (N(s) > 2) s= s (0, 2);
  if (s == "bg") return "bulgarian";
  if (s == "zh") return "chinese";
  if (s == "hr") return "croatian";
  if (s == "cs") return "czech";
  if (s == "da") return "danish";
  if (s == "nl") return "dutch";
  if (s == "en") return "english";
  if (s == "eo") return "esperanto";
  if (s == "fi") return "finnish";
  if (s == "fr") return "french";
  if (s == "de") return "german";
  if (s == "gr") return "greek";
  if (s == "hu") return "hungarian";
  if (s == "it") return "italian";
  if (s == "ja") return "japanese";
  if (s == "ko") return "korean";
  if (s == "pl") return "polish";
  if (s == "pt") return "portuguese";
  if (s == "ro") return "romanian";
  if (s == "ru") return "russian";
  if (s == "sl") return "slovene";
  if (s == "es") return "spanish";
  if (s == "sv") return "swedish";
  if (s == "uk") return "ukrainian";
  return "english";
}

string
language_to_locale (string s) {
  if (s == "american")   return "en_US";
  if (s == "british")    return "en_GB";
  if (s == "bulgarian")  return "bg_BG";
  if (s == "chinese")    return "zh_CN";
  if (s == "croatian")   return "hr_HR";
  if (s == "czech")      return "cs_CZ";
  if (s == "danish")     return "da_DK";
  if (s == "dutch")      return "nl_NL";
  if (s == "english")    return "en_US";
  if (s == "esperanto")  return "eo_EO";
  if (s == "finnish")    return "fi_FI";
  if (s == "french")     return "fr_FR";
  if (s == "german")     return "de_DE";
  if (s == "greek")      return "gr_GR";
  if (s == "hungarian")  return "hu_HU";
  if (s == "italian")    return "it_IT";
  if (s == "japanese")   return "ja_JP";
  if (s == "korean")     return "ko_KR";
  if (s == "polish")     return "pl_PL";
  if (s == "portuguese") return "pt_PT";
  if (s == "romanian")   return "ro_RO";
  if (s == "russian")    return "ru_RU";
  if (s == "slovene")    return "sl_SI";
  if (s == "spanish")    return "es_ES";
  if (s == "swedish")    return "sv_SV";
  if (s == "taiwanese")  return "zh_TW";
  if (s == "ukrainian")  return "uk_UA";
  return "en_US";
}

string
language_to_local_ISO_charset (string s) {
  if (s == "bulgarian")  return "ISO-8859-5";
  if (s == "chinese")    return "";
  if (s == "croatian")   return "ISO-8859-2";
  if (s == "czech")      return "ISO-8859-2";
  if (s == "greek")      return "ISO-8859-7";
  if (s == "hungarian")  return "ISO-8859-2";
  if (s == "japanese")   return "";
  if (s == "korean")     return "";
  if (s == "polish")     return "ISO-8859-2";
  if (s == "romanian")   return "ISO-8859-2";
  if (s == "russian")    return "ISO-8859-5";
  if (s == "slovene")    return "ISO-8859-2";
  if (s == "taiwanese")  return "";
  if (s == "ukrainian")  return "ISO-8859-5";
  return "ISO-8859-1";
}

string
get_locale_language () {
#if OS_MINGW
  return windows_locale_to_language ();
#else
  string env_lan= get_env ("LC_ALL");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LC_MESSAGES");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LANG");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("GDM_LANG");
  if (env_lan != "") return locale_to_language (env_lan);
  return "english";
#endif
}

string
get_locale_charset () {
#ifdef OS_MINGW
  // in principle for now we use 8-bit codepage stuff in windows (at least for filenames);
  // return language_to_local_ISO_charset (get_locale_language ());
  return ("UTF-8"); // do not change this!
  // otherwise there is a weird problem with page width shrinking on screen
#else
  return nl_langinfo (CODESET);
#endif
}

/******************************************************************************
* Getting a formatted date
******************************************************************************/

#ifdef QTTEXMACS
string
get_date (string lan, string fm) {
  return qt_get_date(lan, fm);
}

string
pretty_time (int t) {
  return qt_pretty_time (t);
}
#else

static bool
invalid_format (string s) {
  if (N(s) == 0) return true;
  for (int i=0; i<N(s); i++)
    if (!(is_alpha (s[i]) || is_numeric (s[i]) ||
	  s[i] == ' ' || s[i] == '%' || s[i] == '.' || s[i] == ',' ||
	  s[i] == '+' || s[i] == '-' || s[i] == ':'))
      return true;
  return false;
}

static string
simplify_date (string s) {
  int i, n=N(s);
  string r;
  for (i=0; i<n; i++)
    if ((s[i]!='0') || ((N(r)>0) && is_digit(r[N(r)-1]))) r << s[i];
  return r;
}

string
get_date (string lan, string fm) {
//#ifdef OS_MINGW
//  return win32::get_date(lan, fm);
  if (invalid_format (fm)) {
    if ((lan == "british") || (lan == "english") || (lan == "american"))
      fm= "%B %d, %Y";
    else if (lan == "german")
      fm= "%d. %B %Y";
    else if (lan == "chinese" || lan == "japanese" ||
	     lan == "korean" || lan == "taiwanese")
      {
        string y= simplify_date (var_eval_system ("date +\"%Y\""));
        string m= simplify_date (var_eval_system ("date +\"%m\""));
        string d= simplify_date (var_eval_system ("date +\"%d\""));
        if (lan == "korean")
          return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
	      return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
      }
    else fm= "%d %B %Y";
  }
  lan= language_to_locale (lan);
  string lvar= "LC_TIME";
  if (get_env (lvar) == "") lvar= "LC_ALL";
  if (get_env (lvar) == "") lvar= "LANG";
  string old= get_env (lvar);
  set_env (lvar, lan);
  string date= simplify_date (var_eval_system ("date +\"" * fm * "\""));
  if ((lan == "cz_CZ") || (lan == "hu_HU") || (lan == "pl_PL"))
    date= il2_to_cork (date);
  // if (lan == "ru_RU") date= iso_to_koi8 (date);
  set_env (lvar, old);
  return date;
}

string
pretty_time (int t) {
  return var_eval_system ("date -r " * as_string (t));
}
#endif

/******************************************************************************
* Main interface
******************************************************************************/

typedef const char* const_char_ptr;

static language
make_ucs_text_language (string s, string h) {
  return tm_new<ucs_text_language_rep> (s, h);
}

static language
make_text_language (string s, string h) {
  return tm_new<text_language_rep> (s, h);
}

static language
make_french_language (string s, string h) {
  return tm_new<french_language_rep> (s, h);
}

static language
make_oriental_language (string s) {
  return tm_new<oriental_language_rep> (s);
}

language
text_language (string s) {
  if (language::instances -> contains (s)) return language (s);
  if (s == "american")   return make_text_language (s, "us");
  if (s == "british")    return make_text_language (s, "ukenglish");
  if (s == "bulgarian")  return make_ucs_text_language (s, "bulgarian");
  if (s == "chinese")    return make_oriental_language (s);
  if (s == "croatian")   return make_text_language (s, "croatian");
  if (s == "czech")      return make_text_language (s, "czech");
  if (s == "danish")     return make_text_language (s, "danish");
  if (s == "dutch")      return make_text_language (s, "dutch");
  if (s == "english")    return make_text_language (s, "us");
  if (s == "esperanto")    return make_text_language (s, "esperanto");
  if (s == "finnish")    return make_text_language (s, "finnish");
  if (s == "french")     return make_french_language (s, "french");
  if (s == "german")     return make_text_language (s, "german");
  if (s == "greek")      return make_text_language (s, "greek");
  if (s == "hungarian")  return make_text_language (s, "hungarian");
  if (s == "italian")    return make_text_language (s, "italian");
  if (s == "japanese")   return make_oriental_language (s);
  if (s == "korean")     return make_oriental_language (s);
  if (s == "polish")     return make_text_language (s, "polish");
  if (s == "portuguese") return make_text_language (s, "portuguese");
  if (s == "romanian")   return make_text_language (s, "romanian");
  if (s == "russian")    return make_ucs_text_language (s, "russian");
  if (s == "slovene")    return make_text_language (s, "slovene");
  if (s == "spanish")    return make_text_language (s, "spanish");
  if (s == "swedish")    return make_text_language (s, "swedish");
  if (s == "taiwanese")  return make_oriental_language (s);
  if (s == "ukrainian")  return make_ucs_text_language (s, "ukrainian");
  if (s == "verbatim")   return tm_new<verb_language_rep> ("verbatim");
  failed_error << "The language was " << s << "\n";
  FAILED ("unknown language");
  return tm_new<verb_language_rep> ("verbatim");
}
