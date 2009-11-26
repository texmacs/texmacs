
/******************************************************************************
* MODULE     : text_language.cpp
* DESCRIPTION: natural textual languages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "hyphenate.hpp"
#include "impl_language.hpp"
#include "sys_utils.hpp"

/******************************************************************************
* Western text languages
******************************************************************************/

struct text_language_rep: language_rep {
  hashmap<string,string> H;

  text_language_rep (string lan_name, string hyph_name);
  text_property advance (string s, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

text_language_rep::text_language_rep (string lan_name, string hyph_name):
  language_rep (lan_name), H (load_hyphen_table (hyph_name)) {}

text_property
text_language_rep::advance (string s, int& pos) {
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos]==' ') {
    pos++;
    // while ((pos<N(s)) && (s[pos]==' ')) pos++;
    if ((pos == N(s)) || (!is_punctuation (s[pos])))
      return &tp_space_rep;
    return &tp_blank_rep;
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
  return ::get_hyphens (s, H);
}

void
text_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  array<int> penalty= get_hyphens (s);
  std_hyphenate (s, after, left, right, penalty[after]);
}

/******************************************************************************
* Oriental languages
******************************************************************************/

struct oriental_language_rep: language_rep {
  oriental_language_rep (string lan_name);
  text_property advance (string s, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

oriental_language_rep::oriental_language_rep (string lan_name):
  language_rep (lan_name) {}

text_property
oriental_language_rep::advance (string s, int& pos) {
  if (pos == N(s)) return &tp_normal_rep;

  if (s[pos]==' ') {
    pos++;
    if ((pos == N(s)) || (!is_punctuation (s[pos])))
      return &tp_space_rep;
    return &tp_blank_rep;
  }

  int begin= pos;
  while (pos<N(s) && s[pos] != ' ') {
    int start= pos;
    tm_char_forwards (s, pos);
    string c= s (start, pos);
    if (starts (c, "<#300") && N(c) == 7) {
      if (start > begin) pos= start;
      break;
    }
  }
  return &tp_normal_rep;
}

array<int>
oriental_language_rep::get_hyphens (string s) {
  int i, n= N(s);
  array<int> T (n-1);
  for (i=0; i<n-1; i++)
    T[i]= HYPH_INVALID;
  for (i=0, tm_char_forwards (s, i); i<n; tm_char_forwards (s, i))
    if (s[i] == '<')
      T[i-1]= 0;
  return T;
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

string
locale_to_language (string s) {
  if (N(s) > 5) s= s (0, 5);
  if (s == "en_GB") return "british";
  if (s == "zh_TW") return "taiwanese";
  if (N(s) > 2) s= s (0, 2);
  if (s == "bg") return "bulgarian";
  if (s == "zh") return "chinese";
  if (s == "cs") return "czech";
  if (s == "da") return "danish";
  if (s == "nl") return "dutch";
  if (s == "en") return "english";
  if (s == "fi") return "finnish";
  if (s == "fr") return "french";
  if (s == "de") return "german";
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
  if (s == "american") return "en_US";
  if (s == "british") return "en_GB";
  if (s == "bulgarian") return "bg_BG";
  if (s == "chinese") return "zh_CN";
  if (s == "czech") return "cs_CZ";
  if (s == "danish") return "da_DK";
  if (s == "dutch") return "nl_NL";
  if (s == "english") return "en_US";
  if (s == "finnish") return "fi_FI";
  if (s == "french") return "fr_FR";
  if (s == "german") return "de_DE";
  if (s == "hungarian") return "hu_HU";
  if (s == "italian") return "it_IT";
  if (s == "japanese") return "ja_JP";
  if (s == "korean") return "ko_KR";
  if (s == "polish") return "pl_PL";
  if (s == "portuguese") return "pt_PT";
  if (s == "romanian") return "ro_RO";
  if (s == "russian") return "ru_RU";
  if (s == "slovene") return "sl_SI";
  if (s == "spanish") return "es_ES";
  if (s == "swedish") return "sv_SV";
  if (s == "taiwanese") return "zh_TW";
  if (s == "ukrainian") return "uk_UA";
  return "en_US";
}

string
get_locale_language () {
  string env_lan= get_env ("LC_ALL");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LC_MESSAGES");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LANG");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("GDM_LANG");
  if (env_lan != "") return locale_to_language (env_lan);
  return "english";
}

/******************************************************************************
* Getting a formatted date
******************************************************************************/

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
#if defined(__MINGW__) || defined(__MINGW32__) || defined(OS_WIN32)
  return win32::get_date(lan, fm);
#else
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
	if (lan == "japanese")
	  return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
	if (lan == "korean")
	  return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
	return y * "," * m * "," * d;
      }
    else fm= "%d %B %Y";
  }
  lan= language_to_locale (lan);
  string old= get_env ("LANG");
  set_env ("LANG", lan);
  string date= simplify_date (var_eval_system ("date +\"" * fm * "\""));
  if ((lan == "cz_CZ") || (lan == "hu_HU") || (lan == "pl_PL"))
    date= il2_to_cork (date);
  // if (lan == "ru_RU") date= iso_to_koi8 (date);
  set_env ("LANG", old);
  return date;
#endif
}

/******************************************************************************
* Main interface
******************************************************************************/

typedef const char* const_char_ptr;

static language
make_text_language (string s, string h) {
  return tm_new<text_language_rep> (s, h);
}

language
text_language (string s) {
  if (language::instances -> contains (s)) return language (s);
  if (s == "american") return make_text_language (s, "us");
  if (s == "british") return make_text_language (s, "ukenglish");
  if (s == "bulgarian") return make_text_language (s, "bulgarian");
  if (s == "chinese") return tm_new<oriental_language_rep> (s);
  if (s == "czech") return make_text_language (s, "czech");
  if (s == "danish") return make_text_language (s, "danish");
  if (s == "dutch") return make_text_language (s, "dutch");
  if (s == "english") return make_text_language (s, "us");
  if (s == "finnish") return make_text_language (s, "finnish");
  if (s == "french") return make_text_language (s, "french");
  if (s == "german") return make_text_language (s, "german");
  if (s == "hungarian") return make_text_language (s, "hungarian");
  if (s == "italian") return make_text_language (s, "italian");
  if (s == "japanese") return tm_new<oriental_language_rep> (s);
  if (s == "korean") return tm_new<oriental_language_rep> (s);
  if (s == "polish") return make_text_language (s, "polish");
  if (s == "portuguese") return make_text_language (s, "portuguese");
  if (s == "romanian") return make_text_language (s, "romanian");
  if (s == "russian") return make_text_language (s, "russian");
  if (s == "slovene") return make_text_language (s, "slovene");
  if (s == "spanish") return make_text_language (s, "spanish");
  if (s == "swedish") return make_text_language (s, "swedish");
  if (s == "taiwanese") return tm_new<oriental_language_rep> (s);
  if (s == "ukrainian") return make_text_language (s, "ukrainian");
  if (s == "verbatim") return tm_new<verb_language_rep> ("verbatim");
  cerr << "\nThe language was " << s << "\n";
  FAILED ("unknown language");
  return tm_new<verb_language_rep> ("verbatim");
}
