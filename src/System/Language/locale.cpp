
/******************************************************************************
* MODULE     : locale.cpp
* DESCRIPTION: Locale related routines
* COPYRIGHT  : (C) 1999-2019  Joris van der Hoeven, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "locale.hpp"

#ifndef OS_MINGW
#include <langinfo.h>
#ifndef X11TEXMACS
#include <locale>
#endif
#else
#include <winnls.h>
#endif


#define outline Core_outline
#define extend Core_extends
#ifdef OS_MACOS
#include <CoreFoundation/CFLocale.h>
#include <CoreFoundation/CFString.h>
#endif
#undef extend
#undef outline

#include "sys_utils.hpp"
#include "analyze.hpp"

#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

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
    case LANG_CHINESE:	  language= "chinese"; break;
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
    case LANG_SLOVAK:     language= "slovak"; break;
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

#ifdef OS_MACOS
string
get_mac_language () {
  char mac_lang[50];
  CFLocaleRef locale= CFLocaleCopyCurrent ();
  CFTypeRef lang= CFLocaleGetValue (locale, kCFLocaleLanguageCode);
  CFStringGetCString ((CFStringRef) lang, mac_lang, sizeof (mac_lang), kCFStringEncodingUTF8);
  CFRelease (locale);
  return string (mac_lang);
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
  if (s == "sk") return "slovak";
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
  if (s == "slovak")     return "sk_SK";
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
  if (s == "slovak")     return "ISO-8859-2";
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
#ifdef OS_MACOS
  return locale_to_language (get_mac_language ());
#endif
  return "english";
#endif
}

string
get_locale_charset () {
#ifdef OS_MINGW
  // in principle for now we use 8-bit codepage stuff in windows (at least for filenames);
  // return language_to_local_ISO_charset (get_locale_language ());
  return "UTF-8"; // do not change this!
  // otherwise there is a weird problem with page width shrinking on screen
#elif OS_MACOS
  return "UTF-8";
#elif X11TEXMACS
  return "UTF-8";
#elif OS_HAIKU
  return "UTF-8";
#else
  std::locale previous= std::locale::global (std::locale(""));
  string charset= string (nl_langinfo (CODESET));
  std::locale::global (previous);
  return charset;
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

