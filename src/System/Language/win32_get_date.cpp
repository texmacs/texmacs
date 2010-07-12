
/******************************************************************************
* MODULE     : win32_get_date.cpp
* DESCRIPTION: return formatted date under Windows
* COPYRIGHT  : (C) 2009  David Michel
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#if defined(__MINGW__) || defined(__MINGW32__) || defined(OS_WIN32)
#include <sstream>

#include "analyze.hpp"
#include "hyphenate.hpp"
#include "impl_language.hpp"
#include "sys_utils.hpp"
#include "converter.hpp"

namespace win32 {
  #include <windows.h>
  #include <wchar.h>

  const int MAX_UTF8_LEN = 3;

  WORD
  language_to_LCID (string s) {
    if (s == "american") return MAKELCID(LANG_ENGLISH, SUBLANG_ENGLISH_US);
    if (s == "british") return MAKELCID(LANG_ENGLISH, SUBLANG_ENGLISH_UK);
    if (s == "bulgarian") return MAKELCID(LANG_BULGARIAN, SUBLANG_BULGARIAN_BULGARIA);
    if (s == "chinese") return MAKELCID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED);
    if (s == "czech") return MAKELCID(LANG_CZECH, SUBLANG_CZECH_CZECH_REPUBLIC);
    if (s == "danish") return MAKELCID(LANG_DANISH, SUBLANG_DANISH_DENMARK);
    if (s == "dutch") return MAKELCID(LANG_DUTCH, SUBLANG_DUTCH);
    if (s == "english") return MAKELCID(LANG_ENGLISH, SUBLANG_ENGLISH_US);
    if (s == "finnish") return MAKELCID(LANG_FINNISH, SUBLANG_FINNISH_FINLAND);
    if (s == "french") return MAKELCID(LANG_FRENCH, SUBLANG_FRENCH);
    if (s == "german") return MAKELCID(LANG_GERMAN, SUBLANG_GERMAN);
    if (s == "hungarian") return MAKELCID(LANG_HUNGARIAN, SUBLANG_HUNGARIAN_HUNGARY);
    if (s == "italian") return MAKELCID(LANG_ITALIAN, SUBLANG_ITALIAN);
    if (s == "japanese") return MAKELCID(LANG_JAPANESE, SUBLANG_JAPANESE_JAPAN);
    if (s == "korean") return MAKELCID(LANG_KOREAN, SUBLANG_KOREAN);
    if (s == "polish") return MAKELCID(LANG_POLISH, SUBLANG_POLISH_POLAND);
    if (s == "portuguese") return MAKELCID(LANG_PORTUGUESE, SUBLANG_PORTUGUESE);
    if (s == "romanian") return MAKELCID(LANG_ROMANIAN, SUBLANG_ROMANIAN_ROMANIA);
    if (s == "russian") return MAKELCID(LANG_RUSSIAN, SUBLANG_RUSSIAN_RUSSIA);
    if (s == "slovene") return MAKELCID(LANG_SLOVENIAN, SUBLANG_SLOVENIAN_SLOVENIA);
    if (s == "spanish") return MAKELCID(LANG_SPANISH, SUBLANG_SPANISH);
    if (s == "swedish") return MAKELCID(LANG_SWEDISH, SUBLANG_SWEDISH);
    if (s == "taiwanese") return MAKELCID(LANG_THAI, SUBLANG_THAI_THAILAND);
    if (s == "ukrainian") return MAKELCID(LANG_UKRAINIAN, SUBLANG_UKRAINIAN_UKRAINE);
    return MAKELCID(LANG_ENGLISH, SUBLANG_ENGLISH_US);
  }

  int
  WCHAR_to_utf8 (char* uc, WCHAR wc)
  {
    if (wc < 0x80) {
      uc[0] = wc;
      return 1;
    } else if (wc < 0x800) {
      uc[0] = (wc >> 6) | 0xc0;
      uc[1] = (wc & 0x3f) | 0x80;
      return 2;
    } else {
      uc[0] = (wc >> 12) | 0xe0;
      uc[1] = ((wc >> 6) & 0x3f) | 0x80;
      uc[2] = (wc & 0x3f) | 0x80;
      return 3;
    }
  }

  string
  WCHARP_to_string (WCHAR* wcs) {
    int size = wcslen(wcs);
    char *cs = tm_new_array<char>(MAX_UTF8_LEN * size);
    int j = 0;
    for (int i = 0; i < size; ++i) {
      int t = WCHAR_to_utf8(&cs[j], wcs[i]);
      if (t !=  -1) j += t;
      else cs[j++] = '?';
    }
    cs[j] = 0;
    string res = cs;
    tm_delete_array(cs);
    return utf8_to_cork(res);
  }

  WCHAR*
  string_to_WCHARP (string s) {
    WCHAR* wcs = tm_new_array<WCHAR>(N(s) + 1);
    char* cs = as_charp(cork_to_utf8(s));
    int t, i = 0, j = 0;
    while (t = mbtowc(&wcs[j], &cs[i], MB_CUR_MAX)) {
      if (t != -1) {
        j++;
        i += t;
      } else {
        wcs[j] = L'?';
        wcs[j + 1] = 0;
        tm_delete_array(cs);
        return wcs;
      }
    }
    wcs[j] = 0;
    tm_delete_array(cs);
    return wcs;
  }

  string
  get_date (string lan, string fm) {
    SYSTEMTIME localtime;
    GetLocalTime(&localtime);
    if (fm == "") {
      if ((lan == "british") || (lan == "english") || (lan == "american"))
        fm = "MMMM d, yyyy";
      else if (lan == "german")
        fm = "d. MMMM yyyy";
      else if (lan == "chinese" || lan == "japanese" ||
               lan == "korean" || lan == "taiwanese")
      {
        string y = as_string(localtime.wYear);
        string m = as_string(localtime.wMonth);
        string d = as_string(localtime.wDay);
        if (lan == "japanese")
          return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
        if (lan == "korean")
          return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
        return y * "," * m * "," * d;
      }
      else fm = "d MMMM yyyy";
    }
    string loc = language_to_locale(lan);
    string old = get_env("LANG");
    set_env("LANG", loc);
    WCHAR* format = string_to_WCHARP(fm);
    int size = 1 + GetDateFormatW(language_to_LCID(lan), 0, &localtime, format, 0, 0);
    WCHAR* wcdate = tm_new_array<WCHAR>(size);
    GetDateFormatW(language_to_LCID(lan), 0, &localtime, format, wcdate, size);
    tm_delete_array(format);
    string date = WCHARP_to_string(wcdate);
    tm_delete_array(wcdate);
    set_env("LANG", old);
    return date;
  }
}
#endif

