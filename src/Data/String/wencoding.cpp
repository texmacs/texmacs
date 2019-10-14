
/******************************************************************************
* MODULE     : wencoding.cpp
* DESCRIPTION: Makes heuristic detection of western european charsets.
* COPYRIGHT  : (C) 2012 Francois Poulain
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "wencoding.hpp"
#include "converter.hpp"
#include "locale.hpp"

static char
controls[32] = {
// 0x00                   BEL BS HT LF    FF CR    0x0F
    	1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1,
	    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1
// 0x10                               ESC          0x1F
};

static bool
is_control_char (unsigned char c) {
  return c < 0x20 && controls[c] == 1;
}

static bool
looks_ascii (unsigned char c) {
  return c < 0x7F && !is_control_char (c);
}

bool
looks_ascii (string s) {
  int i=0, n=N(s);
  for (; i<n; i++) {
    if (!looks_ascii (s[i]))
      return false;
  }
  return true;
}

static bool
looks_utf8 (string s, bool bom) {
  int i=0, n=N(s);
  for (; i<n; i++) {
    unsigned char c= s[i];
    if (looks_ascii (c))
      continue;
    if (!bom && is_control_char (c))
      return false;
    unsigned int code= decode_from_utf8 (s, i);
    i--;
    if (code == c && !(is_control_char (c) && bom)) {
      return false;
    }
  }
  return true;
}

bool
looks_utf8 (string s) {
  return looks_utf8 (s, false);
}

bool
looks_utf8_with_bom (string s) {
  int i=0;
  unsigned int code= decode_from_utf8 (s, i), BOM= 0xFEFF;
  return code == BOM && looks_utf8 (s, true);
}

static bool
looks_iso_8859 (unsigned char c) {
  return looks_ascii (c) || c >= 0xA0;
}

bool
looks_iso_8859 (string s) {
  int i=0, n=N(s);
  for (; i<n; i++) {
    if (!looks_iso_8859 (s[i]))
      return false;
  }
  return true;
}

bool
looks_universal (string s) {
  // Looks if s can be from TeXmacs's universal charset
  int i=0, n=N(s);
  bool glyph= false;
  for (; i<n; i++) {
    if (i+2 < n && s[i] == '<') {
      bool unicode= s[++i] == '#';
      if (unicode) i++;
      while (i<n && s[i] != '>') {
        if (unicode && !is_digit (s[i]))
          return false;
        if (!unicode && !(is_alpha (s[i]) || s[i] == '-'))
          return false;
        i++;
      }
      if (i<n && s[i] == '>') glyph= true;
      else return false;
    }
    else if (s[i] == '>') return false;
  }
  return glyph;
}

string
guess_wencoding (string s) {
  if (looks_ascii (s))         return "ASCII";
  if (looks_utf8_with_bom (s)) return "UTF-8-BOM";
  if (looks_utf8 (s))          return "UTF-8";
  if (looks_iso_8859 (s))      return "ISO-8859";
  return "other";
}

string
western_to_cork (string s) {
  string charset= guess_wencoding (s);
  if (charset == "UTF-8-BOM") return convert (s(3, N(s)), "UTF-8", "Cork");
  if (charset == "UTF-8")     return convert (s, "UTF-8", "Cork");
  if (charset == "ISO-8859") {
    charset= language_to_local_ISO_charset (get_locale_language ());
    if (charset != "")
      return convert (s, charset, "Cork");
  }
  if (looks_universal (s)) return s;
  return tm_encode (s);
}
string
western_to_utf8 (string s) {
  string charset= guess_wencoding (s);
  if (charset == "UTF-8-BOM") return s(3, N(s));
  if (charset == "UTF-8")     return s;
  if (charset == "ISO-8859") {
    charset= language_to_local_ISO_charset (get_locale_language ());
    if (charset != "")
      return convert (s, charset, "UTF-8");
  }
  return s;
}
