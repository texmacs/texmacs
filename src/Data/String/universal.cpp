
/******************************************************************************
* MODULE     : universal.cpp
* DESCRIPTION: Internationalization for the universal character encoding
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "universal.hpp"
#include "hashmap.hpp"
#include "converter.hpp"

/******************************************************************************
* Transliteration
******************************************************************************/

static hashmap<string,string>&
get_translit_table () {
  static hashmap<string,string> t ("");
  return t;
}
hashmap<string,string> &translit_table = get_translit_table();
extern char Cork_unaccented[128];

static void
translit_set (int i, string s) {
  string h= as_hexadecimal (i);
  translit_table ("<#" * locase_all (h) * ">")= s;
  translit_table ("<#" * upcase_all (h) * ">")= s;
}

static void
translit_init () {
  if (N(translit_table) != 0) return;

  // Cork accented
  for (int i=0; i<128; i++) {
    string f; f << ((char) ((unsigned char) (i+128)));
    string c; c << Cork_unaccented[i];
    if (c != " ") translit_table (f)= c;
  }

  // Follow passport 2013 ICAO transliteration scheme (except for 42a, 44a)
  translit_set (0x401, "E");
  translit_set (0x410, "A");
  translit_set (0x411, "B");
  translit_set (0x412, "V");
  translit_set (0x413, "G");
  translit_set (0x414, "D");
  translit_set (0x415, "E");
  translit_set (0x416, "ZH");
  translit_set (0x417, "Z");
  translit_set (0x418, "I");
  translit_set (0x419, "I");
  translit_set (0x41A, "K");
  translit_set (0x41B, "L");
  translit_set (0x41C, "M");
  translit_set (0x41D, "N");
  translit_set (0x41E, "O");
  translit_set (0x41F, "P");
  translit_set (0x420, "R");
  translit_set (0x421, "S");
  translit_set (0x422, "T");
  translit_set (0x423, "U");
  translit_set (0x424, "F");
  translit_set (0x425, "KH");
  translit_set (0x426, "TS");
  translit_set (0x427, "CH");
  translit_set (0x428, "SH");
  translit_set (0x429, "SHCH");
  translit_set (0x42A, "");
  translit_set (0x42B, "Y");
  translit_set (0x42C, "");
  translit_set (0x42D, "E");
  translit_set (0x42E, "IU");
  translit_set (0x42F, "IA");
  translit_set (0x430, "a");
  translit_set (0x431, "b");
  translit_set (0x432, "v");
  translit_set (0x433, "g");
  translit_set (0x434, "d");
  translit_set (0x435, "e");
  translit_set (0x436, "zh");
  translit_set (0x437, "z");
  translit_set (0x438, "i");
  translit_set (0x439, "i");
  translit_set (0x43A, "k");
  translit_set (0x43B, "l");
  translit_set (0x43C, "m");
  translit_set (0x43D, "n");
  translit_set (0x43E, "o");
  translit_set (0x43F, "p");
  translit_set (0x440, "r");
  translit_set (0x441, "s");
  translit_set (0x442, "t");
  translit_set (0x443, "u");
  translit_set (0x444, "f");
  translit_set (0x445, "kh");
  translit_set (0x446, "ts");
  translit_set (0x447, "ch");
  translit_set (0x448, "sh");
  translit_set (0x449, "shch");
  translit_set (0x44A, "");
  translit_set (0x44B, "y");
  translit_set (0x44C, "");
  translit_set (0x44D, "e");
  translit_set (0x44E, "iu");
  translit_set (0x44F, "ia");
}

string
uni_translit (string s) {
  translit_init ();
  string r;
  int i=0, n=N(s);
  while (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    string ss= s (start, i);
    if (translit_table->contains (ss)) r << translit_table [ss];
    else r << ss;
  }
  return r;
}

/******************************************************************************
* Changing the case
******************************************************************************/

static hashmap<string,string>&
get_locase_tab () {
  static hashmap<string,string> t ("");
  return t;
}

static hashmap<string,string>&
get_upcase_tab () {
  static hashmap<string,string> t ("");
  return t;
}

static hashmap<string,bool>&
get_letter_tab () {
  static hashmap<string,bool> t (false);
  return t;
}

hashmap<string,string> locase_tab = get_locase_tab ();
hashmap<string,string> upcase_tab = get_upcase_tab ();
hashmap<string,bool  > letter_tab = get_letter_tab ();

static void
add_greek (string sym) {
  locase_tab ("<" * upcase_first (sym) * ">")= "<" * sym * ">";
  upcase_tab ("<" * sym * ">")= "<" * upcase_first (sym) * ">";
  upcase_tab ("<var" * sym * ">")= "<" * upcase_first (sym) * ">";
  letter_tab ("<" * upcase_first (sym) * ">")= true;
  letter_tab ("<" * sym * ">")= true;
  letter_tab ("<var" * sym * ">")= true;
}

static void
init_case_tables () {
  if (N(locase_tab) != 0) return;
  add_greek ("alpha");
  add_greek ("beta");
  add_greek ("gamma");
  add_greek ("delta");
  add_greek ("epsilon");
  add_greek ("zeta");
  add_greek ("eta");
  add_greek ("theta");
  add_greek ("iota");
  add_greek ("kappa");
  add_greek ("lambda");
  add_greek ("mu");
  add_greek ("nu");
  add_greek ("xi");
  add_greek ("omicron");
  add_greek ("pi");
  add_greek ("rho");
  add_greek ("sigma");
  add_greek ("tau");
  add_greek ("upsilon");
  add_greek ("phi");
  add_greek ("chi");
  add_greek ("psi");
  add_greek ("omega");
}

string
uni_locase_char (string s) {
  if (N(s) == 1) {
    unsigned char c= s[0];
    if ((c >= 'A' && c <= 'Z') ||
	(c >= ((unsigned char) 0x80) && (c <= ((unsigned char) 0x9F))) ||
	(c >= ((unsigned char) 0xC0) && (c <= ((unsigned char) 0xDF))))
      return string ((char) (c + 0x20));
    return s;
  }
  else if (starts (s, "<#") && ends (s, ">")) {
    int code= from_hexadecimal (s (2, N(s) - 1));
    if (code >= 0x386 && code <= 0x3AB) {
      if      (code >= 0x391 && code <= 0x3AB) code += 0x20;
      else if (code >= 0x386 && code <= 0x386) code += 0x26;
      else if (code >= 0x388 && code <= 0x38A) code += 0x25;
      else if (code >= 0x38C && code <= 0x38C) code += 0x40;
      else if (code >= 0x38E && code <= 0x38F) code += 0x3f;
    }
    else if (code >= 0x400 && code <= 0x40F) code += 0x50;
    else if (code >= 0x410 && code <= 0x42F) code += 0x20;
    else if (code >= 0x460 && code <= 0x4FF) {
      if ((code & 1) == 0) code += 1;
    }
    return "<#" * as_hexadecimal (code) * ">";
  }
  else {
    init_case_tables ();
    if (locase_tab->contains (s)) return locase_tab[s];
    return s;
  }
}

string
uni_upcase_char (string s) {
  if (N(s) == 1) {
    unsigned char c= s[0];
    if ((c >= 'a' && c <= 'z') ||
	(c >= ((unsigned char) 0xA0) && (c <= ((unsigned char) 0xBF))) ||
	(c >= ((unsigned char) 0xE0)))
      return string ((char) (c - 0x20));
    return s;
  }
  else if (starts (s, "<#") && ends (s, ">")) {
    int code= from_hexadecimal (s (2, N(s) - 1));
    if (code >= 0x3AC && code <= 0x3CE) {
      if      (code >= 0x3B1 && code <= 0x3CB) code -= 0x20;
      else if (code >= 0x3AC && code <= 0x3AC) code -= 0x26;
      else if (code >= 0x3AD && code <= 0x3AF) code -= 0x25;
      else if (code >= 0x3CC && code <= 0x3CC) code -= 0x40;
      else if (code >= 0x3CD && code <= 0x3CE) code -= 0x3f;
    }
    else if (code >= 0x450 && code <= 0x45F) code -= 0x50;
    else if (code >= 0x430 && code <= 0x44F) code -= 0x20;
    else if (code >= 0x460 && code <= 0x4FF) {
      if ((code & 1) == 1) code -= 1;
    }
    return "<#" * as_hexadecimal (code) * ">";
  }
  else {
    init_case_tables ();
    if (upcase_tab->contains (s)) return upcase_tab[s];
    return s;
  }
}

string
uni_locase_first (string s) {
  if (N(s) == 0) return s;
  int pos= 0;
  tm_char_forwards (s, pos);
  return uni_locase_char (s (0, pos)) * s (pos, N(s));
}

string
uni_upcase_first (string s) {
  if (N(s) == 0) return s;
  int pos= 0;
  tm_char_forwards (s, pos);
  return uni_upcase_char (s (0, pos)) * s (pos, N(s));
}

string
uni_locase_all (string s) {
  string r;
  int i=0, n=N(s);
  while (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    r << uni_locase_char (s (start, i));
  }
  return r;
}

string
uni_Locase_all (string s) {
  string r;
  int i=0, n=N(s);
  if (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    r << s (start, i);
  }
  while (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    r << uni_locase_char (s (start, i));
  }
  return r;
}

string
uni_upcase_all (string s) {
  string r;
  int i=0, n=N(s);
  while (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    r << uni_upcase_char (s (start, i));
  }
  return r;
}

/******************************************************************************
* Retrieving accents
******************************************************************************/

static array<string> accented_list;
static hashmap<string,string> unaccent_table;
static hashmap<string,string> get_accent_table;

static void
fill (array<int> a, int start, int kind) {
  for (int i=0; i<N(a); i++)
    if (a[i] != -1) {
      int code= start + i;
      string c= utf8_to_cork (encode_as_utf8 (code));
      string v= utf8_to_cork (encode_as_utf8 (a[i]));
      if (kind == 0) accented_list << c;
      if (kind == 0) unaccent_table (c)= v;
      else get_accent_table (c)= v;
    }
}

array<string>
get_accented_list () {
  (void) uni_unaccent_char ("a");
  return accented_list;
}

string
uni_unaccent_char (string s) {
  if (N(unaccent_table) != 0) return unaccent_table[s];
  array<int> a;
  a << 0x41 << 0x41 << 0x41 << 0x41 << 0x41 << 0x41 <<   -1 << 0x43
    << 0x45 << 0x45 << 0x45 << 0x45 << 0x49 << 0x49 << 0x49 << 0x49
    << 0x44 << 0x4E << 0x4F << 0x4F << 0x4F << 0x4F << 0x4F <<   -1
    << 0x4F << 0x55 << 0x55 << 0x55 << 0x55 << 0x59 <<   -1 <<   -1
    << 0x61 << 0x61 << 0x61 << 0x61 << 0x61 << 0x61 <<   -1 << 0x63
    << 0x65 << 0x65 << 0x65 << 0x65 << 0x69 << 0x69 << 0x69 << 0x69
    << 0x64 << 0x6E << 0x6F << 0x6F << 0x6F << 0x6F << 0x6F <<   -1
    << 0x6F << 0x75 << 0x75 << 0x75 << 0x75 << 0x79 <<   -1 << 0x79;
  fill (a, 0xC0, 0);
  return unaccent_table[s];
}

string
uni_get_accent_char (string s) {
  if (N(get_accent_table) != 0) return get_accent_table[s];
  array<int> a;
  a << 0x60 << 0x0B4 << 0x2C6 << 0x2DC << 0x0A8 << 0x2DA <<    -1 << 0xB8
    << 0x60 << 0x0B4 << 0x2C6 << 0x0A8 << 0x060 << 0x0B4 << 0x2C6 << 0xA8
    <<   -1 << 0x2DC << 0x060 << 0x0B4 << 0x2C6 << 0x2DC << 0x0A8 <<   -1
    <<   -1 << 0x060 << 0x0B4 << 0x2C6 << 0x0A8 << 0x0B4 <<    -1 <<   -1
    << 0x60 << 0x0B4 << 0x2C6 << 0x2DC << 0x0A8 << 0x2DA <<    -1 << 0xB8
    << 0x60 << 0x0B4 << 0x2C6 << 0x0A8 << 0x060 << 0x0B4 << 0x2C6 << 0xA8
    <<   -1 << 0x2DC << 0x060 << 0x0B4 << 0x2C6 << 0x2DC << 0x0A8 <<   -1
    <<   -1 << 0x060 << 0x0B4 << 0x2C6 << 0x0A8 << 0x0B4 <<    -1 << 0xA8;
  fill (a, 0xC0, 1);
  return get_accent_table[s];
}

string
uni_unaccent_all (string s) {
  (void) uni_unaccent_char ("a");
  string r;
  int i=0, n=N(s);
  while (i<n) {
    int start= i;
    tm_char_forwards (s, i);
    string c= s (start, i);
    if (unaccent_table->contains (c)) r << unaccent_table[c];
    else r << c;
  }
  return r;
}

/******************************************************************************
* Separate letters from punctuation
******************************************************************************/

bool
uni_is_letter (string s) {
  if (N(s) == 1) {
    unsigned char c= s[0];
    return
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (((unsigned int) c) >= 128 && (((unsigned int) c) & 97) != 31);
  }
  else if (starts (s, "<#") && ends (s, ">")) {
    int code= from_hexadecimal (s (2, N(s) - 1));
    return
      (code >= 0x3AC && code <= 0x3CE) ||
      (code >= 0x400 && code <= 0x481) ||
      (code >= 0x48A && code <= 0x4FF);
  }
  else {
    init_case_tables ();
    return letter_tab->contains (s);
  }
}

/******************************************************************************
* String comparison for bibliographic sorting
******************************************************************************/

bool
uni_before (string s1, string s2) {
  s1= uni_locase_all (uni_unaccent_all (s1));
  s2= uni_locase_all (uni_unaccent_all (s2));
  return s1 <= s2;
}
