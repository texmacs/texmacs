
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

/******************************************************************************
* Transliteration
******************************************************************************/

hashmap<string,string> translit_table ("");
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
