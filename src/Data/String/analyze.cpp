
/******************************************************************************
* MODULE     : analyze.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "merge_sort.hpp"
#include "converter.hpp"
#include "scheme.hpp"
#include "ntuple.hpp"

/******************************************************************************
* Tests for characters
******************************************************************************/

bool
is_iso_alpha (char c) {
  int i= ((int) ((unsigned char) c));
  return
    ((c>='a') && (c<='z')) ||
    ((c>='A') && (c<='Z')) ||
    ((i >= 128) && (i != 159) && (i != 189) && (i != 190) && (i != 191));
}

bool
is_iso_locase (char c) {
  int code= (int) ((unsigned char) c);
  return
    ((c>='a') && (c<='z')) ||
    ((code >= 160) && (code < 189)) ||
    (code >= 224);
}

bool
is_iso_upcase (char c) {
  int code= (int) ((unsigned char) c);
  return
    ((c>='A') && (c<='Z')) ||
    ((code >= 128) && (code < 159)) ||
    ((code >= 192) && (code < 224));
}

/******************************************************************************
* Tests for strings
******************************************************************************/

bool
is_alpha (string s) {
  int i;
  if (N(s)==0) return false;
  for (i=0; i<N(s); i++)
    if (!is_alpha (s[i])) return false;
  return true;
}

bool
is_locase_alpha (string s) {
  int i;
  if (N(s)==0) return false;
  for (i=0; i<N(s); i++)
    if (s[i]<'a' || s[i]>'z') return false;
  return true;
}

bool
is_iso_alpha (string s) {
  int i;
  if (N(s)==0) return false;
  for (i=0; i<N(s); i++)
    if (!is_iso_alpha (s[i])) return false;
  return true;
}

bool
is_numeric (string s) {
  int i;
  if (N(s)==0) return false;
  for (i=0; i<N(s); i++)
    if (!is_numeric (s[i])) return false;
  return true;
}

/******************************************************************************
* Changing cases
******************************************************************************/

char
upcase (char c) {
  if (is_iso_locase (c))
    return (char) (((int) ((unsigned char) c)) - 32);
  else return c;
}

char
locase (char c) {
  if (is_iso_upcase (c))
    return (char) (((int) ((unsigned char) c)) + 32);
  else return c;
}

char
closing_delimiter (char c) {
  if (c == '{') return '}';
  if (c == '(') return ')';
  if (c == '[') return ']';
  return c;
}

string
upcase_first (string s) {
  if ((N(s)==0) || (!is_iso_locase (s[0]))) return s;
  return string ((char) (((int) ((unsigned char) s[0]))-32)) * s (1, N(s));
}

string
locase_first (string s) {
  if ((N(s)==0) || (!is_iso_upcase (s[0]))) return s;
  return string ((char) (((int) ((unsigned char) s[0]))+32)) * s (1, N(s));
}

string
upcase_all (string s) {
  int i;
  string r (N(s));
  for (i=0; i<N(s); i++)
    if (!is_iso_locase (s[i])) r[i]= s[i];
    else r[i]= (char) (((int) ((unsigned char) s[i]))-32);
  return r;
}

string
locase_all (string s) {
  int i;
  string r (N(s));
  for (i=0; i<N(s); i++)
    if (!is_iso_upcase (s[i])) r[i]= s[i];
    else r[i]= (char) (((int) ((unsigned char) s[i]))+32);
  return r;
}

/******************************************************************************
* Inserting or removing a character into a string as a set of characters
******************************************************************************/

string
string_union (string s1, string s2) {
  return string_minus (s1, s2) * s2;
}

string
string_minus (string s1, string s2) {
  string r;
  int i1, n1= N(s1), i2, n2= N(s2);
  for (i1=0; i1<n1; i1++) {
    for (i2=0; i2<n2; i2++)
      if (s1[i1] == s2[i2]) break;
    if (i2==n2) r << s1[i1];
  }
  return r;
}

/******************************************************************************
* Spanish in relation with ispell
******************************************************************************/

string
ispanish_to_spanish (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if ((s[i] == '\'') && ((i+1)<n)) {
      switch (s[i+1]) {
      case 'A': r << '\301'; break;
      case 'E': r << '\311'; break;
      case 'I': r << '\315'; break;
      case 'N': r << '\321'; break;
      case 'O': r << '\323'; break;
      case 'U': r << '\332'; break;
      case 'Y': r << '\335'; break;
      case 'a': r << '\341'; break;
      case 'e': r << '\351'; break;
      case 'i': r << '\355'; break;
      case 'n': r << '\361'; break;
      case 'o': r << '\363'; break;
      case 'u': r << '\372'; break;
      case 'y': r << '\375'; break;
      default : r << '\'' << s[i+1];
      }
      i++;
    }
    else r << s[i];
  return r;
}

string
spanish_to_ispanish (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\301': r << "'A"; break;
    case '\311': r << "'E"; break;
    case '\315': r << "'I"; break;
    case '\321': r << "'N"; break;
    case '\323': r << "'O"; break;
    case '\332': r << "'U"; break;
    case '\335': r << "'Y"; break;
    case '\341': r << "'a"; break;
    case '\351': r << "'e"; break;
    case '\355': r << "'i"; break;
    case '\361': r << "'n"; break;
    case '\363': r << "'o"; break;
    case '\372': r << "'u"; break;
    case '\375': r << "'y"; break;
    default : r << s[i];
    }
  return r;
}

string
igerman_to_german (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\337') r << '\377';
    else r << s[i];
  return r;
}

string
german_to_igerman (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\377') r << '\337';
    else r << s[i];
  return r;
}

/******************************************************************************
* Iso latin 2 encoding for polish and czech
******************************************************************************/

static string il2_to_cork_string=
  "\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237 \20\212 \211\221\237¨\222\223\224\231‐\232\233 \241˛\252´\251\261ˇ¸\262\263\264\271˝\272\273\217\301\302\200\304\210\202\307\203\311\206\313\205\315\316\204\320\213\214\323\324\216\326.\220\227\332\226\334\335\225\377\257\341\342\240\344\250\242\347\243\351\246\353\245\355\356\244\236\253\254\363\364\256\366/\260\267\372\266\374\375\265 ";
static string cork_to_il2_string=
  "\303\241\306\310\317\314\312G\305\245\243\321\322 \325\300\330\246\251\252\253\336\333\331Y\254\256\257II\360\247\343\261\346\350\357\354\352g\345\265\263\361\362 \365\340\370\266\271\272\273\376\373\371y\274\276\277i!?LA\301\302A\304AA\307E\311E\313I\315\316I\320NO\323\324O\326OOU\332U\334\335 Sa\341\342a\344aa\347e\351e\353i\355\356i\360no\363\364o\366oou\372u\374\375 \337";

static char
il2_to_cork (char c) {
  int i= (int) ((unsigned char) c);
  if (i<128) return c;
  return il2_to_cork_string [i-128];
}

static char
cork_to_il2 (char c) {
  int i= (int) ((unsigned char) c);
  if (i<128) return c;
  return cork_to_il2_string [i-128];
}

string
il2_to_cork (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= il2_to_cork (s[i]);
  return r;
}

string
cork_to_il2 (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= cork_to_il2 (s[i]);
  return r;
}

/******************************************************************************
* Koi8 encoding for russian
******************************************************************************/

static string koi8_to_iso_string=
  "\341\342\367\347\344\345\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\377\371\370\374\340\361\301\302\327\307\304\305\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\337\331\330\334\300\321";
static string iso_to_koi8_string=
  "\376\340\341\366\344\345\364\343\365\350\351\352\353\354\355\356\357\377\360\361\362\363\346\342\374\373\347\370\375\371\367\372\336\300\301\326\304\305\324\303\325\310\311\312\313\314\315\316\317\337\320\321\322\323\306\302\334\333\307\330\335\331\327\332";

static char
koi8_to_iso (char c, bool ukrainian) {
  int i= (int) ((unsigned char) c);
  if (i==156) return '\263';
  if (i==188) return '\243';
  if (ukrainian)
  {
     switch(c)
     {
         case 'I':return '\266';
         case '\210':return '\267';
         case '\231':return '\264';
         case '\200':return '\275';
         case 'i':return '\246';
         case '\250':return '\247';
         case '\271':return '\244';
         case '\240':return '\255';
     }
  }
  if (i<192) return c;
  return koi8_to_iso_string [i-192];
}

static char
iso_to_koi8 (char c, bool ukrainian) {
  int i= (int) ((unsigned char) c);
  if (c=='\263') return (char) 156;
  if (c=='\243') return (char) 188;
  if (ukrainian)
  {
     switch(c)
     {
         case '\266':return 'I';
         case '\267':return '\210';
         case '\264':return '\231';
         case '\275':return '\200';
         case '\246':return 'i';
         case '\247':return '\250';
         case '\244':return '\271';
         case '\255':return '\240';
     }
  }
  if (i<192) return c;
  return iso_to_koi8_string [i-192];
}

string
koi8_to_iso (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= koi8_to_iso (s[i], false);
  return r;
}

string
iso_to_koi8 (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= iso_to_koi8 (s[i], false);
  return r;
}

string
koi8uk_to_iso (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= koi8_to_iso (s[i], true);
  return r;
}

string
iso_to_koi8uk (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++)
    r[i]= iso_to_koi8 (s[i], true);
  return r;
}

/******************************************************************************
* Convert TS1 defined symbols to universal encoding
******************************************************************************/

tree
convert_OTS1_symbols_to_universal_encoding (tree t) {
  if (is_atomic (t)) return t;
  if (N(t) == 0) {
    static tree symbols (CONCAT);
    if (N(symbols) == 0)
      symbols << "cent" << "copyright" << "currency" << "yen" << "twosuperior"
        << "threesuperior" << "onesuperior" << "mu" << "onequarter"
        << "onehalf" << "threequarters" << "trademark";
    tree l= tree (as_string (L(t)));
    if (contains (l, A(symbols)))
      return "<" * as_string (L(t)) * ">";
    else if (l == "degreesign")
      return "<degree>";
    else if (l == "copyleft")
      return "<copyright>"; // Copyleft is nor defined in TeXmacs universal
                            // encoding, neither in utf8, neither buildable
                            // with TeXmacs primitive construction.
    else if (l == "registered")
      return "<circledR>";
    else if (l == "paragraphsign")
      return "<paragraph>";
    else if (l == "euro")
      return "<#20AC>";
    else
      return t;

  }
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= convert_OTS1_symbols_to_universal_encoding (t[i]);
  return r;
}

bool
contains_unicode_char (string s) {
  int i= 0, n= N(s);
  while (i+1<n) {
    if (s[i] == '<' && s[i+1] == '#') return true;
    tm_char_forwards (s, i);
  }
  return false;
}

/******************************************************************************
* Roman and alpha numbers
******************************************************************************/

static string ones[10]    = {"",  "i",  "ii",  "iii",  "iv",
                             "v", "vi", "vii", "viii", "ix"};
static string tens[10]    = {"",  "x",  "xx",  "xxx",  "xl",
                             "l", "lx", "lxx", "lxxx", "xc"};
static string hundreds[10]= {"",  "c",  "cc",  "ccc",  "cd",
                             "d", "dc", "dcc", "dccc", "cm"};
static string thousands[4]= {"", "m", "mm", "mmm"};

string
roman_nr (int32_t nr) {
  if (nr < 0) return "-" * roman_nr (-nr);
  if (nr == 0) return "o";
  if (nr > 3999) return "?";
  return thousands[(nr / 1000) % 10] *
         hundreds [(nr / 100) % 10] *
         tens     [(nr / 10) % 10] *
         ones     [nr % 10];
}

string
Roman_nr (int nr) {
  return upcase_all (roman_nr (nr));
}

string
alpha_nr (int nr) {
  if (nr<0) return "-" * alpha_nr (-nr);
  if (nr==0) return "0";
  if (nr<=26) return string ((char) (((int) 'a')+ nr-1));
  return alpha_nr ((nr-1)/26) * alpha_nr (((nr-1)%26)+1);
}

string
Alpha_nr (int nr) {
  return upcase_all (alpha_nr (nr));
}

string
fnsymbol_nr (int nr) {
  if (nr<0) nr= -nr;
  string sym, r;
  int i, m= (nr-1)%6, n= ((nr-1)/6)+1;
  switch (m) {
    case 0: sym= "<asterisk>";   break;
    case 1: sym= "<dag>";        break;
    case 2: sym= "<ddag>";       break;
    case 3: sym= "<paragraph>";  break;
    case 4: sym= "<endofline>";  break;
    case 5: sym= "||";           break;
  }
  for (i=0; i<n; i++) r << sym;
  return r;
}

/******************************************************************************
* Conversions to and from hexadecimal
******************************************************************************/

static const char* hex_string= "0123456789ABCDEF";

string
as_hexadecimal (int i) {
  if (i<0) return "-" * as_hexadecimal (-i);
  if (i<16) return hex_string [i & 15];
  return as_hexadecimal (i >> 4) * hex_string [i & 15];
}

string
as_hexadecimal (pointer ptr) {
  intptr_t i= (intptr_t) ptr;
  if (i<0) return "-" * as_hexadecimal (-i);
  if (i<16) return hex_string [i & 15];
  return as_hexadecimal (i >> 4) * hex_string [i & 15];
}

string
as_hexadecimal (int i, int len) {
  if (len==1) return hex_string [i & 15];
  else return as_hexadecimal (i >> 4, len-1) * hex_string [i & 15];
}

int
from_hexadecimal (string s) {
  int i, n= N(s), res= 0;
  if ((n>0) && (s[0]=='-'))
    return -from_hexadecimal (s (1, n));
  for (i=0; i<n; i++) {
    res= res << 4;
    if (is_digit (s[i])) res += (int) (s[i] - '0');
    if ((s[i] >= 'A') && (s[i] <= 'F')) res += (int) (s[i] + 10 - 'A');
    if ((s[i] >= 'a') && (s[i] <= 'f')) res += (int) (s[i] + 10 - 'a');
  }
  return res;
}

/******************************************************************************
* Routines for the TeXmacs encoding
******************************************************************************/

string
tm_encode (string s) {
  // verbatim to TeXmacs encoding
  int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') r << "<less>";
    else if (s[i]=='>') r << "<gtr>";
    else r << s[i];
  }
  return r;
}

string
tm_decode (string s) {
  // TeXmacs encoding to verbatim
  int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      int j;
      for (j=i+1; j<N(s); j++)
        if (s[j]=='>') break;
      if (j<N(s)) j++;
      if (s(i,j) == "<less>") r << "<";
      else if (s(i,j) == "<gtr>") r << ">";
      else if (i+7==j && s[i+1]=='#' && s[j-1]=='>')
        r << s(i, j);
      i=j-1;
      if (s[i]!='>') return r;
    }
    else if (s[i]!='>') r << s[i];
  }
  return r;
}

string
tm_var_encode (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    if (s[i]=='<') {
      if (i+1 < n && s[i+1] == '#') {
        while (i<n && s[i] != '>') r << s[i++];
        if (i<n) r << s[i];
      }
      else r << "<less>";
    }
    else if (s[i]=='>') r << "<gtr>";
    else r << s[i];
  }
  return r;
}

string
tm_correct (string s) {
  int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      bool flag= true;
      int j, k;
      for (j=i+1; j<N(s); j++)
        if (s[j]=='>') break;
      if (j==N(s)) return r;
      for (k=i+1; k<j; k++)
        if (s[k]=='<') flag= false;
      if (flag) r << s(i,j+1);
      i=j;
    }
    else if (s[i]!='>') r << s[i];
  }
  return r;
}

void
tm_char_forwards (string s, int& pos) {
  ASSERT (pos >= 0 && pos <= N(s), "out of range");
  int n= N(s);
  if (pos == n);
  else if (s[pos] != '<') pos++;
  else {
    while (pos<n && s[pos] != '>') pos++;
    if (pos<n) pos++;
  }
}

void
tm_char_backwards (string s, int& pos) {
  ASSERT (pos >= 0 && pos <= N(s), "out of range");
  if (pos == 0);
  else if (s[pos-1] != '>') pos--;
  else {
    while (pos>0 && s[pos-1] != '<') pos--;
    if (pos>0) pos--;
  }
}

int
tm_char_next (string s, int pos) {
  tm_char_forwards (s, pos);
  return pos;
}

int
tm_char_previous (string s, int pos) {
  tm_char_backwards (s, pos);
  return pos;
}

string
tm_forward_access (string s, int k) {
  int pos= 0;
  for (int i=0; i<k; i++)
    tm_char_forwards (s, pos);
  int start= pos;
  tm_char_forwards (s, pos);
  return s (start, pos);
}

string
tm_backward_access (string s, int k) {
  int pos= N(s);
  for (int i=0; i<k; i++)
    tm_char_backwards (s, pos);
  int end= pos;
  tm_char_backwards (s, pos);
  return s (pos, end);
}

int
tm_string_length (string s) {
  int i= 0, pos= 0;
  while (pos < N(s)) {
    tm_char_forwards (s, pos);
    i++;
  }
  return i;
}

array<string>
tm_tokenize (string s) {
  array<string> r;
  int pos= 0;
  while (pos < N(s)) {
    int start= pos;
    tm_char_forwards (s, pos);
    r << s (start, pos);
  }
  return r;
}

string
tm_recompose (array<string> a) {
  string r;
  for (int i=0; i<N(a); i++)
    r << a[i];
  return r;
}

int
tm_search_forwards (string s, int pos, string in) {
  int k= N(s), n= N(in);
  if (k == 0) return pos;
  char c= s[0];
  while (pos+k <= n) {
    if (in[pos] == c && test (in, pos, s)) return pos;
    tm_char_forwards (in, pos);
  }
  return -1;
}

int
tm_search_backwards (string s, int pos, string in) {
  while (pos >= 0) {
    if (test (in, pos, s)) return pos;
    tm_char_backwards (in, pos);
  }
  return -1;
}

static array<string>
tm_string_split_between_words (string s) {
  int i= 0, j= -1, n= N(s);
  char status= 'o';
  array<string> r;
  while (i < n && j < n/2) {
    char c= s[i];
    if      (is_numeric (c)   && status == 'c');
    else if (is_iso_alpha (c) && status == 'a');
    else {
      if      (is_numeric   (c)) status= 'c';
      else if (is_iso_alpha (c)) status= 'a';
      else status= 'x';
      j= i;
    }
    tm_char_forwards (s, i);
  }
  if (j > 0 && j < n)
    r << s(0, j) << s(j, n);
  else
    r << s;
  return r;
}

static array<string>
tm_string_split_at_spaces (string s) {
  int i= 0, j= 0, n= N(s);
  array<string> r;
  while (i>=0 && j < n/2) {
    i= tm_search_forwards (" ", i, s);
    if (i == -1) break;
    j= i++;
  }
  if (j < 1 || j >= n)
    r << s;
  else if (j == n-1)
    r << s(0, j) << s(j, n);
  else
    r << s(0, j) << s(j, j+1) << s(j+1, n);
  return r;
}

array<string>
tm_string_split (string s) {
  array<string> r;
  r= tm_string_split_at_spaces (s);
  if (N(r) > 1) return r;
  r= tm_string_split_between_words (s);
  if (N(r) > 1) return r;
  /* else split anywhere */
  int i= 0, n= N(s);
  while (i < n/2)
    tm_char_forwards (s, i);
  return array<string> (s(0, i), s(i, n));
}

/******************************************************************************
* Quoting
******************************************************************************/

string
scm_quote (string s) {
  // R5RS compliant external string representation.
  int i, n= N(s);
  string r;
  r << '"';
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\"':
    case '\\':
      r << '\\' << s[i];
      break;
    default:
      r << s[i];
    }
  r << '"';
  return r;
}

string
scm_unquote (string s) {
  if (is_quoted (s)) {
    int i, n= N(s);
    string r;
    for (i=1; i<n-1; i++)
      if (s[i] == '\\' && (s[i+1] == '\\' || (s[i+1] == '\"' && i+2!=n)))
        r << s[++i];
      else r << s[i];
    return r;
  }
  else return s;
}

string
raw_quote (string s) {
  // Mark the label of a STRING tree as representing a string and not a symbol.
  return "\"" * s * "\"";
}

string
raw_unquote (string s) {
  // Get the string value of a STRING tree label representing a string.
  if (is_quoted (s))
    return s (1, N(s)-1);
  else return s;
}

/******************************************************************************
* Handling escape characters
******************************************************************************/

string
escape_sh (string s) {
#ifdef OS_MINGW
  return raw_quote (s);
#else
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '(':
    case ')':
    case '<':
    case '>':
    case '?':
    case '&':
    case '$':
    case '`':
    case '\"':
    case '\\':
    case ' ':
      r << '\\' << s[i];
      break;
    case '\n':
      r << "\\n";
      break;
    default:
      r << s[i];
    }
  return r;
#endif
}

string
escape_generic (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    if ((s[i] == '\2') || (s[i] == '\5') || (s[i] == '\33')) r << '\33';
    r << s[i];
  }
  return r;
}

string
escape_verbatim (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if ((c == '\n') || (c == '\t')) r << ' ';
    else if (((int) c) >= 32) r << s[i];
  }
  return r;
}

string
escape_spaces (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == ' ') r << '\\';
    r << c;
  }
  return r;
}

string
dos_to_better (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\015');
    else r << s[i];
  return r;
}

string
unescape_guile (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    if (s[i] == '\\') {
      if (i+1 < n && s[i+1] == '\\') {
        r << "\\\\\\\\";
        i+=1;
      }
      else if (i+3 < n && s[i+1] == 'x'
          && is_hex_digit (s[i+2]) && is_hex_digit (s[i+3])) {
        string e= s(i+2, i+4);
        r << (unsigned char) from_hexadecimal (e);
        i+=4;
        //NOTE: format is "\xHH;" in S7
        //In Guile it is  "\xHH"
      }
      else
        r << s[i];
    }
    else
      r << s[i];
  }
  return r;
}

/******************************************************************************
* Reading input from a string
******************************************************************************/

bool
test (string s, int i, const char* test) {
  int n= N(s), j=0;
  while (test[j]!='\0') {
    if (i>=n) return false;
    if (s[i]!=test[j]) return false;
    i++; j++;
  }
  return true;
}

bool
test (string s, int i, string test) {
  int n= N(s), m= N(test), j=0;
  while (j<m) {
    if (i>=n) return false;
    if (s[i]!=test[j]) return false;
    i++; j++;
  }
  return true;
}

bool
starts (string s, const char* what) {
  return test (s, 0, what);
}

bool
starts (string s, const string what) {
  return test (s, 0, what);
}

bool
ends (string s, const char* what) {
  string r (what);
  if (N(r) > N(s)) return false;
  return s (N(s)-N(r), N(s)) == r;
}

bool
ends (string s, const string r) {
  if (N(r) > N(s)) return false;
  return s (N(s)-N(r), N(s)) == r;
}

bool
read (string s, int& i, const char* test) {
  int n= N(s), j=0, k=i;
  while (test[j]!='\0') {
    if (k>=n) return false;
    if (s[k]!=test[j]) return false;
    j++; k++;
  }
  i=k;
  return true;
}

bool
read (string s, string test) {
  int i = 0;
  return read (s, i, test);
}

bool
read (string s, int& i, string test) {
  int n= N(s), m= N(test), j=0, k=i;
  while (j<m) {
    if (k>=n) return false;
    if (s[k]!=test[j]) return false;
    j++; k++;
  }
  i=k;
  return true;
}

bool
read_line (string s, int& i, string& result) {
  int start= i;
  for (; i<N(s); i++) {
    if (s[i]=='\n') {
      result= s(start,i++);
      return true;
    }
  }
  result= s(start,i);
  return false;
}

bool
read_int (string s, int& i, int& result) {
  int n= N(s), start= i;
  result= 0;
  if (i==n) return false;
  if (s[i]=='-') {
    if (i+1==n) return false;
    if (!is_digit (s[i+1])) return false;
    i++;
  }
  else if (!is_digit (s[i])) return false;
  while ((i<n) && is_digit (s[i])) i++;
  result= as_int (s(start,i));
  return true;
}

bool
read_double (string s, int& i, double& result) {
  int n= N(s), start= i;
  result= 0.0;
  if (i==n) return false;
  if (s[i]=='-') {
    if (i+1==n) return false;
    if (!is_numeric (s[i+1])) return false;
    i++;
  }
  else if (!is_numeric (s[i])) return false;
  while ((i<n) && is_digit (s[i])) i++;
  if ((i<n) && (s[i]=='.')) i++;
  while ((i<n) && is_digit (s[i])) i++;
  if ((i<n) && ((s[i]=='e') || (s[i]=='E'))) {
    i++;
    if ((i<n) && (s[i]=='-')) i++;
    if ((i==n) || (!is_digit (s[i]))) { i=start; return false; }
    while ((i<n) && is_digit (s[i])) i++;
  }
  result= as_double (s(start,i));
  return true;
}

bool
read_word (string s, int& i, string& result) {
  int opos= i;
  while (i<N(s) && is_alpha (s[i])) {
    i++;
  }
  result= s(opos, i);
  return i>opos;
}

bool
is_whitespace (string s) {
  for (int i=0; i<N(s); i++)
    if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n') return false;
  return true;
}

void
skip_spaces (string s, int& i) {
  int n=N(s);
  while ((i<n) && ((s[i]==' ') || (s[i]=='\t'))) i++;
}

void
skip_whitespace (string s, int& i) {
  int n=N(s);
  while ((i<n) && ((s[i]==' ') || (s[i]=='\t') || (s[i]=='\n'))) i++;
}

void
skip_line (string s, int& i) {
  int n=N(s);
  while ((i<n) && (s[i]!='\n')) i++;
  if (i<n) i++;
}

void
skip_symbol (string s, int& i) {
  int n=N(s);
  if (i<n) {
    if (s[i]=='<') {
      for (i++; i<n; i++)
        if (s[i-1]=='>') break;
    }
    else i++;
  }
}

string
convert_tabs_to_spaces (string s, int tw) {
  int i= 0, ts= 0, n= N(s);
  string r= "";
  while (i<n) {
    if (s[i] == '\t') {
      r << string (' ', tw - ((i - ts) % tw));
      ts= i+1;
    }
    else if (s[i] == '\n') {
      ts= i+1;
      r << s[i];
    }
    else
      r << s[i];
    i++;
  }
  return r;
}

string
downgrade_math_letters (string s) {
  string r= "";
  for (int i=0; i<N(s); ) {
    int start= i;
    tm_char_forwards (s, i);
    if (i == start + 1) r << s[start];
    else {
      string ss= s (start, i);
      if (starts (ss, "<b-")) ss= "<" * ss (3, N(ss));
      if (starts (ss, "<up-")) ss= "<" * ss (4, N(ss));
      if (starts (ss, "<cal-")) ss= "<" * ss (5, N(ss));
      if (starts (ss, "<bbb-")) ss= "<" * ss (5, N(ss));
      if (starts (ss, "<frak-")) ss= "<" * ss (6, N(ss));
      if (N(ss) == 3) ss= ss (1, 2);
      r << ss;
    }
  }
  return r;
}

/******************************************************************************
* Parsing binary data
******************************************************************************/

void
parse (string s, int& pos, QI& ret) {
  ret= (QI) s[pos++];
}

void
parse (string s, int& pos, QN& ret) {
  ret= (QN) s[pos++];
}

void
parse (string s, int& pos, HI& ret) {
  QI c1= (QI) s[pos++];
  QN c2= (QN) s[pos++];
  ret= (((HI) c1)<<8)+ c2;
}

void
parse (string s, int& pos, HN& ret) {
  QN c1= (QN) s[pos++];
  QN c2= (QN) s[pos++];
  ret= (((HN) c1)<<8)+ c2;
}

void
parse (string s, int& pos, SI& ret) {
  QI c1= (QI) s[pos++];
  QN c2= (QN) s[pos++];
  QN c3= (QN) s[pos++];
  QN c4= (QN) s[pos++];
  ret= (((((((SI) c1)<<8)+ ((SI) c2))<<8)+ ((SI) c3))<<8)+ c4;
}

void
parse (string s, int& pos, SI*& a, int len) {
  int i;
  a= tm_new_array<int> (len);
  for (i=0; i<len; i++) parse (s, pos, a[i]);
}




/******************************************************************************
* Searching, replacing and pattern matching
******************************************************************************/

int
search_forwards (array<string> a, int pos, string in) {
  int n= N(in), na= N(a);
  while (pos <= n) {
    for (int i=0; i<na; i++)
      if (N(a[i])>0 && in[pos] == a[i][0] && test (in, pos, a[i])) return pos;
    pos++;
  }
  return -1;
}

int
search_forwards (string s, int pos, string in) {
  int k= N(s), n= N(in);
  if (k == 0) return pos;
  char c= s[0];
  while (pos+k <= n) {
    if (in[pos] == c && test (in, pos, s)) return pos;
    pos++;
  }
  return -1;
}

int
search_forwards (string s, string in) {
  return search_forwards (s, 0, in);
}

bool
occurs (string what, string in) {
  return search_forwards (what, 0, in) >= 0;
}

int
search_backwards (string s, int pos, string in) {
  while (pos >= 0) {
    if (test (in, pos, s)) return pos;
    pos--;
  }
  return -1;
}

int
search_backwards (string s, string in) {
  return search_backwards (s, N(in)-N(s), in);
}

int
count_occurrences (string s, string in) {
  int count= 0;
  int i=0, next, n= N(in);
  while (i<n) {
    next= search_forwards (s, i, in);
    if (next == -1) break;
    count++;
    i= next+1;
  }
  return count;
}

int
overlapping (string s1, string s2) {
  // return the longuest string being suffix of s1 and prefix of s2
  int i= min (N(s1), N(s2)), n= N(s1);
  while (i>0) {
    if (s1(n-i, n) == s2(0, i))
      return i;
    i--;
  }
  return 0;
}

string
replace (string s, string what, string by) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; )
    if (test (s, i, what)) {
      r << by;
      i += N(what);
    }
    else {
      r << s[i];
      i++;
    }
  return r;
}

static bool
match_wildcard (string s, int spos, string w, int wpos) {
  if (wpos == N(w)) return spos == N(s);
  if (w[wpos] != '*')
    return (spos < N(s)) && (s[spos] == w[wpos]) &&
      match_wildcard (s, spos+1, w, wpos+1);
  while ((wpos<N(w)) && (w[wpos]=='*')) wpos++;
  while (spos <= N(s)) {
    if (match_wildcard (s, spos, w, wpos)) return true;
    spos++;
  }
  return false;
}

bool
match_wildcard (string s, string w) {
  return match_wildcard (s, 0, w, 0);
}

int
find_non_alpha (string s, int pos, bool forward) {
  if (forward) {
    for (; pos<N(s); pos++)
      if (!is_alpha (s[pos])) return pos;
  }
  else {
    for (; pos>0; pos--)
      if (!is_alpha (s[pos-1])) return pos-1;
  }
  return -1;
}

array<string>
tokenize (string s, string sep) {
  int start=0;
  array<string> a;
  for (int i=0; i<N(s); )
    if (test (s, i, sep)) {
      a << s (start, i);
      i += N(sep);
      start= i;
    }
    else i++;
  a << s(start, N(s));
  return a;
}

string
recompose (array<string> a, string sep) {
  string r;
  for (int i=0; i<N(a); i++) {
    if (i != 0) r << sep;
    r << a[i];
  }
  return r;
}

string
trim_spaces_left (string s) {
  int start;
  for (start=0; start<N(s) && is_space (s[start]); start++) ;
  return s (start, N(s));
}

string
trim_spaces_right (string s) {
  int end;
  for (end=N(s)-1; end >= 0 && is_space (s[end]); end--) ;
  return s (0, end+1);
}

string
trim_spaces (string s) {
  return trim_spaces_left (trim_spaces_right (s));
}

array<string>
trim_spaces (array<string> a) {
  array<string> b (N(a));
  for (int i=0; i<N(a); i++)
    b[i]= trim_spaces (a[i]);
  return b;
}

tree
trim_spaces_right (tree t) {
  if (is_atomic (t)) return trim_spaces_right (as_string (t));
  else if (is_concat (t)) {
    tree l;
    int end;
    for (end= N(t)-1; end >= 0; end--) {
      l= trim_spaces_right (t[end]);
      if (l != "") break;
    }
    tree r= tree (L(t));
    for (int i=0; i<end; i++) r << t[i];
    if (end >= 0) r << l;
    if (N(r) == 0) return "";
    else if (N(r) == 1) return r[0];
    else return r;
  }
  else return t;
}

tree
trim_spaces_left (tree t) {
  if (is_atomic (t)) return trim_spaces_left (as_string (t));
  else if (is_concat (t)) {
    tree l;
    int start;
    for (start= 0; start < N(t); start++) {
      l= trim_spaces_left (t[start]);
      if (l != "") break;
    }
    tree r= tree (L(t));
    if (start < N(t)) r << l;
    for (int i=start+1; i<N(t); i++) r << t[i];
    if (N(r) == 0) return "";
    else if (N(r) == 1) return r[0];
    else return r;
  }
  else return t;
}

tree
trim_spaces (tree t) {
  return trim_spaces_left (trim_spaces_right (t));
}

/******************************************************************************
 * Program bracket matching
 ******************************************************************************/

extern tree the_et;
enum dir_t { BACKWARD = -1, FORWARD = 1 }; // don't change the values

  // FIXME: UGLY and won't handle some things
static bool
find_bracket_valid (tree t, int pos) {
  if (pos < 0 || pos >= N(t))
    return false;
  if (L(t) == STRING || L(t) == DOCUMENT)
    return true;
  if (L(t) == CONCAT)
    return L(t[pos]) == STRING || L(t[pos]) == CONCAT || L(t[pos]) == WITH;
  if (L(t) == WITH)
    return pos == N(t)-1 &&
           (L(t[pos]) == STRING || L(t[pos]) == CONCAT || L(t[pos]) == WITH ||
            L(t[pos]) == DOCUMENT);
  else
    return false;
}

static path
find_bracket_sub (tree t, dir_t dir, const string& lbr, const string& rbr,
                  int cnt, int pos) {
  if (pos >= 0 && pos < N(t)) {
    if (is_atomic (t)) {
      void (*next) (string, int&) = (dir == FORWARD) ? tm_char_forwards
                                                      : tm_char_backwards;
      while (true) {
        if (test (t->label, pos, lbr))
          --cnt;
        else if (test (t->label, pos, rbr))
          ++cnt;
        if (cnt == 0)
          return reverse (path (min (N(t), pos), obtain_ip (t)));
        int prev = pos;
        next (t->label, pos);
        if (prev == pos)
          break;
      }
    } else if (find_bracket_valid (t, pos)) { // traverse child: t[pos]
      int npos = (dir == FORWARD) ? 0 : N(t[pos])-1;
      return find_bracket_sub (t[pos], dir, lbr, rbr, cnt, npos);
    } else {  // traverse siblings
      int npos = pos + (int)dir;
      if (npos >= 0 && npos < N(t))
        return find_bracket_sub (t, dir, lbr, rbr, cnt, npos);
    }
  }
  path ip = obtain_ip (t);
  if (is_nil (ip) || last_item (ip) < 0) return path();
  const tree& pt = subtree (the_et, reverse (ip->next));
  int npos = ip->item + (int)dir;
  return find_bracket_sub (pt, dir, lbr, rbr, cnt, npos);
}

path
find_left_bracket (path p, const string& lbr, const string& rbr) {
  if (N(lbr) == 0 || N(rbr) == 0 || is_nil (p)) return path();
  const tree& pt = subtree (the_et, path_up (p));
  int pos = reverse(p)->item;
  string s = as_string (pt);
  if (pos < 0 || pos > N(s)) return path();
  int cnt = test (s, pos, rbr) ? 0 : 1;
  return find_bracket_sub (pt, BACKWARD, lbr, rbr, cnt, pos);
}

path
find_right_bracket (path p, const string& lbr, const string& rbr) {
  if (N(lbr) == 0 || N(rbr) == 0 || is_nil (p)) return path();
  const tree& pt = subtree (the_et, path_up (p));
  int pos = reverse(p)->item;
  string s = as_string (pt);
  if (pos < 0 || pos > N(s)) return path();
  int cnt = test (s, pos, lbr) ? 0 : -1;
  return find_bracket_sub (pt, FORWARD, lbr, rbr, cnt, pos);
}

/******************************************************************************
* Computations with completions
******************************************************************************/

array<string>
as_completions (hashset<string> h) {
  tree t= (tree) h;
  int i, n= N(t);
  array<string> a (n);
  for (i=0; i<n; i++) a[i]= t[i]->label;
  merge_sort (a);
  return a;
}

/*
static void
close_completions (hashset<string>& h) {
  array<string> a= as_completions (h);
  int i, j, n= N(a);
  for (i=1; i<n; i++) {
    for (j=0; j < min (N(a[i-1]), N(a[i])); j++)
      if (a[i-1][j] != a[i][j]) break;
    if (j < min (N(a[i-1]), N(a[i])))
      h->insert (a[i](0,j));
  }
}

array<string>
close_completions (array<string> a) {
  int i, n= N(a);
  hashset<string> h;
  for (i=0; i<n; i++) h->insert (a[i]);
  close_completions (h);
  return as_completions (h);
}
*/

array<string>
close_completions (array<string> a) {
  if (N(a) == 0) return a;
  merge_sort (a);
  int i, j, n= N(a), l= N(a[0]);
  for (i=1; i<n; i++) {
    for (j=0; j<l && j<N(a[i]); j++)
      if (a[i-1][j] != a[i][j]) break;
    l= j;
  }
  array<string> r;
  r << a[0] (0, l);
  for (i=0; i<n; i++)
    if (a[i] != r[N(r)-1])
      r << a[i];
  return r;
}

array<string>
strip_completions (array<string> a, string prefix) {
  int i, n= N(a);
  array<string> b;
  for (i=0; i<n; i++)
    if (starts (a[i], prefix))
      b << a[i] (N(prefix), N(a[i]));
  return b;
}

/******************************************************************************
* Differences between two strings
******************************************************************************/

static int
find_longest (string s1, string s2, int& c1, int& c2) {
  int n1= N(s1), n2= N(s2), bc= 0, bl= 0, br= 0;
  for (c2=0; c2<n2; c2++)
    if (s1[c1] == s2[c2]) {
      int l=0, r=0;
      while (c1+r<n1 && c2+r<n2 && s1[c1+r] == s2[c2+r]) r++;
      while (l<c1 && l<c2 && s1[c1-l-1] == s2[c2-l-1]) l++;
      if (l+r > bl+br) {
        bc= c2;
        bl= l;
        br= r;
      }
    }
  if (bl + br > 0) {
    c1= c1 - bl;
    c2= bc - bl;
  }
  return bl + br;
}

static void
find_common (string s1, string s2, int& c1, int& c2) {
  int best_len= 0;
  c1= c2= 0;
  int n1= N(s1), n2= N(s2);
  if (n1 == 0 || n2 == 0) return;
  int t= min (min (n1, n2), 6);
  for (int k=1; k<t; k++) {
    int a1= (k*n1)/t, a2= (k*n2)/t;
    int len= find_longest (s1, s2, a1, a2);
    if (len > best_len) { best_len= len; c1= a1; c2= a2; }
  }
}

array<int>
differences (string s1, string s2) {
  int n1= N(s1), n2= N(s2);
  int i1= 0, i2= 0, j1= n1, j2= n2;
  while (i1<j1 && i2<j2 && s1[i1] == s2[i2]) { i1++; i2++; }
  while (i1<j1 && i2<j2 && s1[j1-1] == s2[j2-1]) { j1--; j2--; }
  if (i1 == i2 && j1 == j2) return array<int> ();
  if (i1 > 0 || i2 > 0 || j1 < n1 || j2 < n2) {
    array<int> r= differences (s1 (i1, j1), s2 (i2, j2));
    for (int k=0; k<N(r); k+=4) {
      r[k  ] += i1;
      r[k+1] += i1;
      r[k+2] += i2;
      r[k+3] += i2;
    }
    return r;
  }
  else {
    int c1, c2;
    find_common (s1, s2, c1, c2);
    if (c1 == 0 && c2 == 0) {
      array<int> r;
      r << i1 << j1 << i2 << j2;
      return r;
    }
    else {
      array<int> r1= differences (s1 (0 , c1), s2 (0 , c2));
      array<int> r2= differences (s1 (c1, n1), s2 (c2, n2));
      for (int k=0; k<N(r2); k+=4) {
        r2[k  ] += c1;
        r2[k+1] += c1;
        r2[k+2] += c2;
        r2[k+3] += c2;
      }
      r1 << r2;
      return r1;
    }
  }
}

int
distance (string s1, string s2) {
  int d= 0;
  array<int> r= differences (s1, s2);
  for (int k=0; k<N(r); k+=4)
    d += max (r[k+1] - r[k], r[k+3] - r[k+2]);
  return d;
}

/******************************************************************************
* Parse length
******************************************************************************/

void
parse_length (string s, double& len, string& unit) {
  int start= 0;
  int i, n= N(s);
  for (i=start; i<n && !is_locase (s[i]); i++) {}
  string s1= s (start, i);
  string s2= s (i, n);
  if (is_double (s1) && (is_locase_alpha (s2) || is_empty (s2))) {
    len= as_double (s1);
    unit= s2;
  } else {
    len= 0.0;
    unit= "error";
  }
}
