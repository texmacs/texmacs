
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
#include "Scheme/object.hpp"

/******************************************************************************
* Tests for caracters
******************************************************************************/

bool
is_alpha (register char c) {
  return ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z'));
}

bool
is_iso_alpha (register char c) {
  int i= ((int) ((unsigned char) c));
  return
    ((c>='a') && (c<='z')) ||
    ((c>='A') && (c<='Z')) ||
    ((i >= 128) && (i != 159) && (i != 189) && (i != 190) && (i != 191));
}

bool
is_locase (register char c) {
  int code= (int) ((unsigned char) c);
  return
    ((c>='a') && (c<='z')) ||
    ((code >= 160) && (code < 189)) ||
    (code >= 224);
}

bool
is_upcase (register char c) {
  int code= (int) ((unsigned char) c);
  return
    ((c>='A') && (c<='Z')) ||
    ((code >= 128) && (code < 159)) ||
    ((code >= 192) && (code < 224));
}

bool
is_digit (register char c) {
  return (c>='0') && (c<='9');
}

bool
is_numeric (register char c) {
  return ((c>='0') && (c<='9')) || (c=='.');
}

bool
is_punctuation (register char c) {
  return
    (c=='.') || (c==',') || (c==':') || (c=='\'') || (c=='`') ||
    (c==';') || (c=='!') || (c=='?');
}

bool
is_space (register char c) {
  return (c == ' ') || (c == '\11') || (c == '\12') || (c == '\15');\
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
  if (is_locase (c))
    return (char) (((int) ((unsigned char) c)) - 32);
  else return c;
}

char
locase (char c) {
  if (is_upcase (c))
    return (char) (((int) ((unsigned char) c)) + 32);
  else return c;
}

string
upcase_first (string s) {
  if ((N(s)==0) || (!is_locase (s[0]))) return s;
  return string ((char) (((int) ((unsigned char) s[0]))-32)) * s (1, N(s));
}

string
locase_first (string s) {
  if ((N(s)==0) || (!is_upcase (s[0]))) return s;
  return string ((char) (((int) ((unsigned char) s[0]))+32)) * s (1, N(s));
}

string
upcase_all (string s) {
  int i;
  string r (N(s));
  for (i=0; i<N(s); i++)
    if (!is_locase (s[i])) r[i]= s[i];
    else r[i]= (char) (((int) ((unsigned char) s[i]))-32);
  return r;
}

string
locase_all (string s) {
  int i;
  string r (N(s));
  for (i=0; i<N(s); i++)
    if (!is_upcase (s[i])) r[i]= s[i];
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
      case 'A': r << 'Á'; break;
      case 'E': r << 'É'; break;
      case 'I': r << 'Í'; break;
      case 'N': r << 'Ñ'; break;
      case 'O': r << 'Ó'; break;
      case 'U': r << 'Ú'; break;
      case 'Y': r << 'Ý'; break;
      case 'a': r << 'á'; break;
      case 'e': r << 'é'; break;
      case 'i': r << 'í'; break;
      case 'n': r << 'ñ'; break;
      case 'o': r << 'ó'; break;
      case 'u': r << 'ú'; break;
      case 'y': r << 'ý'; break;
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
    case 'Á': r << "'A"; break;
    case 'É': r << "'E"; break;
    case 'Í': r << "'I"; break;
    case 'Ñ': r << "'N"; break;
    case 'Ó': r << "'O"; break;
    case 'Ú': r << "'U"; break;
    case 'Ý': r << "'Y"; break;
    case 'á': r << "'a"; break;
    case 'é': r << "'e"; break;
    case 'í': r << "'i"; break;
    case 'ñ': r << "'n"; break;
    case 'ó': r << "'o"; break;
    case 'ú': r << "'u"; break;
    case 'ý': r << "'y"; break;
    default : r << s[i];
    }
  return r;
}

string
igerman_to_german (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == 'ß') r << 'ÿ';
    else r << s[i];
  return r;
}

string
german_to_igerman (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == 'ÿ') r << 'ß';
    else r << s[i];
  return r;
}

/******************************************************************************
* Iso latin 2 encoding for polish and czech
******************************************************************************/

static string il2_to_cork_string=
  "€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ Š ‰‘Ÿ’“”™š› ¡ª©±²³´¹º»ÁÂ€Äˆ‚ÇƒÉ†Ë…ÍÎ„Ð‹ŒÓÔŽÖ.—Ú–ÜÝ•ÿ¯áâ ä¨¢ç£é¦ë¥íî¤ž«¬óô®ö/°·ú¶üýµ ";
static string cork_to_il2_string=
  "Ã¡ÆÈÏÌÊGÅ¥£ÑÒ ÕÀØ¦©ª«ÞÛÙY¬®¯IIð§ã±æèïìêgåµ³ñò õàø¶¹º»þûùy¼¾¿i!?LAÁÂAÄAAÇEÉEËIÍÎIÐNOÓÔOÖOOUÚUÜÝ Saáâaäaaçeéeëiíîiðnoóôoöoouúuüý ß";

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
  "áâ÷çäåöúéêëìíîïðòóôõæèãþûýÿùøüàñÁÂ×ÇÄÅÖÚÉÊËÌÍÎÏÐÒÓÔÕÆÈÃÞÛÝßÙØÜÀÑ";
static string iso_to_koi8_string=
  "þàáöäåôãõèéêëìíîïÿðñòóæâüûçøýù÷úÞÀÁÖÄÅÔÃÕÈÉÊËÌÍÎÏßÐÑÒÓÆÂÜÛÇØÝÙ×Ú";

static char
koi8_to_iso (char c, bool ukrainian) {
  int i= (int) ((unsigned char) c);
  if (i==156) return '³';
  if (i==188) return '£';
  if (ukrainian)
  {
     switch(c)
     {
         case 'I':return '¶';
         case 'ˆ':return '·';
         case '™':return '´';
         case '€':return '½';
         case 'i':return '¦';
         case '¨':return '§';
         case '¹':return '¤';
         case ' ':return '­';
     }
  }
  if (i<192) return c;
  return koi8_to_iso_string [i-192];
}

static char
iso_to_koi8 (char c, bool ukrainian) {
  int i= (int) ((unsigned char) c);
  if (c=='³') return (char) 156;
  if (c=='£') return (char) 188;
  if (ukrainian)
  {
     switch(c)
     {
         case '¶':return 'I';
         case '·':return 'ˆ';
         case '´':return '™';
         case '½':return '€';
         case '¦':return 'i';
         case '§':return '¨';
         case '¤':return '¹';
         case '­':return ' ';
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
* Convert between TeXmacs and XML strings
******************************************************************************/

static bool
is_xml_name (char c) {
  return
    is_alpha (c) || is_numeric (c) ||
    (c == '.') || (c == '-') || (c == ':');
}

string
tm_to_xml_name (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (is_xml_name (s[i])) r << s[i];
    else r << "_" << as_string ((int) ((unsigned char) s[i])) << "_";
  return r;
}

string
xml_name_to_tm (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] != '_') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!='_')) i++;
      r << (char) ((unsigned char) as_int (s (start, i)));
    }
  return r;
}

string
old_tm_to_xml_cdata (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '>') r << "&gt;";
    else if (s[i] != '<') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!='>')) i++;
      r << "&" << tm_to_xml_name (s (start, i)) << ";";
    }
  return r;
}

object
tm_to_xml_cdata (string s) {
  array<object> a;
  a << symbol_object ("!concat");
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '>') r << "&gt;";
    else if (s[i] == '\\') r << "\\";
    else if (s[i] != '<') r << cork_to_utf8 (s (i, i+1));
    else {
      int start= i++;
      while ((i<n) && (s[i]!='>')) i++;
      string ss= s (start, i+1);
      string rr= cork_to_utf8 (ss);
      string qq= utf8_to_cork (rr);
      if (rr != ss && qq == ss) r << rr;
      else {
	if (r != "") a << object (r);
	a << cons (symbol_object ("tm-sym"),
		   cons (ss (1, N(ss)-1),
			 null_object ()));
	r= "";
      }
    }
  if (r != "") a << object (r);
  if (N(a) == 1) return object ("");
  else if (N(a) == 2) return a[1];
  else return call ("list", a);
}

string
old_xml_cdata_to_tm (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '<') r << "<less>";
    else if (s[i] == '>') r << "<gtr>";
    else if (s[i] != '&') r << s[i];
    else {
      int start= ++i;
      while ((i<n) && (s[i]!=';')) i++;
      string x= "<" * xml_name_to_tm (s (start, i)) * ">";
      if (x == "<amp>") r << "&";
      else r << x;
    }
  return r;
}

string
xml_unspace (string s, bool first, bool last) {
  string r;
  int i= 0, n= N(s);
  if (first) while ((i<n) && is_space (s[i])) i++;
  while (i<n)
    if (!is_space (s[i])) r << s[i++];
    else {
      while ((i<n) && is_space (s[i])) i++;
      if ((i<n) || (!last)) r << ' ';
    }
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

static string ones[10]= {
  "", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix" };
static string tens[10]= {
  "", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc" };
static string hundreds[10]= {
  "", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm" };

string
roman_nr (int nr) {
  if (nr<0) return "-" * roman_nr (nr);
  if (nr==0) return "o";
  if (nr>1000) return "m" * roman_nr (nr-1000);
  if (nr==1000) return "m";
  if (nr==999) return "im";
  if (nr==499) return "id";
  if ((nr%100)==99) return hundreds[nr/100] * "ic";
  if ((nr%100)==49) return hundreds[nr/100] * "il";
  return hundreds[nr/100] * tens[(nr%100)/10] * ones[nr%10];
}

string
Roman_nr (int nr) {
  return upcase_all (roman_nr (nr));
}

string
alpha_nr (int nr) {
  if (nr<0) return "-" * alpha_nr (nr);
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
  string sym, r;
  int i, m= (nr-1)%3, n= ((nr-1)/3)+1;
  switch (m) {
  case 0: sym= "<ast>"; break;
  case 1: sym= "<dag>"; break;
  case 2: sym= "<ddag>"; break;
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
    if ((s[i] >= '0') && (s[i] <= '9')) res += (int) (s[i] - '0');
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
  register int i;
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
  register int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      register int j;
      for (j=i+1; j<N(s); j++)
	if (s[j]=='>') break;
      if (j<N(s)) j++;
      if (s(i,j) == "<less>") r << "<";
      else if (s(i,j) == "<gtr>") r << ">";
      i=j-1;
      if (s[i]!='>') return r;
    }
    else if (s[i]!='>') r << s[i];
  }
  return r;
}

string
tm_var_encode (string s) {
  register int i, n= N(s);
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
  register int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      register bool flag= true;
      register int j, k;
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
  if ((N(s)>=2) && (s[0]=='\"') && (s[N(s)-1]=='\"')) {
    int i, n= N(s);
    string r;
    for (i=1; i<n-1; i++)
      if (s[i] == '\\' && (s[i+1] == '\"' || s[i+1] == '\\')) r << s[++i];
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
  if ((N(s)>=2) && (s[0]=='\"') && (s[N(s)-1]=='\"'))
    return s (1, N(s)-1);
  else return s;
}

/******************************************************************************
* Handling escape characters
******************************************************************************/

string
escape_sh (string s) {
#if defined (__MINGW__) || defined (__MINGW32__) || defined (OS_WIN32)
  return raw_quote (s);
#else
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '?':
    case '&':
    case '$':
    case '`':
    case '\"':
    case '\\':
    case ' ':
      r << '\\' << s[i];
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

void
skip_spaces (string s, int& i) {
  int n=N(s);
  while ((i<n) && ((s[i]==' ') || (s[i]=='\t'))) i++;
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
  int i=0, next, n= N(s);
  while (i<n) {
    next= search_forwards (s, i, in);
    if (next == -1) break;
    count++;
    i= next+1;
  }
  return count;
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
