
/******************************************************************************
* MODULE     : encoding.cpp
* DESCRIPTION: font encodings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*------------------------------------------------------------------------------
* Encodings are abstract objects that give a semantic meaning to strings,
* that is, how to see the string as a sequence of tokens with a well
* defined semantics (where a token may require several chars in the string).
* This is done by providing methods to convert between the encoding
* and a standard 'universal' encoding, in which strings are built
* up from tokens, represented by strings between angular brackets < and >.
* Of course, these tokens are assumed to have some standard well defined
* semantic meaning. For instance, <a> is the token a, <alpha> the token
* \alpha in TeX, <'a> an accented a, and <leq> the less-or-equal token.
* The methods provided by an encoding are as follows:
*     valid: checks whether a string is valid in the encoding.
*     encode: convert from the universal encoding to the actual one.
*     decode: convert from the actual encoding to the universal one
*     token_forward: move one token forward at a given cursor position.
*     token_backward: move one token backward at a given cursor position.
* The methods token_forward resp. token_backward returns a flag
* which is true if an invalid token in the encoding is encountered
* The routine 'token_forward' is used by default in order to
* check the validity of the entire string in the encoding.
* An empty string is returned in the case of invalid conversions.
******************************************************************************/

#include "encoding.hpp"
#include "hashset.hpp"

RESOURCE_CODE(encoding);

bool
encoding_rep::valid (string s) {
  int i;
  for (i=0; i<N(s); )
    if (token_forward (s, i)) return false;
  return true;
}

/******************************************************************************
* The universal encoding
******************************************************************************/

struct univ_encoding_rep: encoding_rep {
  univ_encoding_rep ();
  string encode (string);
  string decode (string);
  bool token_forward  (string s, int& pos);
  bool token_backward (string s, int& pos);
};

univ_encoding_rep::univ_encoding_rep (): encoding_rep ("universal") {}

string
univ_encoding_rep::encode (string s) {
  if (!valid (s)) return string("");
  return s;
}

string
univ_encoding_rep::decode (string s) {
  if (!valid (s)) return string("");
  return s;
}

bool
univ_encoding_rep::token_forward (string s, int& pos) {
  if (pos==N(s)) return false;
  if (s[pos]!='<') return true;
  if ((pos<=N(s)-3) && ((s[pos+1]=='<')||(s[pos+1]=='>')) && (s[pos+2]=='>')) {
    pos+=3;
    return false;
  }
  do {
    pos++;
    if (pos==N(s)) return true;
    if (s[pos]=='<') return true;
  } while (s[pos]!='>');
  pos++;
  return false;
}

bool
univ_encoding_rep::token_backward (string s, int& pos) {
  if (pos==0) return false;
  pos--;
  if (s[pos]!='>') return true;
  if ((pos>=2) && ((s[pos-1]=='<')||(s[pos-1]=='>')) && (s[pos-2]=='<')) {
    pos-=2;
    return false;
  }
  do {
    if (pos==0) return true;
    pos--;
    if (s[pos]=='>') return true;
  } while (s[pos]!='<');
  return false;
}

/******************************************************************************
* sub-encodings
******************************************************************************/

struct sub_encoding_rep: encoding_rep {
  encoding enc;
  hashset<string> valid_tokens;

  sub_encoding_rep (string name, encoding& enc2, hashset<string>& S);
  string encode (string);
  string decode (string);
  bool token_forward  (string s, int& pos);
  bool token_backward (string s, int& pos);
};

sub_encoding_rep::sub_encoding_rep (string name, encoding& enc2,
				    hashset<string>& S):
  encoding_rep (name), enc (enc2), valid_tokens (S) {}

string
sub_encoding_rep::encode (string s) {
  string r=enc->encode (s);
  if (!valid (r)) return string("");
  return r;
}

string
sub_encoding_rep::decode (string s) {
  if (!valid (s)) return string("");
  return enc->decode (s);
}

bool
sub_encoding_rep::token_forward (string s, int& pos) {
  int start=pos;
  if (enc->token_forward (s, pos)) return true;
  return !valid_tokens->contains (s (start, pos));
}

bool
sub_encoding_rep::token_backward (string s, int& pos) {
  int end=pos;
  if (enc->token_backward (s, pos)) return true;
  return !valid_tokens->contains (s (pos, end));
}

/******************************************************************************
* ascii encodings
******************************************************************************/

struct ascii_encoding_rep: encoding_rep {
  bool (*in_range) (char);
  ascii_encoding_rep (string name, bool (*in_range) (char));
  string encode (string);
  string decode (string);
  bool token_forward  (string s, int& pos);
  bool token_backward (string s, int& pos);
};

ascii_encoding_rep::ascii_encoding_rep (string name, bool (*in_range2) (char)):
  encoding_rep (name), in_range (in_range2) {}

string
ascii_encoding_rep::encode (string s) {
  if ((N(s)%3)!=0) return string ("");
  int i;
  string r(N(s)/3);
  for (i=0; i<N(s); i+=3) {
    if ((s[3*i]!='<') ||
	(!in_range (s[3*i+1])) ||
	(s[3*i+2]!='>')) return string ("");
    r[i]= s[3*i+1];
  }
  return r;
}

string
ascii_encoding_rep::decode (string s) {
  int i;
  string r(3*N(s));
  for (i=0; i<N(s); i++) {
    if (!in_range (s[i])) return string ("");
    r[3*i  ]= '<';
    r[3*i+1]= s[i];
    r[3*i+2]= '>';
  }
  return r;
}

bool
ascii_encoding_rep::token_forward (string s, int& pos) {
  if (pos==N(s)) return false;
  return !in_range (s[pos++]);
}

bool
ascii_encoding_rep::token_backward (string s, int& pos) {
  if (pos==0) return false;
  return !in_range (s[--pos]);
}

/******************************************************************************
* joining encodings
******************************************************************************/

struct join_encoding_rep: encoding_rep {
  encoding enc1, enc2;
  join_encoding_rep (string name, encoding enc1, encoding enc2);
  string encode (string);
  string decode (string);
  bool token_forward  (string s, int& pos);
  bool token_backward (string s, int& pos);
};

join_encoding_rep::join_encoding_rep (string name,
				      encoding enc1b, encoding enc2b):
  encoding_rep (name), enc1 (enc1b), enc2 (enc2b) {}

string
join_encoding_rep::encode (string s) {
  string r(0);
  int pos=0;
  while (pos<N(s)) {
    int start=pos;
    if (universal_enc->token_forward (s, pos)) return string ("");
    string ss = s (start, pos);
    string ss1= enc1->encode (ss);
    if (N(ss1)!=0) r << ss1;
    else {
      string ss2= enc2->encode (ss);
      if (N(ss1)!=0) r << ss2;
      else return string ("");
    }
  }
  return r;
}

string
join_encoding_rep::decode (string s) {
  string r(0);
  int pos=0;
  while (pos<N(s)) {
    int start=pos;
    if (enc1->token_forward (s, pos)) {
      pos=start;
      if (enc2->token_forward (s, pos)) return string ("");
      else {
	string ss = s (start, pos);
	r << enc2->decode (ss);
      }
    }
    else {
      string ss = s (start, pos);
      r << enc1->decode (ss);
    }
  }
  return r;
}

bool
join_encoding_rep::token_forward (string s, int& pos) {
  int retry= pos;
  if (enc1->token_forward (s, pos)) {
    pos= retry;
    return enc2->token_forward (s, pos);
  }
  return false;
}

bool
join_encoding_rep::token_backward (string s, int& pos) {
  int retry= pos;
  if (enc1->token_backward (s, pos)) {
    pos= retry;
    return enc2->token_backward (s, pos);
  }
  return false;
}

encoding
join (encoding enc1, encoding enc2) {
  string name= enc1->res_name * "|" * enc2->res_name;
  return make (encoding, name, new join_encoding_rep (name, enc1, enc2));
}

/******************************************************************************
* some important standard encodings
******************************************************************************/

static bool char_is_numeric (char c) {
  return (c=='.') || ((c>='0') && (c<='9')) || (c==','); }
static bool char_is_capital (char c) {
  return ((c>='A') && (c<='Z')); }
static bool char_is_alpha (char c) {
  return ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z')); }
static bool char_is_alpha_num (char c) {
  return
    (c=='-') || ((c>='0') && (c<='9')) || (c=='.') || (c==',') ||
    ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z')); }
static bool always_true (char c) { (void) c;
  return true; }
static bool char_not_less_gtr (char c) {
  return (c!='<') & (c!='>'); }

static string ALW ("always");
static string AAL ("almost_always");
encoding universal_enc= new univ_encoding_rep ();
encoding always_enc= new ascii_encoding_rep (ALW, always_true);
encoding almost_always_enc= new ascii_encoding_rep (AAL, char_not_less_gtr);
encoding math_enc= join (universal_enc, almost_always_enc);

encoding
num_enc () {
  return make (encoding, "num",
    new ascii_encoding_rep ("num", char_is_numeric));
}

encoding
capital_enc () {
  return make (encoding, "capital",
    new ascii_encoding_rep ("capital", char_is_capital));
}

encoding
alpha_enc () {
  return make (encoding, "alpha",
    new ascii_encoding_rep ("alpha", char_is_alpha));
}

encoding
alpha_num_enc () {
  return make (encoding, "alpha",
    new ascii_encoding_rep ("alphanum", char_is_alpha_num));
}
