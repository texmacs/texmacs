
/******************************************************************************
* MODULE     : base64.hpp
* DESCRIPTION: Implementation of the base64 coding as described by RFC-3548.
* COPYRIGHT  : (C) 2013  Francois Poulain
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "array.hpp"
#include "string.hpp"
#include "base64.hpp"

// 0 maps to 'A', 1 maps to 'B', and so on ...
static const char
int_to_b64[]= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

inline static string
encode_base64 (char c1, char c2, char c3) {
  mut_string r(4);
  int idx0, idx1, idx2, idx3;

  idx0= ((c1 >> 2) & 0x3F);
  idx1= (((c1 << 4) & 0x30) + ((c2 >> 4) & 0x0F));
  idx2= (((c2 << 2) & 0x3C) + ((c3 >> 6) & 0x03));
  idx3= (c3 & 0x3F);

  r[0]= int_to_b64[idx0];
  r[1]= int_to_b64[idx1];
  r[2]= int_to_b64[idx2];
  r[3]= int_to_b64[idx3];
  return r;
}

string
encode_base64 (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i+2<n; i+=3) {
    if (i > 0 && i % 60 == 0) r << "\n";
    r << encode_base64 (s[i], s[i+1], s[i+2]);
  }

  if (i == n-1) {
    if (i > 0 && i % 60 == 0) r << "\n";
    r << encode_base64 (s[i], '\0', '\0')(0,2) << "==";
  }
  else if (i == n-2) {
    if (i > 0 && i % 60 == 0) r << "\n";
    r << encode_base64 (s[i], s[i+1], '\0')(0,3) << "=";
  }
  return r;
}

// 'A' maps to (64+0), 'B' maps to (64+1), and so on, until '/' maps to (64+63)
// Others maps to 43 ('?')
static const char
b64_to_int[]= "???????????????????????????????????????????~???\177tuvwxyz{|}??\
?????@ABCDEFGHIJKLMNOPQRSTUVWXY??????Z[\\]^_`abcdefghijklmnopqrs?????";


string
decode_base64 (array<int> ac) {
  mut_string r(3);
  int n= N(ac), n1, n2, n3, n4;

  n1= b64_to_int[(int)ac[0]] - 64;
  n2= b64_to_int[(int)ac[1]] - 64;
  n3= (n>2)? b64_to_int[(int)ac[2]] - 64 : 0;
  n4= (n>3)? b64_to_int[(int)ac[3]] - 64 : 0;

  r[0]= ((n1 << 2) & 0xFC) + ((n2 >> 4) & 0x03);
  r[1]= ((n2 << 4) & 0xF0) + ((n3 >> 2) & 0x0F);
  r[2]= ((n3 << 6) & 0xC0) + (n4 & 0x3F);

  return r(0,n-1);
}

string
decode_base64 (string s) {
  string r;
  bool end= false;
  array<int> tmp;
  int i, n= N(s), cnt=0;
  for (i=0; i<n && !end; i++) {
    if (b64_to_int[(int)s[i]] != '?') {
      tmp << (int)s[i];
      cnt++;
    }
    else if (s[i] == '=') {
      end= true;
    }

    if (cnt == 4 || end) {
      r << decode_base64 (tmp);
      cnt= 0;
      tmp= array<int> ();
    }
  }
  return r;
}
