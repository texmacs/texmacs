
/******************************************************************************
* MODULE     : hyphenate.cpp
* DESCRIPTION: hyphenation by Liang's algorithm
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "hyphenate.hpp"
#include "analyze.hpp"
#include "converter.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEARCH 10
#define MAX_BUFFER_SIZE 256

/*
static bool
my_strncmp (char* s1, char* s2, int len) {
  int i;
  for (i=0; i<len; i++) if (s1[i]!=s2[i]) return false;
  return true;
}
*/

static string
unpattern (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; ) {
    while (i<n && is_digit (s[i])) i++;
    if (i<n) r << s[i++];
  }
  return r;
}

static string
hyphen_normalize (string s) {
  int i;
  string r (0);
  for (i=0; i<N(s); i++)
    if ((i+3<N(s)) && (s[i]=='^') && (s[i+1]=='^')) {
      r << from_hexadecimal (s (i+2, i+4));
      i+=3;
    }
    else r << s[i];
  return r;
}

void
load_hyphen_tables (string file_name,
                    hashmap<string,string>& patterns,
                    hashmap<string,string>& hyphenations, bool toCork) {
  string s;
  file_name= string ("hyphen.") * file_name;
  load_string (url ("$TEXMACS_PATH/langs/natural/hyphen", file_name), s, true);
  if (DEBUG_VERBOSE)
    debug_automatic << "TeXmacs] Loading " << file_name << "\n";

  if (toCork) s= utf8_to_cork (s);

  hashmap<string,string> H ("?");
  bool pattern_flag=false;
  bool hyphenation_flag=false;
  int i=0, n= N(s);
  while (i<n) {
    string buffer;
    while ((i<n) && (s[i]!=' ') && (s[i]!='\t') && (s[i]!='\n') && (s[i]!='\r')) {
      if (s[i] != '%') buffer << s[i++];
      else while ((i<n) && (s[i]!='\n')) i++;
    }
    if (i<n) i++;
    if (buffer == "}") {
      pattern_flag=false;
      hyphenation_flag=false;
    }
    if (pattern_flag && i != 0 && N(buffer) != 0) {
      string norm= hyphen_normalize (buffer);
      patterns (unpattern (norm))= norm;
      //cout << unpattern (norm) << " ==> " << norm << "\n";
    }
    if (hyphenation_flag && i != 0 && N(buffer) != 0) {
      string word= replace (buffer, "-", "");
      hyphenations (word)= buffer;
      //cout << word << " --> " << buffer << "\n";
    }
    if (buffer == "\\patterns{") pattern_flag=true;
    if (buffer == "\\hyphenation{") hyphenation_flag=true;
  }
}

array<int>
get_hyphens (string s,
             hashmap<string,string> patterns,
             hashmap<string,string> hyphenations) {
  return get_hyphens (s, patterns, hyphenations, false);
}

void
goto_next_char (string s, int &i, bool utf8) {
  if (utf8) decode_from_utf8 (s, i);
  else if (i < N(s)) {
    if (s[i] == '<') {
      i++;
      while (i < N(s) && s[i] != '>') i++;
      if (i < N(s)) i++;
    }
    else i++;
  }
}

int
str_length (string s, bool utf8) {
  if (utf8) {
    int i=0, r=0;
    while (i < N(s)) {
      decode_from_utf8 (s, i);
      r++;
    }
    return r;
  }
  else return N(s);
}

array<int>
get_hyphens (string s,
             hashmap<string,string> patterns,
             hashmap<string,string> hyphenations, bool utf8) {
  ASSERT (N(s) != 0, "hyphenation of empty string");

  if (utf8) s= cork_to_utf8 (s);

  if (hyphenations->contains (s)) {
    string h= hyphenations [s];
    array<int> penalty (str_length (s, utf8)-1);
    int i=0, j=0;
    while (h[j] == '-') j++;
    i++; goto_next_char (h, j, utf8);
    while (i < N(penalty)+1) {
      penalty[i-1]= HYPH_INVALID;
      while (j < N(h) && h[j] == '-') {
        penalty[i-1]= HYPH_STD;
        j++;
      }
      i++;
      goto_next_char (h, j, utf8);
    }
    //cout << s << " --> " << penalty << "\n";
    return penalty;
  }
  else {
    s= "." * locase_all (s) * ".";
    // cout << s << "\n";
    int i, j, k, l, m, len;
    array<int> T (str_length (s, utf8)+1);
    for (i=0; i<N(T); i++) T[i]=0;
    for (len=1; len < MAX_SEARCH; len++)
      for (i=0, l=0; i<N(s) - len; goto_next_char (s, i, utf8), l++) {
        string r= patterns [s (i, i+len)];
        if (!(r == "?")) {
          // cout << "  " << s (i, i+len) << " => " << r << "\n";
          for (j=0, k=0; j<=len; j++, k++) {
            if (k<N(r) && is_digit (r[k])) {
              m= ((int) r[k])-((int) '0');
              k++;
            }
            else m=0;
            if (m>T[l+j]) T[l+j]=m;
          }
        }
      }

    array<int> penalty (N(T)-4);
    for (i=2; i < N(T)-4; i++)
      penalty [i-2]= (((T[i]&1)==1)? HYPH_STD: HYPH_INVALID);
    if (N(penalty)>0) penalty[0] = penalty[N(penalty)-1] = HYPH_INVALID;
    if (N(penalty)>1) penalty[1] = penalty[N(penalty)-2] = HYPH_INVALID;
    if (N(penalty)>2) penalty[N(penalty)-3] = HYPH_INVALID;
    // cout << s << " --> " << penalty << "\n";
    return penalty;
  }
}

void
std_hyphenate (string s, int after, string& left, string& right, int penalty) {
  std_hyphenate (s, after, left, right, penalty, false);
}

void
std_hyphenate (string s, int after, string& left, string& right, int penalty,
               bool utf8) {
  //cout << "Hyphen " << s << ", " << after << "\n";
  if (!utf8) {
    left = s (0, after+1);
    right= s (after+1, N(s));
  }
  else {
    int i= 0, l= 0;
    while (i < N(s) && l <= after) {
      if (s[i] == '<') {
        while (i < N(s) && s[i] != '>') i++;
        if (i < N(s)) i++;
      }
      else i++;
      l++;
    }
    left = s (0, i);
    right= s (i, N(s));
    if (i == N(s)) return;
  }
  if (penalty >= HYPH_INVALID) left << string ("\\");
  else left << string ("-");
  //cout << "Yields " << left << ", " << right << "\n";
}
