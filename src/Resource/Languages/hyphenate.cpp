
/******************************************************************************
* MODULE     : hyphenate.cpp
* DESCRIPTION: hyphenation by Liang's algorithm
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "file.hpp"
#include "Languages/hyphenate.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int SI;
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
    while ((i<n) && (s[i]>='0') && (s[i]<='9')) i++;
    if (i<n) r << s[i++];
  }
  return r;
}

static int
unhex (char c) {
  if ((c>='0') && (c<='9')) return ((int) c)- ((int) '0');
  if ((c>='a') && (c<='f')) return 10+ ((int) c)- ((int) 'a');
  if ((c>='A') && (c<='F')) return 10+ ((int) c)- ((int) 'A');
  return 0;
}

static string
hyphen_normalize (string s) {
  int i;
  string r (0);
  for (i=0; i<N(s); i++)
    if ((i+3<N(s)) && (s[i]=='^') && (s[i+1]=='^')) {
      r << ((char) (unhex (s[i+2])*16 + unhex (s[i+3])));
      i+=3;
    }
    else r << s[i];
  return r;
}

hashmap<string,string>
load_hyphen_table (string file_name) {
  string s;
  file_name= string ("hyphen.") * file_name;
  load_string (url ("$TEXMACS_PATH/langs/natural/hyphen", file_name), s, true);
  if (DEBUG_AUTO) cout << "TeXmacs] Loading " << file_name << "\n";

  hashmap<string,string> H ("?");
  bool flag=false;
  int i=0, n= N(s);
  while (i<n) {
    string buffer;
    while ((i<n) && (s[i]!=' ') && (s[i]!='\t') && (s[i]!='\n')) {
      if (s[i] != '%') buffer << s[i++];
      else while ((i<n) && (s[i]!='\n')) i++;
    }
    if (i<n) i++;
    if (buffer == "}") flag=false;
    string norm= hyphen_normalize (buffer);
    //cout << norm << "\n";
    if (flag && (i!=0)) H (unpattern (norm))= norm;
    if (buffer == "\\patterns{") flag=true;
  }

  // cout << file_name << " done!\n";
  return H;
}

static string
lower_case (string s) {
  int i;
  string r (N(s));
  for (i=0; i<N(s); i++) {
    if ((s[i]>='A') && (s[i]<='Z'))
      r[i]= (char) (((int) s[i])+ ((int) 'a')- ((int) 'A'));
    else r[i]=s[i];
  }
  return r;
}

array<int>
get_hyphens (string s, hashmap<string,string> H) {
  if (N(s)==0) fatal_error ("hyphenation of empty string", "get_hyphens");
  s= "." * lower_case (s) * ".";
  // cout << s << "\n";
  int i, j, k, m, len;
  array<int> T (N(s)+1);
  for (i=0; i<N(s)+1; i++) T[i]=0;
  for (len=1; len < MAX_SEARCH; len++)
    for (i=0; i<N(s)-len; i++) {
      string r= H [s (i, i+len)];
      if (!(r == "?")) {
	// cout << "  " << s (i, i+len) << " => " << r << "\n";
	for (j=0, k=0; j<=len; j++, k++) {
	  if ((k<N(r)) && (r[k]>='0') && (r[k]<='9')) {
	    m=((int) r[k])-((int) '0');
	    k++;
	  }
	  else m=0;
	  if (m>T[i+j]) T[i+j]=m;
	}
      }
    }

  array<int> penalty (N(s)-3);
  for (i=2; i<N(s)-1; i++)
    penalty [i-2]= (((T[i]&1)==1)? HYPH_STD: HYPH_INVALID);
  if (N(penalty)>0) penalty[0] = penalty[N(penalty)-1] = HYPH_INVALID;
  if (N(penalty)>1) penalty[1] = penalty[N(penalty)-2] = HYPH_INVALID;
  if (N(penalty)>2) penalty[N(penalty)-3] = HYPH_INVALID;
  // cout << "  -> " << penalty << "\n";
  return penalty;
}

void
std_hyphenate (string s, int after, string& left, string& right, int penalty) {
  left = s (0, after+1);
  right= s (after+1, N(s));
  if (penalty >= HYPH_INVALID) left << string ("\\");
  else left << string ("-");
}
