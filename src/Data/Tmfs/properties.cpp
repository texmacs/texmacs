
/******************************************************************************
* MODULE     : properties.cpp
* DESCRIPTION: properties
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Building arrays of strings
******************************************************************************/

strings
seq (string s1) {
  strings a; a << s1; return a;
}

strings
seq (string s1, string s2) {
  strings a; a << s1 << s2; return a;
}

strings
seq (string s1, string s2, string s3) {
  strings a; a << s1 << s2 << s3; return a;
}

strings
seq (string s1, string s2, string s3, string s4) {
  strings a; a << s1 << s2 << s3 << s4; return a;
}

/******************************************************************************
* Encoding and decoding of properties
******************************************************************************/

property
property_quote (property p) {
  property q;
  int i, n= N(p);
  for (i=0; i<n; i++) {
    string s= p[i], r;
    for (int j=0; j<N(s); j++)
      if (s[j] == ',' || s[j] == '*' || s[j] == '\\') r << '\\' << s[j];
      else r << s[j];
    q << r;
  }
  return q;
}

property
property_unquote (property p) {
  property q;
  int i, n= N(p);
  for (i=0; i<n; i++) {
    string s= p[i], r;
    for (int j=0; j<N(s); j++)
      if (s[j] == '\\' && (j+1 < N(s))) { j++; r << s[j]; }
      else r << s[j];
    q << r;
  }
  return q;
}

string
property_append (property p) {
  if (N(p) == 0) return "";
  string r= copy (p[0]);
  for (int i=1; i<N(p); i++) r << ',' << p[i];
  return r;
}

property
property_unappend (string s) {
  property p;
  int i, n= N(s);
  for (i=0; i<n; i++) {
    int start= i;
    while (i<n && s[i] != ',') {
      if (s[i] == '\\' && (i+1<n)) i+=2;
      else i++;
    }
    p << s (start, i);
  }
  return p;
}

string
properties_encode (properties ps) {
  string r;
  for (int i=0; i<N(ps); i++)
    r << property_append (property_quote (ps[i])) << '\n';
  return r;
}

properties
properties_decode (string s) {
  properties ps;
  for (int i=0; i<N(s); i++) {
    int start= i;
    while (i<N(s) && s[i] != '\n') i++;
    ps << property_unquote (property_unappend (s (start, i)));
  }
  return ps;
}

/******************************************************************************
* Matching queries
******************************************************************************/

bool
matches (property p, property q) {
  if (N(p) != N(q)) return false;
  solution sol;
  for (int i=0; i<N(p); i++)
    if (is_unknown (q[i])) {
      if (!sol->contains (q[i])) sol(q[i])= p[i];
      else if (sol[q[i]] != p[i]) return false;
    }
    else if (q[i] != p[i]) return false;
  return true;
}

properties
reset (properties ps, property p) {
  properties r;
  for (int i=0; i<N(ps); i++)
    if (!matches (ps[i], p))
      r << ps[i];
  return r;
}

properties
reset (properties ps, properties qs) {
  for (int i=0; i<N(qs); i++)
    ps= reset (ps, qs[i]);
  return ps;
}

/******************************************************************************
* Substitutions
******************************************************************************/

property
substitute (property p, string what, string by) {
  property q= copy (p);
  for (int i=0; i<N(p); i++)
    if (p[i] == what) q[i]= by;
  return q;
}

properties
substitute (properties ps, string what, string by) {
  properties qs;
  for (int i=0; i<N(ps); i++)
    qs << substitute (ps[i], what, by);
  return qs;
}

property
substitute (property p, solution sol) {
  property r= copy (p);
  for (int i=0; i<N(p); i++)
    if (sol->contains (p[i]))
      r[i]= sol[p[i]];
  return r;
}

properties
substitute (property p, solutions sols) {
  properties r;
  for (int i=0; i<N(sols); i++)
    r << substitute (p, sols[i]);
  return r;
}

/******************************************************************************
* Other routines
******************************************************************************/

properties
exclude_types (properties ps, collection c) {
  properties r;
  for (int i=0; i<N(ps); i++)
    if (N(ps[i])>0 && !c->contains (ps[i][0]))
      r << ps[i];
  return r;
}

properties
widen (properties ps) {
  properties r;
  for (int i=0; i<N(ps); i++) {
    property c= copy (ps[i]);
    if (N(c) > 0) c[N(c)-1]= "?any";
    r << c;
  }
  return r;
}
