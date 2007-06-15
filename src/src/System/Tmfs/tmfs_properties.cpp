
/******************************************************************************
* MODULE     : tmfs_properties.cpp
* DESCRIPTION: property database
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

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
property_encode (property p) {
  if (N(p) == 0) return "";
  string r= copy (p[0]);
  for (int i=1; i<N(p); i++) r << ',' << p[i];
  return r;
}

property
property_decode (string s) {
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

/******************************************************************************
* Routines for properties and solutions
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

property
simplify (property p, solutions sols) {
  p= copy (p);
  int i, n= N(p);
  for (i=0; i<n; i++)
    if (is_unknown (p[i])) {
      string r= p[i];
      for (int j=0; j<N(sols); j++)
	if (sols[j]->contains (p[i])) {
	  if (is_unknown (r)) r= sols[j][p[i]];
	  else {
	    if (sols[j][p[i]] == r) continue;
	    r= p[i];
	    break;
	  }
	}
      p[i]= r;
    }
  return p;
}

properties
exclude_types (properties ps, collection c) {
  properties r;
  for (int i=0; i<N(ps); i++)
    if (N(ps[i])>0 && !c->contains (ps[i][0]))
      r << ps[i];
  return r;
}

collection
as_collection (solutions sols, string key) {
  collection c;
  for (int i=0; i<N(sols); i++)
    if (sols[i]->contains (key))
      c (sols[i][key])= 1;
  return c;
}

collection
as_collection (solutions sols, property p) {
  int i, n= N(p);
  for (i=0; i<n; i++)
    if (is_unknown (p[i])) return as_collection (sols, p[i]);
  return collection ();
}

/******************************************************************************
* Adding and removing properties
******************************************************************************/

static void
property_treat (transaction& t, property p, int i, property val, int eps) {
  //cout << "Property treat " << p << ", " << i << ", "
  //<< val << ", " << eps << "\n";
  int j, n= N(p);
  for (j=1; j<n; j++)
    if (p[j] != "*") {
      add (t, property_encode (p), property_encode (val), eps);
      break;
    }
  for (j=i; j<n; j++) {
    property p2  = copy (p);
    property val2= copy (val);
    p2[j]= "*";
    val2 << p[j];
    property_treat (t, p2, j+1, val2, eps);
  }
}

transaction
as_transaction (property p, int eps) {
  p= property_quote (p);
  transaction t;
  property_treat (t, p, 0, property (), eps);
  return t;
}

void
tmfs_raw_set_property (property p) {
  tmfs_write (as_transaction (p, 1));
}

void
tmfs_raw_set_properties (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_raw_set_property (ps[i]);
}

void
tmfs_raw_reset_property (property p) {
  for (int i=0; i<N(p); i++)
    if (is_unknown (p[i])) {
      properties ps= tmfs_raw_get_matches (p);
      for (int j=0; j<N(ps); j++)
	tmfs_write (as_transaction (ps[j], -1));
      return;
    }
  tmfs_write (as_transaction (p, -1));
}

/******************************************************************************
* Querying for properties
******************************************************************************/

void
property_wildcards (property query, property& p, property &v) {
  p= property_quote (query);
  v= property ();
  int i, n= N(query);
  for (i=0; i<n; i++)
    if (is_unknown (query[i])) {
      p[i]= "*";
      v << query[i];
    }
}

solutions
decode_solutions (collection t, property v) {
  solutions sols;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    solution sol;
    string s= it->next ();
    property d= property_unquote (property_decode (s));
    if (t[s] > 0 && N(v) <= N(d))
      for (int i=0; i<N(v); i++)
	sol(v[i])= d[i];
    sols << sol;
  }
  return sols;
}

solutions
tmfs_raw_get_solutions (property query) {
  // cout << "  Query: " << query << "\n";
  property p;
  property v;
  property_wildcards (query, p, v);
  collection c= tmfs_get (property_encode (p));
  return decode_solutions (c, v);
}

collection
tmfs_raw_get_values (property query) {
  return as_collection (tmfs_raw_get_solutions (query), query);
}

properties
tmfs_raw_get_matches (property query) {
  return substitute (query, tmfs_raw_get_solutions (query));
}

/******************************************************************************
* Combining queries
******************************************************************************/

solutions
combine (solutions sols1, solutions sols2) {
  solutions sols;
  int i1, i2, n1= N(sols1), n2= N(sols2);
  for (i1=0; i1<n1; i1++)
    for (i2=0; i2<n2; i2++) {
      bool ok= true;
      solution sol= copy (sols1[i1]);
      iterator<string> it= iterate (sols2[i2]);
      while (it->busy ()) {
	string s= it->next ();
	if (sol->contains (s)) {
	  ok= (sol[s] == sols2[i2][s]);
	  if (!ok) break;
	}
	else sol(s)= sols2[i2][s];
      }
      if (ok) sols << sol;
    }
  return sols;
}

solutions
tmfs_raw_get_solutions (solutions sols1, property query) {
  //cout << "Get property " << sols1 << ", " << query << "\n";
  solutions sols2;
  hashmap<string,solutions> cache;
  for (int i=0; i<N(sols1); i++) {
    property p= substitute (query, sols1[i]);
    string s= property_encode (p);
    if (!cache->contains (s))
      cache(s)= tmfs_raw_get_solutions (p);
    solutions sols3; sols3 << sols1[i];
    sols2 << combine (sols3, cache[s]);
  }
  return sols2;
}

solutions
tmfs_var_get_solutions (solutions sols1, property p) {
  property query= simplify (p, sols1);
  solutions sols2= tmfs_raw_get_solutions (query);
  return combine (sols1, sols2);
}
