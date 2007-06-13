
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
* Adding and removing properties
******************************************************************************/

static void
property_treat (transaction& t, property p, int i, property val, int eps) {
  //cout << "Property treat " << p << ", " << i << ", "
  //<< val << ", " << eps << "\n";
  int j, n= N(p);
  add (t, property_encode (p), property_encode (val), eps);
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
  property_treat (t, p, 1, property (), eps);
  return t;
}

void
tmfs_set_property (property p) {
  tmfs_write (as_transaction (p, 1));
}

void
tmfs_reset_property (property p) {
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

collection
as_collection (solutions sols, string key) {
  collection c;
  for (int i=0; i<N(sols); i++)
    if (sols[i]->contains (key))
      c (sols[i][key])= 1;
  return c;
}

string
get_first_var (property p) {
  int i, n= N(p);
  for (i=0; i<n; i++)
    if (is_unknown (p[i])) return p[i];
  return "";
}

solutions
tmfs_get_property (property query) {
  //cout << "Query: " << query << "\n";
  property p;
  property v;
  property_wildcards (query, p, v);
  collection c= tmfs_get (property_encode (p));
  return decode_solutions (c, v);
}

collection
tmfs_get_property_value (property query) {
  solutions sols= tmfs_get_property (query);
  string var= get_first_var (query);
  if (!is_unknown (var)) return collection ();
  return as_collection (sols, var);
}

/******************************************************************************
* Combining queries
******************************************************************************/

property
simplify (property query, solutions sols) {
  query= copy (query);
  int i, n= N(query);
  for (i=0; i<n; i++)
    if (is_unknown (query[i])) {
      string r= query[i];
      for (int j=0; j<N(sols); j++)
	if (sols[j]->contains (query[i])) {
	  if (is_unknown (r)) r= sols[j][query[i]];
	  else {
	    if (sols[j][query[i]] == r) continue;
	    r= query[i];
	    break;
	  }
	}
      query[i]= r;
    }
  return query;
}

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
tmfs_get_property (solutions sols1, property p) {
  property query= simplify (p, sols1);
  solutions sols2= tmfs_get_property (query);
  return combine (sols1, sols2);
}

collection
tmfs_get_property_value (solutions sols1, property query) {
  solutions sols2= tmfs_get_property (sols1, query);
  string var= get_first_var (query);
  if (!is_unknown (var)) return collection ();
  return as_collection (sols2, var);
}

/******************************************************************************
* File attributes
******************************************************************************/

transaction
attribute_as_transaction (property p, int eps) {
  transaction t= as_transaction (p, eps);
  for (int i=1; i<N(p); i++)
    if (is_identifier (p[i]))
      add (t, "@" * p[i], property_encode (p), eps);
  return t;
}

void
tmfs_set_attribute (property p) {
  tmfs_write (attribute_as_transaction (p, 1));
}

void
tmfs_set_attributes (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_set_attribute (ps[i]);
}

void
tmfs_reset_attribute (property p) {
  tmfs_write (attribute_as_transaction (p, -1));
}

void
tmfs_reset_attributes (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_reset_attribute (ps[i]);
}

solutions
tmfs_get_attribute (property p) {
  tmfs_get_property (p);
}

collection
tmfs_get_attribute_value (property p) {
  tmfs_get_property_value (p);
}

properties
tmfs_get_attributes (string s) {
  properties ps;
  collection c= tmfs_get ("@" * s);
  iterator<string> it= iterate (c);
  while (it->busy ())
    ps << property_decode (it->next ());
  return ps;
}
