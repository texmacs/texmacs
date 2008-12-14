
/******************************************************************************
* MODULE     : tmfs_properties.cpp
* DESCRIPTION: property database
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tmfs.hpp"

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
      add (t, property_append (p), property_append (val), eps);
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
tmfs_set_property (property p) {
  tmfs_write (as_transaction (p, 1));
}

void
tmfs_set_properties (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_set_property (ps[i]);
}

void
tmfs_reset_property (property p) {
  for (int i=0; i<N(p); i++)
    if (is_unknown (p[i])) {
      properties ps= substitute (p, tmfs_get_solutions (p));
      for (int j=0; j<N(ps); j++)
	tmfs_write (as_transaction (ps[j], -1));
      return;
    }
  tmfs_write (as_transaction (p, -1));
}

void
tmfs_reset_properties (properties ps) {
  for (int i=0; i<N(ps); i++)
    tmfs_reset_property (ps[i]);
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
    property d= property_unquote (property_unappend (s));
    if (t[s] > 0 && N(v) <= N(d))
      for (int i=0; i<N(v); i++)
	sol(v[i])= d[i];
    sols << sol;
  }
  return sols;
}

solutions
tmfs_get_solutions (property query) {
  // cout << "  Query: " << query << "\n";
  property p;
  property v;
  property_wildcards (query, p, v);
  collection c= tmfs_get (property_append (p));
  return decode_solutions (c, v);
}

collection
tmfs_get_values (property query) {
  return as_collection (tmfs_get_solutions (query), query);
}

/******************************************************************************
* Combining queries
******************************************************************************/

solutions
tmfs_get_solutions (solutions sols1, property query) {
  //cout << "Get property " << sols1 << ", " << query << "\n";
  solutions sols2;
  hashmap<string,solutions> cache;
  for (int i=0; i<N(sols1); i++) {
    property p= substitute (query, sols1[i]);
    string s= property_append (p);
    if (!cache->contains (s))
      cache(s)= tmfs_get_solutions (p);
    solutions sols3; sols3 << sols1[i];
    sols2 << combine (sols3, cache[s]);
  }
  return sols2;
}

solutions
tmfs_get_solutions (properties queries) {
  solutions sols; sols << solution ();
  for (int i=0; i<N(queries); i++)
    sols= tmfs_get_solutions (sols, queries[i]);
  return sols;
}

solutions
tmfs_var_get_solutions (solutions sols1, property p) {
  property query= simplify (p, sols1);
  solutions sols2= tmfs_get_solutions (query);
  return combine (sols1, sols2);
}
