
/******************************************************************************
* MODULE     : solutions.cpp
* DESCRIPTION: solutions to property queries
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

/******************************************************************************
* Extracting information from solutions
******************************************************************************/

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
* Unification
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

/******************************************************************************
* Simplify a list of queries according to previously found solutions
******************************************************************************/

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
