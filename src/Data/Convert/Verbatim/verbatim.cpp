
/******************************************************************************
* MODULE     : verbatim.cpp
* DESCRIPTION: routines for converting between TeXmacs trees and text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "convert.hpp"
#include "vars.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Verbatim input
******************************************************************************/

void
tree_to_verbatim (string& buf, tree t, bool pritty) {
  if (is_atomic (t)) {
    string s= tm_decode (t->label);
    if (pritty) {
      int i, i0=0, j, n=N(s), l, k=N(buf);
      for (l=0; l<k; l++)
	if (buf[k-1-l] == '\n') break;
      for (i=0; i<n; i++)
	if (s[i] == ' ') {
	  for (j=i+1; j<n; j++)
	    if (s[j] == ' ') break;
	  if (l+j-i0 > 78) {
	    buf << '\n';
	    l=0; i0=i+1;
	  }
	  else buf << s[i];
	}
	else buf << s[i];
    }
    else buf << s;
  }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (std_drd->is_accessible_child (t, i)) {
	if (is_document (t) && (i>0)) {
	  buf << "\n";
	  if (pritty) buf << "\n";
	}
	tree_to_verbatim (buf, t[i], pritty);
      }
  }
}

string
tree_to_verbatim (tree t, bool pritty) {
  if (!is_snippet (t)) t= extract (t, "body");
  string buf;
  tree_to_verbatim (buf, t, pritty);
  return buf;
}

/******************************************************************************
* Verbatim output
******************************************************************************/

static string
un_special (string s) {
  int i, j;
  string r;
  for (i=0, j=0; i<N(s); i++, j++)
    if (s[i]=='\t') {
      do {
	r << " "; j++;
      } while ((j&7) != 0);
      j--;
    }
    else if ((s[i]=='\b') && (N(r)>0) && (r[N(r)-1]!='\n'))
      r->resize (N(r)-1);
    else r << s[i];
  return r;
}

tree
verbatim_to_tree (string s) {
  int i, j;
  for (i=0; i<N(s); i++)
    if (s[i]=='\n') {
      tree t (DOCUMENT);
      for (i=0, j=0; i<N(s); i++)
	if (s[i]=='\n') {
	  t << tm_encode (un_special (s (j, i)));
	  j= i+1;
	}
      t << tm_encode (un_special (s (j, i)));
      return t;
    }
  return tm_encode (un_special (s));
}

tree
verbatim_document_to_tree (string s) {
  tree t    = verbatim_to_tree (s);
  tree init = tree (COLLECTION,
		    tree (ASSOCIATE, TEXT_LANGUAGE, "verbatim"),
		    tree (ASSOCIATE, TEXT_FAMILY, "tt"),
		    tree (ASSOCIATE, PAR_FIRST, "0cm"));
  return tree (DOCUMENT, compound ("body", t), compound ("initial", init));
}
