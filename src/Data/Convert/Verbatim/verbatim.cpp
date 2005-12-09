
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
#include "converter.hpp"
#include "vars.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Verbatim input
******************************************************************************/

static void
tree_to_verbatim (string& buf, tree t, bool pretty, string enc) {
  if (is_atomic (t)) {
    string s;
    if (enc == "utf-8") s= cork_to_utf8 (t->label);
    else s= tm_decode (t->label);
    buf << s;
  }
  else if (is_func (t, SURROUND, 3)) {
    tree_to_verbatim (buf, t[0], pretty, enc);
    tree_to_verbatim (buf, t[2], pretty, enc);
    tree_to_verbatim (buf, t[1], pretty, enc);
  }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (std_drd->is_accessible_child (t, i)) {
	if (is_document (t) && (i>0)) {
	  buf << "\n";
	  if (pretty) buf << "\n";
	}
	tree_to_verbatim (buf, t[i], pretty, enc);
      }
  }
}

string
unix_to_dos (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\n') r << "\r\n";
    else r << s[i];
  return r;
}

string
tree_to_verbatim (tree t, bool pretty, string enc) {
  if (!is_snippet (t)) t= extract (t, "body");
  string buf;
  tree_to_verbatim (buf, t, pretty, enc);
  if (pretty) {
    int i= 0, n= N(buf);
    while (i<n) {
      int start= i;
      while (i<n && buf[i]!='\n') i++;
      while (i-start > 78) {
	int mid= start+78;
	while (mid>start && buf[mid] != ' ') mid--;
	if (mid<=start) while (mid<i && buf[mid] != ' ') mid++;
	if (mid<i) { buf[mid]= '\n'; start= mid+1; }
	else break;
      }
      if (i<n) i++;
    }
  }
#ifdef OS_WIN32
  return unix_to_dos (buf);
#else
  return buf;
#endif
}

/******************************************************************************
* Verbatim output
******************************************************************************/

static string
un_special (string s) {
  int i, j;
  string r;
  for (i=0, j=0; i<N(s); i++, j++)
    if (s[i] == '\t') {
      do {
	r << " "; j++;
      } while ((j&7) != 0);
      j--;
    }
    else if ((s[i] == '\b') && (N(r)>0) && (r[N(r)-1]!='\n'))
      r->resize (N(r)-1);
#ifdef OS_WIN32
    else if (s[i] == '\r');
#endif
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
		    tree (ASSOCIATE, LANGUAGE, "verbatim"),
		    tree (ASSOCIATE, FONT_FAMILY, "tt"),
		    tree (ASSOCIATE, PAR_FIRST, "0cm"));
  return tree (DOCUMENT, compound ("body", t), compound ("initial", init));
}
