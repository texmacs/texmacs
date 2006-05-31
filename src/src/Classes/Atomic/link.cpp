
/******************************************************************************
* MODULE     : link.cpp
* DESCRIPTION: Persistent links between trees
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "link.hpp"

/******************************************************************************
* Routines on links
******************************************************************************/

int nr_link_labels= 0;
hashmap<int,string> link_label_to_name ("?");
hashmap<string,int> name_to_link_label (-1);

link_label
as_link_label (string s) {
  if (!name_to_link_label->contains (s)) {
    link_label_to_name (nr_link_labels)= s;
    name_to_link_label (s)= nr_link_labels++;
  }
  return (link_label) name_to_link_label [s];
}

string
as_string (link_label lab) {
  return link_label_to_name [(int) lab];
}

/******************************************************************************
* Link types
******************************************************************************/

array<tree>
A (link ln) {
  int i, n= N(ln);
  array<tree> a (n);
  for (i=0; i<n; i++)
    a[i]= ln[i];
  return a;
}

link&
operator << (link& ln, array<tree> a) {
  array<observer> obs= get_link_observers (a);
  ln->obs << obs;
  insert_link (obs, ln.rep);
  return ln;
}

link&
operator << (link& ln, tree t) {
  tree ts[1]; ts[0]= t;
  return ln << array<tree> (ts, 1);
}

static list<link>
strong (list<weak_link> l) {
  if (nil (l)) return list<link> ();
  else return list<link> (link (l->item), strong (l->next));
}

list<link>
get_links (tree t) {
  observer obs;
  if (!nil (t->obs)) obs= t->obs->get_link_observer ();
  if (nil (obs)) return list<link> ();
  list<weak_link> wlns;
  obs->get_links (wlns);
  return strong (wlns);
}
