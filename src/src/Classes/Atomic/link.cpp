
/******************************************************************************
* MODULE     : hard_link.cpp
* DESCRIPTION: Persistent hard_links between trees
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "link.hpp"

/******************************************************************************
* Routines on hard_links
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
* Hard_Link types
******************************************************************************/

array<tree>
A (hard_link ln) {
  int i, n= N(ln);
  array<tree> a (n);
  for (i=0; i<n; i++)
    a[i]= ln[i];
  return a;
}

hard_link&
operator << (hard_link& ln, array<tree> a) {
  array<observer> obs= get_link_observers (a);
  ln->obs << obs;
  insert_link (obs, ln.rep);
  return ln;
}

hard_link&
operator << (hard_link& ln, tree t) {
  tree ts[1]; ts[0]= t;
  return ln << array<tree> (ts, 1);
}

static list<hard_link>
strong (list<weak_link> l) {
  if (nil (l)) return list<hard_link> ();
  else return list<hard_link> (hard_link (l->item), strong (l->next));
}

list<hard_link>
get_links (tree t) {
  observer obs;
  if (!nil (t->obs)) obs= t->obs->get_link_observer ();
  if (nil (obs)) return list<hard_link> ();
  list<weak_link> wlns;
  obs->get_links (wlns);
  return strong (wlns);
}

/******************************************************************************
* Soft links
******************************************************************************/

hashmap<string,list<pointer> > id_resolve;
hashmap<pointer,list<string> > tree_resolve;
hashmap<string,list<soft_link> > id_occurrences;

void
register_tree (string id, tree_rep* which) {
  cout << "Register: " << id << " -> " << which << "\n";
  list<pointer>& l1= id_resolve (id);
  l1= list<pointer> ((pointer) which, l1);
  list<string>& l2= tree_resolve ((pointer) which);
  l2= list<string> (id, l2);
}

void
unregister_tree (string id, tree_rep* which) {
  cout << "Unregister: " << id << " -> " << which << "\n";
  list<pointer>& l1= id_resolve (id);
  l1= remove (l1, (pointer) which);
  if (nil (l1)) id_resolve->reset (id);
  list<string>& l2= tree_resolve ((pointer) which);
  l2= remove (l2, id);
  if (nil (l2)) tree_resolve->reset ((pointer) which);
}

void
register_link_component (string id, soft_link ln) {
  if (id == "") return;
  list<soft_link>& l= id_occurrences (id);
  l= list<soft_link> (ln, l);
}

void
unregister_link_component (string id, soft_link ln) {
  if (id == "") return;
  list<soft_link>& l= id_occurrences (id);
  l= remove (l, ln);
  if (nil (l)) id_occurrences->reset (id);
}

void
register_link (soft_link ln) {
  cout << "Register: " << ln->t << "\n";
  int i, n= N(ln->t);
  for (i=1; i<n; i++)
    register_link_component (as_string (ln->t[i]), ln);
}

void
unregister_link (soft_link ln) {
  cout << "Unregister: " << ln->t << "\n";
  int i, n= N(ln->t);
  for (i=1; i<n; i++)
    unregister_link_component (as_string (ln->t[i]), ln);
}

/******************************************************************************
* Link repositories
******************************************************************************/

link_repository_rep::link_repository_rep () {}

link_repository_rep::~link_repository_rep () {
  while (!nil (loci)) {
    unregister_tree (ids->item, (tree_rep*) loci->item);
    ids= ids->next;
    loci= loci->next;
  }
  while (!nil (links)) {
    unregister_link (links->item);
    links= links->next;
  }
}

void
link_repository_rep::insert_locus (string id, tree t) {
  tree_rep* rep= t.rep;
  register_tree (id, rep);
  ids= list<string> (id, ids);
  loci= list<pointer> ((pointer*) rep, loci);
}

void
link_repository_rep::insert_link (soft_link ln) {
  register_link (ln);
  links= list<soft_link> (ln, links);
}

/******************************************************************************
* Routines for navigation
******************************************************************************/

list<string>
get_ids (tree t) {
  tree_rep* rep= t.operator -> ();
  return reverse (tree_resolve [(pointer) rep]);
}

list<tree>
as_trees (list<pointer> l) {
  if (nil (l)) return list<tree> ();
  else return list<tree> (tree ((tree_rep*) l->item), as_trees (l->next));
}

list<tree>
get_trees (string id) {
  return reverse (as_trees (id_resolve [id]));
}

list<tree>
as_tree_list (list<soft_link> l) {
  if (nil (l)) return list<tree> ();
  else return list<tree> (l->item->t, as_tree_list (l->next));
}

list<tree>
get_links (string id) {
  return reverse (as_tree_list (id_occurrences [id]));
}
