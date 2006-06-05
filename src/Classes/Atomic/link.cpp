
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
* Soft links
******************************************************************************/

hashmap<string,list<observer> > id_resolve;
hashmap<observer,list<string> > pointer_resolve;
hashmap<string,list<soft_link> > id_occurrences;

void
register_pointer (string id, observer which) {
  //cout << "Register: " << id << " -> " << which << "\n";
  list<observer>& l1= id_resolve (id);
  l1= list<observer> (which, l1);
  list<string>& l2= pointer_resolve (which);
  l2= list<string> (id, l2);
}

void
unregister_pointer (string id, observer which) {
  //cout << "Unregister: " << id << " -> " << which << "\n";
  list<observer>& l1= id_resolve (id);
  l1= remove (l1, which);
  if (nil (l1)) id_resolve->reset (id);
  list<string>& l2= pointer_resolve (which);
  l2= remove (l2, id);
  if (nil (l2)) pointer_resolve->reset (which);
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

static string
as_id (tree t) {
  if (is_atomic (t)) return t->label;
  if (is_func (t, ID, 1)) return as_id (t[0]);
  return "";
}

void
register_link (soft_link ln) {
  //cout << "Register: " << ln->t << "\n";
  int i, n= N(ln->t);
  for (i=1; i<n; i++)
    register_link_component (as_id (ln->t[i]), ln);
}

void
unregister_link (soft_link ln) {
  //cout << "Unregister: " << ln->t << "\n";
  int i, n= N(ln->t);
  for (i=1; i<n; i++)
    unregister_link_component (as_id (ln->t[i]), ln);
}

/******************************************************************************
* Link repositories
******************************************************************************/

link_repository_rep::link_repository_rep () {}

link_repository_rep::~link_repository_rep () {
  while (!nil (loci)) {
    tree t= obtain_tree (loci->item);
    unregister_pointer (ids->item, loci->item);
    detach_pointer (t, loci->item);
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
  observer obs= tree_pointer (t);
  register_pointer (id, obs);
  attach_pointer (t, obs);
  ids= list<string> (id, ids);
  loci= list<observer> (obs, loci);
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
get_ids (list<observer> l) {
  if (nil (l)) return list<string> ();
  return pointer_resolve [l->item] * get_ids (l->next);
}

list<string>
get_ids (tree t) {
  list<observer> l= t->obs->get_tree_pointers ();
  return reverse (get_ids (l));
}

list<tree>
as_trees (list<observer> l) {
  if (nil (l)) return list<tree> ();
  else return list<tree> (obtain_tree (l->item), as_trees (l->next));
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
