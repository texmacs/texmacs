
/******************************************************************************
* MODULE     : hard_link.cpp
* DESCRIPTION: Persistent hard_links between trees
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "path.hpp"
#include "link.hpp"
#include "iterator.hpp"
#include "vars.hpp"

hashmap<string,list<observer> > id_resolve;
hashmap<observer,list<string> > pointer_resolve;
hashmap<tree,list<soft_link> > vertex_occurrences;
hashmap<string,int> type_count (0);

static string current_locus_on_paper= "preserve";
static string current_locus_color= "#404080";
static string current_visited_color= "#702070";

static hashset<string> visited_table;

extern tree the_et;

/******************************************************************************
* Soft links
******************************************************************************/

void
register_pointer (string id, observer which) {
  // cout << "Register: " << id << " -> " << which << "\n";
  // cout << "Register: " << id << " -> " << obtain_tree (which) << "\n";
  list<observer>& l1= id_resolve (id);
  l1= list<observer> (which, l1);
  list<string>& l2= pointer_resolve (which);
  l2= list<string> (id, l2);
}

void
unregister_pointer (string id, observer which) {
  // cout << "Unregister: " << id << " -> " << which << "\n";
  // cout << "Unregister: " << id << " -> " << obtain_tree (which) << "\n";
  list<observer>& l1= id_resolve (id);
  l1= remove (l1, which);
  if (is_nil (l1)) id_resolve->reset (id);
  list<string>& l2= pointer_resolve (which);
  l2= remove (l2, id);
  if (is_nil (l2)) pointer_resolve->reset (which);
}

void
register_vertex (tree v, soft_link ln) {
  list<soft_link>& l= vertex_occurrences (v);
  l= list<soft_link> (ln, l);
}

void
unregister_vertex (tree v, soft_link ln) {
  list<soft_link>& l= vertex_occurrences (v);
  l= remove (l, ln);
  if (is_nil (l)) vertex_occurrences->reset (v);
}

void
register_link (soft_link ln) {
  // cout << "Register: " << ln->t << "\n";
  int i, n= N(ln->t);
  if (is_atomic (ln->t[0]))
    type_count (ln->t[0]->label) ++;
  for (i=1; i<n; i++)
    register_vertex (ln->t[i], ln);
}

void
unregister_link (soft_link ln) {
  // cout << "Unregister: " << ln->t << "\n";
  int i, n= N(ln->t);
  if (is_atomic (ln->t[0])) {
    type_count (ln->t[0]->label) --;
    if (type_count (ln->t[0]->label) == 0)
      type_count->reset (ln->t[0]->label);
  }
  for (i=1; i<n; i++)
    unregister_vertex (ln->t[i], ln);
}

/******************************************************************************
* Link repositories
******************************************************************************/

link_repository_rep::link_repository_rep () {}

link_repository_rep::~link_repository_rep () {
  while (!is_nil (loci)) {
    tree t= obtain_tree (loci->item);
    unregister_pointer (ids->item, loci->item);
    detach_observer (t, loci->item);
    ids= ids->next;
    loci= loci->next;
  }
  while (!is_nil (links)) {
    unregister_link (links->item);
    links= links->next;
  }
}

void
link_repository_rep::insert_locus (string id, tree t) {
  observer obs= tree_pointer (t);
  register_pointer (id, obs);
  attach_observer (t, obs);
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
  if (is_nil (l)) return list<string> ();
  return pointer_resolve [l->item] * get_ids (l->next);
}

list<string>
get_ids (tree t) {
  if (is_nil (t->obs)) return list<string> ();
  list<observer> l= t->obs->get_tree_pointers ();
  return reverse (get_ids (l));
}

list<tree>
as_trees (list<observer> l) {
  if (is_nil (l)) return list<tree> ();
  else return list<tree> (obtain_tree (l->item), as_trees (l->next));
}

list<tree>
get_trees (string id) {
  return reverse (as_trees (id_resolve [id]));
}

list<tree>
as_tree_list (list<soft_link> l) {
  if (is_nil (l)) return list<tree> ();
  else return list<tree> (l->item->t, as_tree_list (l->next));
}

list<tree>
get_links (tree v) {
  return reverse (as_tree_list (vertex_occurrences [v]));
}

list<string>
all_link_types () {
  list<string> l;
  iterator<string> it= iterate (type_count);
  while (it->busy()) {
    string s= it->next();
    l= list<string> (s, l);
  }
  return l;
}

/******************************************************************************
* Locus rendering
******************************************************************************/

void
set_locus_rendering (string var, string val) {
  if (var == "locus-on-paper") current_locus_on_paper= val;
  if (var == LOCUS_COLOR) current_locus_color= val;
  if (var == VISITED_COLOR) current_visited_color= val;
}

string
get_locus_rendering (string var) {
  if (var == "locus-on-paper") return current_locus_on_paper;
  if (var == LOCUS_COLOR) return current_locus_color;
  if (var == VISITED_COLOR) return current_visited_color;
  return "";
}

void
declare_visited (string id) {
  visited_table->insert (id);
}

bool
has_been_visited (string id) {
  return visited_table->contains (id);
}

/******************************************************************************
* Checks
******************************************************************************/

bool
has_subtree (tree t, path p) {
  if (is_nil (p)) return true;
  if (is_atomic (t)) return false;
  if ((0 > p -> item) || (p -> item >= N(t))) return false;
  return has_subtree (t[p->item], p->next);
}

bool
can_assign (tree t, path p, tree u) {
  (void) u;
  return has_subtree (t, p);
}

bool
can_insert (tree t, path p, tree u) {
  if (!has_subtree (t, path_up (p))) return false;
  tree st= subtree (t, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) return l >= 0 && l <= N(st->label) && is_atomic (u);
  else return l >= 0 && l >= N(st) && is_compound (u);
}

bool
can_remove (tree t, path p, int nr) {
  if (!has_subtree (t, path_up (p))) return false;
  tree st= subtree (t, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) return l >= 0 && l+nr <= N(st->label);
  else return l >= 0 && l+nr >= N(st);
}

bool
can_split (tree t, path p) {
  if (!has_subtree (t, path_up (p))) return false;
  tree st= subtree (t, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) return l >= 0 && l <= N(st->label);
  else return l >= 0 && l >= N(st);
}

bool
can_join (tree t, path p) {
  if (!has_subtree (t, path_up (p))) return false;
  tree st= subtree (t, path_up (p));
  int l= last_item (p);
  if (l < 0 || l+1 >= N(st)) return false;
  if (is_atomic (st[l]) && is_atomic (st[l+1])) return true;
  if (is_compound (st[l]) && is_compound (st[l+1])) return true;
  return false;
}

bool
can_assign_node (tree t, path p, tree_label op) {
  return has_subtree (t, p) && is_compound (subtree (t, p));
}

bool
can_insert_node (tree t, path p, tree u) {
  return has_subtree (t, path_up (p)) && is_compound (u) &&
         last_item (p) >= 0 && last_item (p) <= N(u);
}

bool
can_remove_node (tree t, path p) {
  return has_subtree (t, p) && N(p) >= 1;
}

/******************************************************************************
* Link event handlers for tree changes
******************************************************************************/

list<string>
get_ids (observer obs) {
  return pointer_resolve [obs];
}

list<tree>
get_links (string id) {
  return get_links (compound ("id", id));
}

tree&
get_reference (tree& t) {
  path ip= obtain_ip (t);
  if (ip_attached (ip)) return subtree (the_et, reverse (ip));
  else return t;
}

list<tree>
not_done (list<tree> l) {
  if (is_nil (l)) return l;
  else if (is_modifying (get_reference (l->item))) return not_done (l->next);
  return list<tree> (l->item, not_done (l->next));
}

list<tree>
get_mirrors (tree ln, string id) {
  /*
  if (!is_compound (ln, "link", 4) ||
      ln[0] != "mirror" ||
      !is_compound (ln[2], "id", 1) ||
      !is_atomic (ln[2][0]) ||
      !is_compound (ln[3], "id", 1) ||
      !is_atomic (ln[3][0]))
    return list<tree> ();
  if (ln[2][0] == id) return not_done (get_trees (ln[3][0]->label));
  if (ln[3][0] == id) return not_done (get_trees (ln[2][0]->label));
  */
  (void) ln; (void) id;
  return list<tree> ();
}

void
link_assign (tree ln, string id, path p, tree t) {
  //cout << "Link assign " << ln << ", " << id
  //     << ", " << p << ", " << t << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_assign (l->item, p, t))
      assign (subtree (get_reference (l->item), p), copy (t));
}

void
link_insert (tree ln, string id, path p, tree ins) {
  //cout << "Link insert " << ln << ", " << id
  //     << ", " << p << ", " << ins << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_insert (l->item, p, ins))
      insert (subtree (get_reference (l->item), path_up (p)),
	      last_item (p), copy (ins));
}

void
link_remove (tree ln, string id, path p, int nr) {
  //cout << "Link remove " << ln << ", " << id
  //     << ", " << p << ", " << nr << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_remove (l->item, p, nr))
      remove (subtree (get_reference (l->item), path_up (p)),
	      last_item (p), nr);
}

void
link_split (tree ln, string id, path p) {
  //cout << "Link split " << ln << ", " << id
  //     << ", " << p << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_split (l->item, p))
      split (subtree (get_reference (l->item), path_up (path_up (p))),
	     last_item (path_up (p)), last_item (p));
}

void
link_join (tree ln, string id, path p) {
  //cout << "Link join " << ln << ", " << id
  //     << ", " << p << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_split (l->item, p))
      join (subtree (get_reference (l->item), path_up (p)), last_item (p));
}

void
link_assign_node (tree ln, string id, path p, tree_label op) {
  //cout << "Link assign node " << ln << ", " << id
  //     << ", " << p << ", " << tree (op) << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_assign_node (l->item, p, op))
      assign_node (subtree (get_reference (l->item), p), op);
}

void
link_insert_node (tree ln, string id, path p, tree ins) {
  //cout << "Link insert node " << ln << ", " << id
  //     << ", " << p << ", " << ins << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_insert_node (l->item, p, ins))
      insert_node (subtree (get_reference (l->item), path_up (p)),
		   last_item (p), ins);
}

void
link_remove_node (tree ln, string id, path p) {
  //cout << "Link remove node " << ln << ", " << id
  //     << ", " << p << "\n";
  for (list<tree> l= get_mirrors (ln, id); !is_nil (l); l= l->next)
    if (can_remove_node (l->item, p))
      remove_node (subtree (get_reference (l->item), path_up (p)),
		   last_item (p));
}
