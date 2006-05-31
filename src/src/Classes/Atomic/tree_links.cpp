
/******************************************************************************
* MODULE     : tree_links.cpp
* DESCRIPTION: Persistently links between trees
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "link.hpp"
#include "list.hpp"

static hashmap<string,pointer> unique_id_decode (NULL);

/******************************************************************************
* Definition of the tree_links_rep class
******************************************************************************/

static string empty_string;

class tree_links_rep: public observer_rep {
private:
  string id;
  tree_rep* ptr;
  list<weak_link> lns;

public:
  tree_links_rep (tree ref): id (empty_string), ptr (ref.rep) {}
  ostream& print (ostream& out) { return out << " links"; }

  void notify_assign      (tree& ref, tree t);
  void notify_insert      (tree& ref, int pos, int nr);
  void notify_remove      (tree& ref, int pos, int nr);
  void notify_split       (tree& ref, int pos, tree prev);
  void notify_var_split   (tree& ref, tree t1, tree t2);
  void notify_join        (tree& ref, int pos, tree next);
  void notify_var_join    (tree& ref, tree t, int offset);
  void notify_insert_node (tree& ref, int pos);
  void notify_remove_node (tree& ref, int pos);
  void notify_assign_node (tree& ref, tree_label op);
  void notify_detach      (tree& ref, tree closest, bool right);

  observer get_link_observer ();
  bool set_tree (tree t);
  bool get_tree (tree& t);
  bool get_links (list<weak_link>& lns);
  bool insert_link (weak_link ln);
  bool remove_link (weak_link ln);
  bool get_unique_id (string& s);
  bool set_unique_id (string s);
};

/******************************************************************************
* Specific routines for tree link observers
******************************************************************************/

observer
tree_links_rep::get_link_observer () {
  return observer (this);
}

bool
tree_links_rep::get_tree (tree& t) {
  t= tree (ptr);
  return true;
}

bool
tree_links_rep::set_tree (tree t) {
  if (ptr != t.rep) {
    tree ref (ptr);
    remove_observer (ref->obs, observer (this));
    ptr= t.rep;
    insert_observer (t->obs, observer (this));
  }
  return true;
}

bool
tree_links_rep::get_links (list<weak_link>& lns2) {
  lns2= lns;
  return true;
}

bool
tree_links_rep::insert_link (weak_link ln) {
  cout << "Insert link " << ln << " to " << lns << "\n";
  lns= list<weak_link> (ln, lns);
  return true;
}

static list<weak_link>
remove (list<weak_link> lns, weak_link ln, bool& removed) {
  if (nil (lns)) return lns;
  else if (lns->item == ln) {
    removed= true;
    return remove (lns->next, ln, removed);
  }
  else return list<weak_link> (lns->item, remove (lns->next, ln, removed));
}

bool
tree_links_rep::remove_link (weak_link ln) {
  cout << "Remove link " << ln << " from " << lns << "\n";
  bool removed= false;
  lns= remove (lns, ln, removed);
  if (nil (lns) && id == empty_string)
    remove_observer (ptr->obs, observer (this));
  return removed;
}

bool
tree_links_rep::get_unique_id (string& s) {
  s= id;
  return true;
}

static void
substitute (list<weak_link> lns, observer obs1, observer obs2) {
  while (!nil (lns)) {
    int i, n= N(lns->item->obs);
    for (i=0; i<n; i++)
      if (lns->item->obs[i] == obs1)
	lns->item->obs[i] == obs2;
    lns= lns->next;
  }
}

bool
tree_links_rep::set_unique_id (string s) {
  if (id != "") unique_id_decode->reset (id);
  if (s != "") {
    tree_links_rep* old= (tree_links_rep*) unique_id_decode [s];
    if (old != NULL) {
      cout << "Merge " << old << " -> " << this << "\n";
      cout << "  " << tree (old->ptr) << " -> " << tree (ptr) << "\n";
      substitute (old->lns, observer (old), observer (this));
      lns= old->lns * lns;
      old->lns= list<weak_link> ();
      unique_id_decode (s)= (pointer) this;
      remove_observer (old->ptr->obs, observer (old));
    }
    else unique_id_decode (s)= (pointer) this;
  }
  id= s;
  return true;
}

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
tree_links_rep::notify_assign (tree& ref, tree t) {
  // cout << "Notify assign " << ref << ", " << t << "\n";
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_insert (tree& ref, int pos, int nr) {
  // cout << "Notify insert " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_remove (tree& ref, int pos, int nr) {
  // cout << "Notify remove " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref; (void) pos; (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref; (void) pos; (void) prev;
}

void
tree_links_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  // cout << "Notify var split " << ref << ", " << t1 << ", " << t2 << "\n";
  (void) t2;
  (void) set_tree (t1); // always at the left
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref; (void) pos; (void) next;
}

void
tree_links_rep::notify_var_join (tree& ref, tree t, int offset) {
  // cout << "Notify var join " << ref << ", " << t << ", " << offset << "\n";
  (void) ref; (void) offset;
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_insert_node (tree& ref, int pos) {
  // cout << "Notify insert node " << ref << ", " << pos << "\n";
  (void) ref; (void) pos;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_remove_node (tree& ref, int pos) {
  // cout << "Notify remove node " << ref << ", " << pos << "\n";
  (void) set_tree (ref[pos]);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_assign_node (tree& ref, tree_label op) {
  // cout << "Notify assign node " << ref << ", " << as_string (op) << "\n";
  (void) ref; (void) op;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_links_rep::notify_detach (tree& ref, tree closest, bool right) {
  // cout << "Notify detach " << ref << ", " << closest <<", "<< right << "\n";
  (void) right;
  (void) set_tree (closest);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

/******************************************************************************
* Public interface
******************************************************************************/

observer
tree_links (tree ref) {
  return new tree_links_rep (ref);
}

observer
get_link_observer (tree t) {
  cout << "Get link observer " << t << "\n";
  observer tl;
  if (!nil (t->obs)) tl= t->obs->get_link_observer ();
  cout << "-> " << tl << "\n";
  if (nil (tl)) {
    tl= tree_links (t);
    insert_observer (t->obs, tl);
  }
  cout << "-> " << tl << "\n";
  return tl;
}

array<observer>
get_link_observers (array<tree> a) {
  int i, n= N(a);
  array<observer> obs (n);
  for (i=0; i<n; i++)
    obs[i]= get_link_observer (a[i]);
  return obs;
}

list<weak_link>
get_links (observer o) {
  list<weak_link> lns;
  (void) o->get_links (lns);
  return lns;
}

void
insert_link (array<observer> obs, weak_link ln) {
  int i, n= N(obs);
  for (i=0; i<n; i++)
    (void) obs[i] -> insert_link (ln);
}

void
remove_link (array<observer> obs, weak_link ln) {
  int i, n= N(obs);
  for (i=0; i<n; i++)
    (void) obs[i] -> remove_link (ln);
}

string
get_unique_id (tree& ref) {
  observer tl;
  if (!nil (ref->obs)) tl= ref->obs->get_link_observer ();
  if (!nil (tl)) {
    string s;
    if (tl->get_unique_id (s)) return s;
  }
  return "";
}

void
set_unique_id (tree& ref, string s) {
  observer tl= get_link_observer (ref);
  (void) tl->set_unique_id (s);
}

bool
unique_id_exists (string id) {
  return unique_id_decode -> contains (id);
}

tree
unique_id_to_tree (string id) {
  tree t;
  if (unique_id_decode -> contains (id)) {
    tree_links_rep* rep= (tree_links_rep*) unique_id_decode [id];
    (void) rep->get_tree (t);
  }
  return t;
}
