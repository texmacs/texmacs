
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

#ifndef LINK_H
#define LINK_H
#include "tree.hpp"
#include "list.hpp"
#include "hashmap.hpp"

/******************************************************************************
* Link types
******************************************************************************/

enum link_label {};

extern int nr_link_labels;
extern hashmap<int,string> link_label_to_name;
extern hashmap<string,int> name_to_link_label;

link_label as_link_label (string s);
string as_string (link_label lab);

/******************************************************************************
* The link class
******************************************************************************/

class link;
class link_rep: concrete_struct {
public:
  link_label      lab;
  array<observer> obs;

public:
  inline link_rep (link_label lab2, array<tree> a):
    lab (lab2), obs (get_link_observers (a)) { insert_link (obs, this); }
  inline ~link_rep () { remove_link (obs, this); }

  friend class link;
};

class link {
public:
ABSTRACT(link);
public:
  inline link (link_label lab, array<tree> a):
    rep (new link_rep (lab, a)) {}
  inline friend link_label L (link ln) {
    return ln.rep->lab; }
  inline friend int N (link ln) {
    return N (ln.rep->obs); }
  inline tree operator [] (int i) {
    tree t; (void) rep->obs[i]->get_tree (t); return t; }
  inline friend bool operator == (link ln1, link ln2) {
    return ln1.rep == ln2.rep; }
  inline friend bool operator != (link ln1, link ln2) {
    return ln1.rep != ln2.rep; }
  inline friend ostream& operator << (ostream& out, link ln) {
    return out << "link (" << ln.rep << ")"; }
};
ABSTRACT_CODE(link);

/******************************************************************************
* Further routines on links
******************************************************************************/

array<tree> A (link ln);
link& operator << (link& ln, tree t);
link& operator << (link& ln, array<tree> a);
list<link> get_links (tree t);

#endif // LINK_H
