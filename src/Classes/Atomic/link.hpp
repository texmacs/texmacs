
/******************************************************************************
* MODULE     : link.cpp
* DESCRIPTION: Linking of trees
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
* The soft_link class
******************************************************************************/

class soft_link;
class soft_link_rep: public concrete_struct {
public:
  tree t;

public:
  inline soft_link_rep (tree t2): t (t2) {}
  inline ~soft_link_rep () {}

  friend class soft_link;
};

class soft_link {
public:
CONCRETE(soft_link);
public:
  inline soft_link (tree t):
    rep (new soft_link_rep (t)) {}
  inline friend bool operator == (soft_link ln1, soft_link ln2) {
    return ln1.rep == ln2.rep; }
  inline friend bool operator != (soft_link ln1, soft_link ln2) {
    return ln1.rep != ln2.rep; }
  inline friend ostream& operator << (ostream& out, soft_link ln) {
    return out << "soft_link (" << ln.rep << ")"; }
};
CONCRETE_CODE(soft_link);

/******************************************************************************
* Link repositories
******************************************************************************/

class link_repository_rep: public abstract_struct {
public:
  list<string> ids;
  list<observer> loci;
  list<soft_link> links;
  link_repository_rep ();
  ~link_repository_rep ();
  void insert_locus (string id, tree t);
  void insert_link (soft_link ln);
};

class link_repository {
ABSTRACT_NULL(link_repository);
public:
  inline link_repository (bool active):
    rep (new link_repository_rep ()) { rep->ref_count++; }
};
ABSTRACT_NULL_CODE(link_repository);

/******************************************************************************
* Routines for navigation
******************************************************************************/

list<string> get_ids (tree t);
list<tree> get_trees (string id);
list<tree> get_links (string id);

#endif // HARD_LINK_H
