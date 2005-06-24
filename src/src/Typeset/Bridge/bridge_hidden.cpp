
/******************************************************************************
* MODULE     : bridge_hidden.cpp
* DESCRIPTION: Bridge between logical and physical local enviroment changes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"

class bridge_hidden_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_hidden_rep (typesetter ttt, tree st, path ip);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_hidden_rep::bridge_hidden_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  initialize ();
}

void
bridge_hidden_rep::initialize () {
  if (nil(body)) body= make_bridge (ttt, st[0], descend (ip, 0));
  else replace_bridge (body, st[0], descend (ip, 0));
}

bridge
bridge_hidden (typesetter ttt, tree st, path ip) {
  return new bridge_hidden_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_hidden_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  if (nil (p) && (!is_func (u, HIDDEN)))
    fatal_error ("Nil path", "bridge_hidden_rep::notify_assign");
  if (nil (p)) {
    st= u;
    initialize ();
  }
  else {
    bool mp_flag= is_multi_paragraph (st);
    if (p->item != 0)
      fatal_error ("Invalid path", "bridge_hidden_rep::notify_assign");
    if (atom (p)) body= make_bridge (ttt, u, descend (ip, 0));
    else body->notify_assign (p->next, u);
    st= substitute (st, p->item, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_hidden_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_hidden_rep::notify_insert");
  if (atom (p) || (p->item != 0)) bridge_rep::notify_insert (p, u);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, 0, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_hidden_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_hidden_rep::notify_remove");
  if (atom (p) || (p->item != 0)) bridge_rep::notify_remove (p, nr);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, 0, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_hidden_rep::notify_macro (int type, string var, int l, path p, tree u) {
  bool flag= body->notify_macro (type, var, l, p, u);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_hidden_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_hidden_rep::my_exec_until (path p) {
  if (p->item != 0) return;
  body->exec_until (p->next);
}

bool
bridge_hidden_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_hidden_rep::my_typeset (int desired_status) {
  body->typeset (desired_status);
  ttt->l = array<page_item> (0);
  ttt->sb= stack_border ();
}
