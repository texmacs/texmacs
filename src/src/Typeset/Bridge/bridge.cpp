
/******************************************************************************
* MODULE     : bridge.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"
#include "Boxes/construct.hpp"

bridge bridge_document (typesetter, tree, path);
bridge bridge_surround (typesetter, tree, path);
bridge bridge_formatting (typesetter, tree, path, string);
bridge bridge_with (typesetter, tree, path);
bridge bridge_expand (typesetter, tree, path);
bridge bridge_apply (typesetter, tree, path);
bridge bridge_include (typesetter, tree, path);
bridge bridge_argument (typesetter, tree, path);
bridge bridge_default (typesetter, tree, path);
bridge bridge_extension (typesetter, tree, path);
bridge bridge_executable (typesetter, tree, path);

bridge nil_bridge;

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

bridge_rep::bridge_rep (typesetter ttt2, tree st2, path ip2):
  ttt (ttt2), env (ttt->env), st (st2), ip (ip2),
  status (CORRUPTED), changes (UNINIT) {}

bridge
make_bridge (typesetter ttt, tree st, path ip) {
  switch (L(st)) {
  case DOCUMENT:
    return bridge_document (ttt, st, ip);
  case SURROUND:
    return bridge_surround (ttt, st, ip);
  case DECORATE_ATOMS:
    return bridge_formatting (ttt, st, ip, ATOM_DECORATIONS);
  case DECORATE_LINES:
    return bridge_formatting (ttt, st, ip, LINE_DECORATIONS);
  case DECORATE_PAGES:
    return bridge_formatting (ttt, st, ip, PAGE_DECORATIONS);
  case TABLE_FORMAT:
    return bridge_formatting (ttt, st, ip, CELL_FORMAT);
  case WITH:
    return bridge_with (ttt, st, ip);
  case EXPAND:
  case VAR_EXPAND:
  case HIDE_EXPAND:
    return bridge_expand (ttt, st, ip);
  case APPLY:
    return bridge_apply (ttt, st, ip);
  case INCLUDE:
    return bridge_include (ttt, st, ip);
  case ARGUMENT:
    return bridge_argument (ttt, st, ip);
  case EXTERN:
    return bridge_executable (ttt, st, ip);
  default:
    if (L(st) < START_EXTENSIONS) return bridge_default (ttt, st, ip);
    else return bridge_extension (ttt, st, ip);
  }
}

void
replace_bridge (bridge& br, tree st, path ip) {
  bridge new_br= make_bridge (br->ttt, st, ip);
  new_br->changes= br->changes;
  br= new_br;
}

bool
bridge::operator == (bridge item2) {
  return rep == item2.rep;
}

bool
bridge::operator != (bridge item2) {
  return rep != item2.rep;
}

ostream&
operator << (ostream& out, bridge br) {
  return out << "bridge [" << br->st << ", " << br->ip << "]";
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) {
    if (is_compound (u))
      fatal_error ("two atoms expected", "bridge_rep::notify_insert");
    t= insert (t->label, l, u->label);
  }
  else t= insert (t, l, u);
  notify_assign (q, t);
}

void
bridge_rep::notify_remove (path p, int nr) {
  // cout << "Insert " << p << ", " << nr << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) t= remove (t->label, l, nr);
  else t= remove (t, l, nr);
  notify_assign (q, t);
}

void
bridge_rep::notify_split (path p) {
  // cout << "Split " << p << " in " << st << "\n";
  path q  = path_up (p, 2);
  int  pos= last_item (path_up (p));
  int  l  = last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos])) {
    string s1, s2;
    split (t[pos]->label, l, s1, s2);
    notify_insert (q * pos, tree (L(t), s1));
    notify_assign (q * (pos+1), s2);
  }
  else {
    tree t1, t2;
    split (t[pos], l, t1, t2);
    notify_insert (q * pos, tree (L(t), t1));
    notify_assign (q * (pos+1), t2);
  }
}

void
bridge_rep::notify_join (path p) {
  // cout << "Join " << p << " in " << st << "\n";
  path q  = path_up (p);
  int  pos= last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos]) && is_atomic (t[pos+1])) {
    string j= t[pos]->label * t[pos+1]->label;
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
  else {
    tree j= join (t[pos], t[pos+1]);
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
}

/******************************************************************************
* Getting environment variables and typesetting
******************************************************************************/

void
bridge_rep::my_exec_until (path p) {
  env->exec_until (st, p);
}

bool
bridge_rep::my_typeset_will_be_complete () {
  return (status & VALID_MASK) == CORRUPTED;
}

void
bridge_rep::my_typeset (int desired_status) {
  if ((desired_status & WANTED_MASK) == WANTED_PARAGRAPH)
    ttt->insert_paragraph (st, ip);
  if ((desired_status & WANTED_MASK) == WANTED_PARUNIT)
    ttt->insert_parunit (st, ip);
}

void
bridge_rep::exec_until (path p) {
  if ((status & VALID_MASK) != PROCESSED) env->exec_until (st, p);
  else if (p == path (1)) env->patch_env (changes);
  else if (p != path (0)) my_exec_until (p);
}

void
bridge_rep::typeset (int desired_status) {
  // cout << "Typesetting " << st << ", " << desired_status << "\n";
  if ((status==desired_status) && (N(ttt->old_patch)==0)) {
    // cout << "  cached\n";
    env->monitored_patch_env (changes);
    // cout << "  changes       = " << changes << "\n";
  }
  else {
    // cout << "Typesetting " << st << ", " << desired_status << "\n";
    // cout << "  recomputing\n";
    hashmap<string,tree> prev_back (UNINIT);
    ttt->local_start (l, sb);
    env->local_start (prev_back);
    my_typeset (desired_status);
    env->local_update (ttt->old_patch, changes);
    env->local_end (prev_back);
    ttt->local_end (l, sb);
    status= desired_status;
    // cout << "  old_patch     = " << ttt->old_patch << "\n";
    // cout << "  changes       = " << changes << "\n";
    // cout << "Typesetted " << st << ", " << desired_status << "\n";
  }
  // cout << "Typesetted " << st << ", " << desired_status << "\n";

  // ttt->insert_stack (l, sb);
  if (ttt->paper || (N(l) <= 1)) ttt->insert_stack (l, sb);
  else {
    bool flag= false;
    int i, n= N(l);
    for (i=0; i<n; i++)
      flag= flag || (N (l[i]->fl) != 0) || (l[i]->nr_cols > 1);
    if (flag) ttt->insert_stack (l, sb);
    else {
      int last=-1;
      array<box> bs;
      array<SI>  spc;
      for (i=0; i<n; i++)
	if (l[i]->type == PAGE_LINE_ITEM) {
	  bs  << l[i]->b;
	  spc << l[i]->spc->def;
	  last= i;
	}
      box lb= stack_box (path (ip), bs, spc);
      lb= move_box (path (ip), lb, 0, bs[0]->y2);
      array<page_item> new_l (1);
      new_l[0]= page_item (lb);
      new_l[0]->spc= l[last]->spc;
      ttt->insert_stack (new_l, sb);
    }
  }

  // cout << "  l   = " << l << "\n";
  // cout << "  sb  = " << sb << "\n";
}
